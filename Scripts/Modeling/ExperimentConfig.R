# Define all experimental variations
create_experiment_matrix <- function() {
   
   # Define the variations of experiments
   base_experiments <- expand.grid(
      model_type = c("gam", "qgam"),
      ar_structure = c("none", "ar1", "ar2"),       # Different order autoregressive terms
      time_scope = c("single_year", "all_years"),
      year_focus = c(2016, NA),                     # 2016 for single year, NA for all years
      stringsAsFactors = FALSE
   ) %>%
      # Remove invalid combinations
      filter(!(time_scope == "all_years" & !is.na(year_focus))) %>%
      filter(!(time_scope == "single_year" & is.na(year_focus)))
   
   # Add experiment IDs and metadata
   experiments <- base_experiments %>%
      mutate(
         experiment_id = paste(
            model_type,
            ar_structure, 
            time_scope,
            if_else(time_scope == "single_year", as.character(year_focus), "allyears"),
            sep = "_"
         ),
         
         # Define computational requirements
         expected_runtime_hours = case_when(
            time_scope == "single_year" & model_type == "gam" ~ 2,
            time_scope == "single_year" & model_type == "qgam" ~ 4,
            time_scope == "all_years" & model_type == "gam" ~ 8,
            time_scope == "all_years" & model_type == "qgam" ~ 16
         ),
         memory_gb = case_when(
            time_scope == "single_year" ~ 16,
            time_scope == "all_years" ~ 32
         ),
         cores_needed = case_when(
            model_type == "qgam" ~ 20,
            TRUE ~ 15
         ),
         
         # Define data subsets needed
         data_filter = case_when(
            time_scope == "single_year" ~ sprintf("Year == %d", year_focus),
            TRUE ~ "TRUE"  # All data
         ),
         
         # Quantile settings
         quantile = if_else(model_type == "qgam", 0.75, NA_real_),  # Focus on upper tail
         # AR settings  
         ar_order = case_when(
            ar_structure == "ar1" ~ 1L,
            ar_structure == "ar2" ~ 2L,
            TRUE ~ 0L
         ),
         use_ar = ar_structure != "none"
      )
   
   return(experiments)
}


# Create specific experiment configurations
create_experiment_config <- function(experiment_row) {
   
   config <- list(
      # Experiment metadata
      experiment_id = experiment_row$experiment_id,
      model_type = experiment_row$model_type,
      ar_structure = experiment_row$ar_structure,
      time_scope = experiment_row$time_scope,
      year_focus = experiment_row$year_focus,
      
      # Model parameters
      use_qgam = experiment_row$model_type == "qgam",
      quantile = experiment_row$quantile,
      use_ar = experiment_row$use_ar,
      ar_order = experiment_row$ar_order,
      
      # Data parameters
      data_filter = experiment_row$data_filter,
      
      # Computational parameters
      cores_needed = experiment_row$cores_needed,
      memory_gb = experiment_row$memory_gb,
      expected_runtime_hours = experiment_row$expected_runtime_hours,
      
      # Stage configurations (same structure as before but experiment-specific)
      stages = list(
         stage1 = list(
            name = "Strategy Screening",
            strategies = c("baseline", "smooth_all", "smooth_flow", "smooth_stress", 
                           "smooth_tide", "tensor_flow_stress", "mixed_interactions"),
            weights = c("quantile"),
            distributions = if(experiment_row$model_type == "qgam") c("gaussian") else c("gaussian")
         ),
         stage2 = list(
            name = "Distribution Testing", 
            strategies = NULL,  # Will be populated from stage1
            weights = c("quantile"),
            distributions = if(experiment_row$model_type == "qgam") c("gaussian") else c("gaussian", "gamma")
         ),
         stage3 = list(
            name = "Weight Optimization",
            strategies = NULL,  # Will be populated from stage2
            weights = c("quantile", "ar_event_sequence", "ar_gradient", "ar_buildup"),
            distributions = NULL  # Will be populated from stage2
         )
      ),
      
      # Output paths
      output_dir = file.path("Experiments/NonLinearModeling", experiment_row$experiment_id),
      results_dir = file.path("Experiments/NonLinearModeling", experiment_row$experiment_id, "results"),
      logs_dir = file.path("Experiments/NonLinearModeling", experiment_row$experiment_id, "logs")
   )
   
   return(config)
}


# Generate all experiment configurations
setup_all_experiments <- function() {
   experiments <- create_experiment_matrix()
   
   # Create directories for each experiment
   for(i in 1:nrow(experiments)) {
      exp_config <- create_experiment_config(experiments[i, ])
      
      # Create directory structure
      dir.create(exp_config$output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(exp_config$results_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(exp_config$logs_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Save experiment configuration
      qs::qsave(exp_config, file.path(exp_config$output_dir, "config.qs"))
   }
   
   # Save master experiment matrix
   qs::qsave(experiments, "Experiments/NonLinearModeling/ExperimentMatrix.qs")
   
   cat(sprintf("Set up %d experiments:\n", nrow(experiments)))
   print(experiments[, c("experiment_id", "model_type", "ar_structure", "time_scope")])
   
   return(experiments)
}
