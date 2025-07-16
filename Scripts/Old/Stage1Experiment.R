# Perform the first stage of GAM model fitting - screening of strategies


# Command line argument parsing
args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 1) {
   stop("Usage: Rscript 01_stage1_experiment.R <experiment_id> [ncores]")
}

experiment_id <- args[1]
ncores <- if(length(args) > 1) as.numeric(args[2]) else 20

# Source necessary functions
func_env <- new.env()
dirs <- c("Scripts/Functions/NonLinearModeling", "Scripts/Functions/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = func_env)
      })
   })
)

# Load necessary packages
library(dplyr)
library(purrr)
library(furrr)

# Setup parallel processing
plan(multisession, workers = ncores)

# Load experiment configuration
cat(sprintf("Loading experiment configuration for: %s\n", experiment_id))
exp_config <- qread(file.path("Experiment/NonLinearModeling", experiment_id, "config.qs"))

# Load best linear model as starting point
cat("Loading best linear model and data...\n")
linear_model_results <- func_env$read_qs_files('Outputs/LinearModeling/LinearModelResults.qs')

# Load engineered model data
base_data <- as.data.frame(func_env$read_qs_files('Data/Tidied/Final/FinalModelData.qs'))

# Filter data according to experiment configuration
cat(sprintf("Filtering data: %s\n", exp_config$data_filter))
if(exp_config$data_filter != "TRUE") {
   model_data <- base_data %>% filter(!!rlang::parse_expr(exp_config$data_filter))
} else {
   model_data <- base_data
}

cat(sprintf("Experiment data: %d observations\n", nrow(model_data)))

# Extract stage 1 configuration
stage1_config <- exp_config$stages$stage1

# Create combinations for stage 1
combinations <- expand.grid(
   strategy = stage1_config$strategies,
   weight_scheme = stage1_config$weights,
   distribution = stage1_config$distributions,
   stringsAsFactors = FALSE
)

cat(sprintf("Experiment: %s\n", experiment_id))
cat(sprintf("Model type: %s (quantile: %s)\n", 
            exp_config$model_type, 
            if(exp_config$use_qgam) exp_config$quantile else "N/A"))
cat(sprintf("AR structure: %s (order: %d)\n", 
            exp_config$ar_structure, exp_config$ar_order))
cat(sprintf("Time scope: %s\n", exp_config$time_scope))
cat(sprintf("Stage 1: Testing %d combinations\n", nrow(combinations)))

# Parallel model fitting with experiment-specific parameters
stage1_start <- Sys.time()

results <- combinations %>%
   split(seq(nrow(.))) %>%
   future_map(function(combo) {
      tryCatch({
         result <- fit_gam(
            data = data,
            linear_formula = linear_formula,
            linear_predictors = linear_predictors,
            strategy = combo$strategy,
            weight = combo$weight_scheme,
            distribution = combo$distribution,
            weight_schemes = combo$weight_schemes,
            distributions = combo$distributions,
            salinity_threshold = salinity_threshold,
            
            # Experiment-specific parameters
            stage_num = 1,
            use_ar = exp_config$use_ar,
            ar_order = exp_config$ar_order,
            use_qgam = exp_config$use_qgam,
            quantile = exp_config$quantile,
            time_var = exp_config$time_var,
            group_var = exp_config$group_var,
            strip = TRUE
         )
         
         # This needs to be checked
         if(!is.null(result)) {
            list(
               strategy = combo$strategy,
               weight_scheme = combo$weight_scheme,
               distribution = combo$distribution,
               score = result$score,
               model_id = paste(combo$strategy, combo$weight_scheme, 
                                combo$distribution, sep = "_"),
               rmse = result$overall_rmse,
               r2 = result$overall_r2,
               high_sal_r2 = result$high_salinity_r2,
               # Experiment metadata
               experiment_id = experiment_id,
               model_type = exp_config$model_type,
               ar_structure = exp_config$ar_structure,
               time_scope = exp_config$time_scope
            )
         }
      }, error = function(e) {
         cat(sprintf("Error in %s: %s\n", paste(combo, collapse="_"), e$message))
         return(NULL)
      })
   }, .options = furrr_options(seed = 42)) %>%  # Fixed seed for reproducibility
   compact()

stage1_time <- difftime(Sys.time(), stage1_start, units = "mins")

# Process and save results
cat("Processing Stage 1 results...\n")

stage1_performance <- map_dfr(results, as_tibble) %>%
   arrange(desc(score))

# Determine top strategies for Stage 2
best_score <- max(stage1_performance$score, na.rm = TRUE)
top_strategies <- stage1_performance %>%
   filter(score >= best_score * 0.9) %>%
   pull(strategy) %>%
   unique() %>%
   head(3)











