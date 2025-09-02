combine_config <- function(base, overrides) {
   # If overrides contain vectors (multiple values), create all combinations
   # Otherwise just overwrite base with overrides
   
   # Check for vector parameters
   vector_params <- Filter(function(x) length(x) > 1, overrides)
   
   if (length(vector_params) == 0) {
      # Just overwrite keys in base with values in overrides
      config <- base
      for (name in names(overrides)) {
         config[[name]] <- overrides[[name]]
      }
      return(list(config))
   } else {
      # Create a grid of all combinations
      grid_df <- expand.grid(overrides, stringsAsFactors = FALSE)
      configs <- apply(grid_df, 1, function(row) {
         config <- base
         for (name in names(row)) {
            config[[name]] <- row[[name]]
         }
         config
      })
      return(configs)
   }
}

# --- Function to run one experiment ---
run_one_experiment <- function(experiment_type, experiment_name, base_config) {
   
   library(jsonlite)
   
   # Load Python inside worker so reticulate session is fresh
   library(reticulate)
   source_python('Scripts/Modeling/RollingWindowModel.py') 
   
   # Build output directory
   out_dir <- file.path(
      OUTPUT_PATH, experiment_type, experiment_name
   )
   dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
   
   # Build config locally
   config <- base_config
   
   # Adjust config depending on experiment type
   if (experiment_type == "DistributionScreening") {
      config$distribution_family <- experiment_name
      config$experiment_type <- experiment_type
   } else if (experiment_type == "WindowSizeScreening") {
      config$window_length <- as.numeric(experiment_name)
      config$experiment_type <- experiment_type
   }
   
   message("[", Sys.time(), "] Running ", experiment_type, ": ", experiment_name)
   
   result <- tryCatch({
      run_rolling_model(config)
   }, error = function(e) {
      message("Error running ", experiment_name, ": ", e$message)
      return(NULL)
   })
   
   if (!is.null(result)) {
      write_json(result, file.path(out_dir, "results.json"), pretty = TRUE, auto_unbox = TRUE)
      message("[", Sys.time(), "] Saved results for ", experiment_name)
   }
   
   return(result)
}

# ---- Function to load all results from a given experiment type ----
load_results <- function(experiment_type, base_path = "Outputs/Experiments/RollingWindowModeling") {
   
   library(jsonlite)
   
   # Build path to experiment type directory
   experiment_path <- file.path(base_path, experiment_type)
   
   # List all result.json files recursively
   json_files <- list.files(experiment_path, pattern = "results.json$", recursive = TRUE, full.names = TRUE)
   
   if (length(json_files) == 0) {
      stop("No results.json files found in ", experiment_path)
   }
   
   # Read each JSON file into a list
   results_list <- map(json_files, read_json, simplifyVector = TRUE)
   
   # Extract experiment name from folder (distribution name or window size)
   experiment_names <- basename(dirname(json_files))
   
   # Combine into a single dataframe
   results_df <- map2_dfr(results_list, experiment_names, function(res, exp_name) {
      tibble(
         experiment_name = exp_name,
         window_length = res$model_info$window_length,
         salinity_threshold = res$model_info$salinity_threshold,
         distribution_family = res$model_info$distribution_family,
         n_predictors = res$model_info$n_predictors,
         observed_exceedance_rate = res$model_info$observed_exceedance_rate,
         total_observations = res$model_info$total_observations,
         distribution_fits = res$summary$distribution_fits,
         covariance_periods = res$summary$covariance_periods,
         prediction_periods = res$summary$prediction_periods,
         raw_results = list(res)  # keep nested JSON for detailed analysis
      )
   })
   
   return(results_df)
}

unnest_results <- function(results_df, experiment_col = "experiment_name") {
   
   map_df(seq_len(nrow(results_df)), function(i) {
      res <- results_df$raw_results[[i]]
      
      roll_df <- res$rolling_distributions
      if (is.null(roll_df) || nrow(roll_df) == 0) return(NULL)
      
      roll_df <- as_tibble(roll_df)
      
      # Add experiment-level info
      roll_df[[experiment_col]] <- results_df[[experiment_col]][i]
      roll_df[["distribution_family"]] <- res$model_info$distribution_family
      roll_df[["window_length"]] <- res$model_info$window_length
      roll_df[["salinity_threshold"]] <- res$model_info$salinity_threshold
      
      # Keep only relevant columns
      roll_df <- roll_df %>% 
         select(timestamp, exceedance_probability, n_observations, everything(), 
                all_of(experiment_col), distribution_family, window_length, salinity_threshold)
      
      roll_df
   })
}
