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

save_experiment_output <- function(result_list, experiment_name) {
   # Create directory if it doesn't exist
   dir_path <- file.path("Outputs", "Experiments", experiment_name)
   if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
   }
   
   # Save results list as an RDS file
   saveRDS(result_list, file = file.path(dir_path, "model_results.rds"))
   
   cat(sprintf("Saved experiment '%s' results to %s\n", experiment_name, dir_path))
}


run_experiment <- function(experiment_name, base_config, overrides) {
   # Combine base config with overrides
   configs <- combine_config(base_config, overrides)
   
   
   for (i in seq_along(configs)) {
      cfg <- configs[[i]]
      # If multiple configs (grid), add suffix
      run_name <- if(length(configs) > 1) {
         paste0(experiment_name, "_run", i)
      } else {
         experiment_name
      }
      
      # Convert to python config dict via reticulate
      py_cfg <- r_to_py(cfg)
      
      # Run model (assuming run_copula_pot_model is loaded in py environment)
      result <- py$run_copula_pot_model(py_cfg)
      
      # Save results
      save_experiment_output(result, run_name)
   }
}
