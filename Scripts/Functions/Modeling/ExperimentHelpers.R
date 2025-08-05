# Define Necessary Functions
# Helper to merge base config with overrides
merge_config <- function(base, override) {
   combined <- base
   for (name in names(override)) {
      combined[[name]] <- override[[name]]
   }
   combined
}

# Function to save results per experiment
save_experiment_results <- function(res, experiment_name, exp_i) {
   base_path <- file.path("Outputs", "Experiments", experiment_name)
   dir.create(base_path, recursive = TRUE, showWarnings = FALSE)
   
   exp_path <- file.path(base_path, paste0("exp_", exp_i))
   dir.create(exp_path, showWarnings = FALSE)
   
   # Save hybrid predictions if available
   if (!is.null(res$hybrid_predictions)) {
      write.csv(res$hybrid_predictions, file.path(exp_path, "hybrid_predictions.csv"), row.names = FALSE)
   }
   
   # Save metrics if available
   if (!is.null(res$metrics)) {
      saveRDS(res$metrics, file.path(exp_path, "metrics.rds"))
      # Also save as JSON (optional)
      jsonlite::write_json(res$metrics, file.path(exp_path, "metrics.json"), pretty = TRUE, auto_unbox = TRUE)
   }
   
   # Optionally save config for reproducibility
   config_path <- file.path(exp_path, "config.json")
   if (!is.null(res$config)) {
      jsonlite::write_json(res$config, config_path, pretty = TRUE, auto_unbox = TRUE)
   }
}

# Main function to run experiments for a given stage/grid
run_all_experiments <- function(grid, base_config, experiment_name) {
   results_list <- list()
   
   for (i in seq_len(nrow(grid))) {
      override <- grid[i, , drop = FALSE]
      config_i <- merge_config(base_config, as.list(override))
      
      # Save config in result for saving later
      # Optionally you can add timestamp or experiment ID here
      config_i$config_id <- paste0(experiment_name, "_exp_", i)
      
      py_config <- r_to_py(config_i, convert = TRUE)
      
      cat(sprintf("Running experiment %d/%d for stage '%s'...\n", i, nrow(grid), experiment_name))
      res <- try(py$run_model(py_config), silent = TRUE)
      
      if (inherits(res, "try-error")) {
         cat(sprintf("Experiment %d failed: %s\n", i, res))
         results_list[[i]] <- list(error = res)
      } else {
         # Attach R-side config for saving
         res$config <- config_i
         
         results_list[[i]] <- res
         
         # Save results to disk immediately
         save_experiment_results(res, experiment_name, i)
         
         cat(sprintf("Experiment %d completed and saved.\n", i))
      }
   }
   
   results_list
}





extract_metrics <- function(result) {
   metrics_list <- result$metrics
   dist_name <- class(result$tail_distribution)[[1]]
   
   flatten_metrics <- function(x, prefix = NULL) {
      if (is.list(x)) {
         map2_df(names(x), x, ~ flatten_metrics(.y, c(prefix, .x)))
      } else {
         tibble(
            Metric_Type = ifelse(length(prefix) > 0, prefix[1], NA_character_),
            Sub_Type = ifelse(length(prefix) > 2, prefix[2], NA_character_),
            Metric_Name = ifelse(length(prefix) > 1, prefix[length(prefix)], NA_character_),
            Value = x
         )
      }
   }
   
   flatten_metrics(metrics_list) %>%
      mutate(Distribution = dist_name) %>%
      select(Distribution, Metric_Type, Sub_Type, Metric_Name, Value)
}

bind_all_hybrid_predictions <- function(df, stage1_results) {
   all_preds <- df
   
   for (res in stage1_results) {
      preds_df <- res$hybrid_predictions
      
      # Extract distribution name from class
      dist_name <- tolower(gsub(".*\\.", "", class(res$tail_distribution)[[1]]))
      
      # Rename prediction columns with distribution suffix
      pred_cols <- setdiff(names(preds_df), "DateTime")
      renamed_preds <- preds_df %>%
         rename_with(~ paste0(., "_", dist_name), all_of(pred_cols))
      
      # Join with main df by DateTime
      all_preds <- left_join(all_preds, renamed_preds, by = "DateTime")
   }
   
   return(all_preds)
}
