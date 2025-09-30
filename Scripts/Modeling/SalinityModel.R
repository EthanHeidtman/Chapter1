library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# =============================================================================
# Run Logistic Regression
# =============================================================================
run_LR <- function(data, base_formula = 'actual_exceedance ~', other_preds, weights) {
   formula <- paste0(base_formula, other_preds)
   regression <- glm(formula, data, family = binomial(link = 'logit'), na.action = na.exclude, weights = weight)
}

# =============================================================================
# Run Generalized Added Model 
# =============================================================================
run_GAM <- function(data, base_formula = 'actual_exceedance ~', other_preds, weights) {
   library(mgcv)
   
   # Parse the predictor string
   vars <- trimws(unlist(strsplit(other_preds, "\\+")))
   
   # First two get smooths
   smooth_terms <- paste0("s(", vars[1:2], ")", collapse = " + ")
   
   # Interaction between the first two
   interaction_term <- paste0("ti(", vars[1], ", ", vars[2], ")")
   
   # Last two stay linear
   linear_terms <- paste(vars[3:4], collapse = " + ")
   
   # Reconstruct GAM-friendly predictor string
   preds_gam <- paste(
      smooth_terms,
      interaction_term,
      linear_terms,
      sep = " + "
   )
   
   # Buld formula
   form <- as.formula(paste0(base_formula, preds_gam))
   
   gam_fit <- gam(formula = form, family = binomial(link = "logit"), data = data, weights = weight, method = "REML", na.action = na.exclude)
   
}



# =============================================================================
# Run Salinity Model, Create Performance Plots, and Save Outputs
# =============================================================================
run_logistic_regression_analysis <- function(data, 
                                             weight_val = 1,
                                             threshold_quantile = 0.7, 
                                             threshold_val = NULL,
                                             LR,
                                             GAM,
                                             other_preds = c('RollingPowInflows', 'PowDischarge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                             output_path = "models",
                                             plot_path = "plots",
                                             years = c(2014, 2015, 2016, 2017, 2018),
                                             months = c(6, 7, 8, 9, 10, 11),
                                             predictor_vars = c('RollingPowInflows', 'PowDischarge'),
                                             start_date = "2016-10-01",
                                             end_date = "2016-10-31") {
   
   # Determine threshold value
   if (!is.null(threshold_val)) {
      threshold_value <- threshold_val
      thresh_label <- paste0("val", round(threshold_value, 3))
   } else {
      threshold_value <- quantile(data$Salinity, threshold_quantile)
      thresh_label <- paste0("q", threshold_quantile, "_", round(threshold_value, 3))
   }
   
   # Create unique folder name - exclude DayOfYear variables since they're in all models
   other_preds_clean <- other_preds[!grepl("DayOfYear", other_preds)]
   other_preds_string <- paste(other_preds_clean, collapse = "_")
   pred_vars_string <- paste(predictor_vars, collapse = "_")
   
   # Get run counter from parent environment if it exists for guaranteed uniqueness
   if (exists("counter", envir = parent.frame())) {
      run_id <- get("counter", envir = parent.frame())
      folder_name <- paste0("Run", sprintf("%02d", run_id), "_thresh", thresh_label, 
                            "_", gsub("[^A-Za-z0-9_]", "", other_preds_string))
   } else {
      # Fallback for single runs - use timestamp for uniqueness
      timestamp <- format(Sys.time(), "%H%M%S")
      folder_name <- paste0("LR_", timestamp, "_thresh", thresh_label,
                            "_", gsub("[^A-Za-z0-9_]", "", other_preds_string))
   }
   
   # Create directories
   model_dir <- file.path(output_path, folder_name)
   plot_dir <- file.path(plot_path, folder_name)
   dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
   dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
   
   # Prepare data
   model_data <- data %>%
      mutate(actual_exceedance = Salinity > threshold_value) %>%
      mutate(weight = ifelse(actual_exceedance == TRUE, weight_val, 1))
   
   # Build predictor string for run_LR function
   other_preds_string_for_model <- paste(other_preds, collapse = " + ")
   
   # Run logistic regression
   if (LR == TRUE) {
      model <- run_LR(model_data, other_preds = other_preds_string_for_model, weights = model_data$weight)
   } else if (GAM == TRUE) {
      model <- run_GAM(model_data, other_preds = other_preds_string_for_model, weights = model_data$weight)
   }
   
   # Add predictions to data
   prediction <- predict(model, type = "link", se.fit = TRUE)
   model_data$exceedance_probability <- plogis(prediction$fit) # transform to the response scale 
   model_data$exceedance_lower <- plogis(prediction$fit - 1.96 * prediction$se.fit) # 97.5th%
   model_data$exceedance_upper <- plogis(prediction$fit + 1.96 * prediction$se.fit) # 2.5th%
   
   # Save model
   model_file <- file.path(model_dir, "logistic_model.rds")
   saveRDS(LR, model_file)
   
   # Save model summary
   summary_file <- file.path(model_dir, "model_summary.txt")
   capture.output(summary(model), file = summary_file)
   
   # Save model data with predictions
   data_file <- file.path(model_dir, "model_data_with_predictions.rds")
   saveRDS(model_data, data_file)
   
   # Create and save plots
   
   # 1. Salinity exceedance plot
   exceedance_plot <- create_salinity_exceedance_plot(
      data = model_data,
      years = years,
      months = months,
      threshold = threshold_value,
      plot_title = paste0("Salinity Exceedance - Threshold: ", round(threshold_value, 2)),
      predictor_vars = predictor_vars
   )
   
   ggsave(file.path(plot_dir, "salinity_exceedance_plot.png"), 
          exceedance_plot, width = 12, height = 8, dpi = 600)
   
   # 2. Logistic performance plots
   model_name <- paste0('Logistic Regression on Threshold ', round(threshold_value, 2))
   results <- plot_logistic_performance(model_data, model_name = model_name)
   
   ggsave(file.path(plot_dir, "key_metrics_plot.png"), 
          results$key_metrics_plot, width = 10, height = 6, dpi = 600)
   
   ggsave(file.path(plot_dir, "prob_metrics_plot.png"), 
          results$prob_metrics_plot, width = 10, height = 6, dpi = 600)
   
   ggsave(file.path(plot_dir, "calibration_plot.png"), 
          results$calibration_plot, width = 8, height = 6, dpi = 600)
   
   ggsave(file.path(plot_dir, "distribution_plot.png"), 
          results$distribution_plot, width = 8, height = 6, dpi = 600)
   
   # 3. Logistic timeseries plot
   timeseries_plot <- plot_logistic_timeseries(model_data, 
                                               start_date = start_date, 
                                               end_date = end_date)
   
   ggsave(file.path(plot_dir, "logistic_timeseries.png"), 
          timeseries_plot, width = 12, height = 6, dpi = 600)
   
   # 4. Matrix scatter plots
   # Create the three scatter plots
   p1 <- create_matrix_plot(model_data, model, predictor_vars[1], "DayOfYear") + 
      theme(legend.position = 'none')
   
   p2 <- create_matrix_plot(model_data, model, predictor_vars[1], predictor_vars[2]) + 
      theme(legend.position = "right")
   
   p3 <- create_matrix_plot(model_data, model, "DayOfYear", predictor_vars[2]) + 
      theme(legend.position = "none")
   
   # Combine into matrix plot
   matrix_plot <- (p2) /
      (p1 | p3) +
      plot_annotation(
         title = "Matrix Plot for Salinity Exceedance Logistic Regression",
         subtitle = paste0("Contours show logistic regression predicted exceedance probability | Points colored by actual exceedance | ", 
                           format(as.Date(start_date), "%B %Y"), " data are outlined in black"),
         theme = theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11),
            plot.caption = element_text(size = 9, color = "gray50")
         )
      )
   
   ggsave(file.path(plot_dir, "matrix_scatter_plot.png"), 
          matrix_plot, width = 14, height = 9, dpi = 600)
   
   # Create a metadata file with run parameters
   metadata <- list(
      threshold_quantile = threshold_quantile,
      threshold_val = threshold_val,
      threshold_value = threshold_value,
      other_preds = other_preds,
      predictor_vars = predictor_vars,
      years = years,
      months = months,
      start_date = start_date,
      end_date = end_date,
      run_timestamp = Sys.time(),
      folder_name = folder_name
   )
   
   metadata_file <- file.path(model_dir, "run_metadata.rds")
   saveRDS(metadata, metadata_file)
   
   # Return useful information
   return(list(
      model = model,
      data = model_data,
      results = results,
      folder_name = folder_name,
      model_dir = model_dir,
      plot_dir = plot_dir,
      threshold_value = threshold_value,
      metadata = metadata
   ))
}

# =============================================================================
# =============================================================================
run_multiple_lr_analyses <- function(data, 
                                     output_path = OUTPUT_PATH, 
                                     plot_path = PLOT_PATH,
                                     LR,
                                     GAM,
                                     weight = 1, 
                                     threshold_quantiles = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.96, 0.97, 0.98, 0.99, 1.0),
                                     predictor_combinations = list(
                                        c('RollingPowInflows', 'PowDischarge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                        c('RollingLogInflows', 'LogDischarge', 'DayOfYear_sin', 'DayOfYear_cos')
                                     )) {
   
   results_summary <- data.frame()
   all_results <- list()
   
   counter <- 1
   for (thresh in threshold_quantiles) {
      for (preds in predictor_combinations) {
         
         cat("Running analysis", counter, "- Threshold:", thresh, "Predictors:", paste(preds, collapse = ", "), "\n")
         
         # Run the analysis
         result <- run_logistic_regression_analysis(
            data = data,
            threshold_quantile = thresh,
            other_preds = preds,
            output_path = output_path,
            plot_path = plot_path,
            predictor_vars = c(preds[1 : 2]), 
            weight_val = weight,
            LR = LR,
            GAM = GAM
         )
         
         # Store results
         all_results[[counter]] <- result
         
         # Add to summary
         results_summary <- rbind(results_summary, data.frame(
            run_id = counter,
            threshold_quantile = thresh,
            threshold_value = result$threshold_value,
            predictors = paste(preds, collapse = ", "),
            folder_name = result$folder_name,
            stringsAsFactors = FALSE
         ))
         
         counter <- counter + 1
      }
   }
   
   # Save overall summary
   write.csv(results_summary, file.path(output_path, "analysis_summary.csv"), row.names = FALSE)
   saveRDS(all_results, file.path(output_path, "all_results.rds"))
   
   return(list(
      summary = results_summary,
      all_results = all_results
   ))
}





