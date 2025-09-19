library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


run_LR <- function(data, base_formula = 'actual_exceedance ~', other_preds) {
   formula <- paste0(base_formula, other_preds)
   regression <- glm(formula, data, family = binomial(link = 'logit'), na.action = na.exclude)
}

run_logistic_regression_analysis <- function(data, 
                                             threshold_quantile = 0.7, 
                                             threshold_val = NULL,
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
      mutate(actual_exceedance = Salinity > threshold_value)
   
   # Build predictor string for run_LR function
   other_preds_string_for_model <- paste(other_preds, collapse = " + ")
   
   # Run logistic regression
   LR <- run_LR(model_data, other_preds = other_preds_string_for_model)
   
   # Add predictions to data
   model_data$exceedance_probability <- predict(LR, type = "response")
   
   # Save model
   model_file <- file.path(model_dir, "logistic_model.rds")
   saveRDS(LR, model_file)
   
   # Save model summary
   summary_file <- file.path(model_dir, "model_summary.txt")
   capture.output(summary(LR), file = summary_file)
   
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
   p1 <- create_scatter_plot_with_prob(model_data, LR, predictor_vars[1], "DayOfYear") + 
      theme(legend.position = 'none')
   
   p2 <- create_scatter_plot_with_prob(model_data, LR, predictor_vars[1], predictor_vars[2]) + 
      theme(legend.position = "right")
   
   p3 <- create_scatter_plot_with_prob(model_data, LR, "DayOfYear", predictor_vars[2]) + 
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
      model = LR,
      data = model_data,
      results = results,
      folder_name = folder_name,
      model_dir = model_dir,
      plot_dir = plot_dir,
      threshold_value = threshold_value,
      metadata = metadata
   ))
}




# Adapted function for single model performance evaluation
plot_logistic_performance <- function(data, 
                                      model_name = "Logistic Model",
                                      prob_threshold = 0.5,
                                      salinity_threshold = NULL) {
   
   # Extract threshold from data if not provided
   if (is.null(salinity_threshold)) {
      # Infer threshold from data - find minimum salinity where actual_exceedance is TRUE
      salinity_threshold <- data %>%
         filter(actual_exceedance == TRUE) %>%
         pull(Salinity) %>%
         min(na.rm = TRUE)
   }
   
   # Calculate basic confusion matrix metrics
   confusion_metrics <- data %>%
      summarise(
         # True/False Positives/Negatives
         TP = sum(actual_exceedance == TRUE & exceedance_probability > prob_threshold, na.rm = TRUE),
         TN = sum(actual_exceedance == FALSE & exceedance_probability <= prob_threshold, na.rm = TRUE),
         FP = sum(actual_exceedance == FALSE & exceedance_probability > prob_threshold, na.rm = TRUE),
         FN = sum(actual_exceedance == TRUE & exceedance_probability <= prob_threshold, na.rm = TRUE),
         
         # Derived metrics
         precision = ifelse(TP + FP > 0, TP / (TP + FP), 0),
         sensitivity = ifelse(TP + FN > 0, TP / (TP + FN), 0),  # Recall
         specificity = ifelse(TN + FP > 0, TN / (TN + FP), 0),
         f1_score = ifelse(precision + sensitivity > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0),
         accuracy = (TP + TN) / (TP + TN + FP + FN),
         
         # False rates
         false_positive_rate = ifelse(TN + FP > 0, FP / (TN + FP), 0),
         false_negative_rate = ifelse(TP + FN > 0, FN / (TP + FN), 0),
         
         # Probabilistic metrics
         brier_score = mean((exceedance_probability - as.numeric(actual_exceedance))^2, na.rm = TRUE),
         log_loss = -mean(
            as.numeric(actual_exceedance) * log(pmax(exceedance_probability, 1e-15)) + 
               (1 - as.numeric(actual_exceedance)) * log(pmax(1 - exceedance_probability, 1e-15)),
            na.rm = TRUE
         )
      ) %>%
      mutate(model = model_name)
   
   # Calculate calibration data
   calibration_data <- data %>%
      mutate(prob_bin = cut(exceedance_probability, 
                            breaks = seq(0, 1, 0.05), 
                            include.lowest = TRUE)) %>%
      group_by(prob_bin) %>%
      summarise(
         predicted_prob = mean(exceedance_probability, na.rm = TRUE),
         observed_freq = mean(as.numeric(actual_exceedance), na.rm = TRUE),
         n_obs = n(),
         .groups = "drop"
      ) %>%
      filter(n_obs >= 10)  # Only bins with sufficient observations
   
   # Plot 1: Key metrics
   key_metrics_plot <- function() {
      key_metrics <- confusion_metrics %>%
         select(precision, sensitivity, f1_score, false_positive_rate, false_negative_rate) %>%
         pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
         mutate(metric_label = case_when(
            metric == 'false_positive_rate' ~ 'False Positive Rate',
            metric == 'false_negative_rate' ~ 'False Negative Rate',
            metric == 'sensitivity' ~ 'Recall/Sensitivity',
            metric == 'precision' ~ 'Precision',
            metric == "f1_score" ~ "F1 Score"
         )) %>%
         mutate(metric_label = factor(metric_label,
                                      levels = c('False Positive Rate', 'False Negative Rate', 
                                                 'Recall/Sensitivity', 'Precision', 'F1 Score')))
      
      ggplot(key_metrics, aes(x = metric_label, y = value, fill = metric_label)) +
         geom_col(width = 0.7) +
         geom_text(aes(label = paste0(round(value * 100, 1), "%")),
                   vjust = -0.3, size = 4, fontface = "bold") +
         scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
         scale_fill_manual(
            values = c("False Positive Rate" = "darkred",
                       'False Negative Rate' = 'orange',
                       "Recall/Sensitivity" = "steelblue",
                       "Precision" = "darkgreen", 
                       "F1 Score" = "purple")
         ) +
         labs(title = paste("Classification Performance:", model_name),
              subtitle = paste("Confusion matrix metrics at", prob_threshold, "probability threshold"),
              x = "Metric", y = "Performance") +
         theme_bw(base_size = 11) +
         theme(legend.position = "none",
               axis.text.x = element_text(angle = 45, hjust = 1))
   }
   
   # Plot 2: Probabilistic performance metrics  
   prob_metrics_plot <- function() {
      prob_data <- data.frame(
         metric = c("Accuracy", "Brier Score", "Log Loss"),
         value = c(confusion_metrics$accuracy, confusion_metrics$brier_score, confusion_metrics$log_loss),
         better = c("Higher", "Lower", "Lower")
      )
      
      ggplot(prob_data, aes(x = metric, y = value, fill = better)) +
         geom_col(width = 0.6) +
         geom_text(aes(label = round(value, 3)), vjust = -0.3, size = 4, fontface = "bold") +
         scale_fill_manual(values = c("Higher" = "darkgreen", "Lower" = "darkred")) +
         labs(title = "Additional Performance Metrics",
              subtitle = "Accuracy (higher better), Brier Score & Log Loss (lower better)",
              x = "Metric", y = "Score") +
         theme_bw(base_size = 11) +
         theme(legend.position = "none")
   }
   
   # Plot 3: Calibration plot
   calibration_plot <- function() {
      ggplot(calibration_data, aes(x = predicted_prob, y = observed_freq)) +
         geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.7, size = 1) +
         geom_point(aes(size = n_obs), color = "steelblue", alpha = 0.7) +
         geom_smooth(method = "loess", se = TRUE, color = "darkred", size = 1.2) +
         scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
         scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
         labs(
            title = "Model Calibration",
            subtitle = "Perfect calibration follows diagonal line. Points sized by # observations.",
            x = "Predicted Probability",
            y = "Observed Frequency",
            size = "# Observations"
         ) +
         theme_bw(base_size = 11) +
         theme(legend.position = "bottom")
   }
   
   # Plot 4: Probability distributions
   prob_dist_plot <- function() {
      plot_data <- data %>%
         mutate(outcome = ifelse(actual_exceedance, "Exceedance", "No Exceedance"))
      
      ggplot(plot_data, aes(x = exceedance_probability, fill = outcome)) +
         geom_histogram(alpha = 0.7, position = "identity", bins = 50) +
         geom_vline(xintercept = prob_threshold, linetype = "dashed", color = "red", size = 1) +
         scale_fill_manual(values = c("No Exceedance" = "lightblue", "Exceedance" = "coral")) +
         labs(title = "Predicted Probability Distribution",
              subtitle = paste("Red line shows decision threshold (", prob_threshold, ")"),
              x = "Predicted Probability",
              y = "Count",
              fill = "Actual Outcome") +
         theme_bw(base_size = 11) +
         theme(legend.position = "bottom")
   }
   
   # Generate all plots
   key_plot <- key_metrics_plot()
   prob_plot <- prob_metrics_plot()
   cal_plot <- calibration_plot()
   dist_plot <- prob_dist_plot()
   
   # Combine plots
   combined_plot <- (key_plot | prob_plot) / (cal_plot | dist_plot) + 
      plot_layout(heights = c(1, 1))
   
   return(list(
      combined_plot = combined_plot,
      key_metrics_plot = key_plot, 
      prob_metrics_plot = prob_plot,
      calibration_plot = cal_plot,
      distribution_plot = dist_plot,
      metrics_table = confusion_metrics,
      calibration_data = calibration_data
   ))
}

# Function to compare two models
compare_logistic_models <- function(data1, data2, 
                                    model1_name = "Model 1", 
                                    model2_name = "Model 2",
                                    prob_threshold = 0.5) {
   
   # Get metrics for both models
   calc_metrics <- function(data, name) {
      data %>%
         summarise(
            model = name,
            TP = sum(actual_exceedance == TRUE & exceedance_probability > prob_threshold, na.rm = TRUE),
            TN = sum(actual_exceedance == FALSE & exceedance_probability <= prob_threshold, na.rm = TRUE),
            FP = sum(actual_exceedance == FALSE & exceedance_probability > prob_threshold, na.rm = TRUE),
            FN = sum(actual_exceedance == TRUE & exceedance_probability <= prob_threshold, na.rm = TRUE),
            precision = ifelse(TP + FP > 0, TP / (TP + FP), 0),
            sensitivity = ifelse(TP + FN > 0, TP / (TP + FN), 0),
            specificity = ifelse(TN + FP > 0, TN / (TN + FP), 0),
            f1_score = ifelse(precision + sensitivity > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0),
            accuracy = (TP + TN) / (TP + TN + FP + FN),
            false_positive_rate = ifelse(TN + FP > 0, FP / (TN + FP), 0),
            false_negative_rate = ifelse(TP + FN > 0, FN / (TP + FN), 0),
            brier_score = mean((exceedance_probability - as.numeric(actual_exceedance))^2, na.rm = TRUE),
            log_loss = -mean(
               as.numeric(actual_exceedance) * log(pmax(exceedance_probability, 1e-15)) + 
                  (1 - as.numeric(actual_exceedance)) * log(pmax(1 - exceedance_probability, 1e-15)),
               na.rm = TRUE
            )
         )
   }
   
   metrics1 <- calc_metrics(data1, model1_name)
   metrics2 <- calc_metrics(data2, model2_name)
   
   combined_metrics <- bind_rows(metrics1, metrics2)
   
   # Comparison plot
   comparison_plot <- combined_metrics %>%
      select(model, precision, sensitivity, f1_score, false_positive_rate, false_negative_rate) %>%
      pivot_longer(cols = c(precision, sensitivity, f1_score, false_positive_rate, false_negative_rate),
                   names_to = "metric", values_to = "value") %>%
      mutate(metric_label = case_when(
         metric == 'false_positive_rate' ~ 'False Positive Rate',
         metric == 'false_negative_rate' ~ 'False Negative Rate',
         metric == 'sensitivity' ~ 'Recall/Sensitivity',
         metric == 'precision' ~ 'Precision',
         metric == "f1_score" ~ "F1 Score"
      )) %>%
      ggplot(aes(x = metric_label, y = value, fill = model)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = paste0(round(value * 100, 1), "%")),
                position = position_dodge(width = 0.8),
                vjust = -0.3, size = 3.5) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
      scale_fill_manual(values = c("steelblue", "coral")) +
      labs(title = "Model Comparison",
           subtitle = paste("Performance metrics at", prob_threshold, "probability threshold"),
           x = "Metric", y = "Performance", fill = "Model") +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
   
   return(list(
      comparison_plot = comparison_plot,
      metrics_table = combined_metrics
   ))
}

# Time series plot adapted for single model
plot_logistic_timeseries <- function(data, 
                                     start_date, 
                                     end_date, 
                                     model_name = "Logistic Model",
                                     salinity_threshold = NULL) {
   
   # Extract threshold from data if not provided
   if (is.null(salinity_threshold)) {
      salinity_threshold <- data %>%
         filter(actual_exceedance == TRUE) %>%
         pull(Salinity) %>%
         min(na.rm = TRUE)
   }
   
   # Filter data for specified time period
   period_data <- data %>%
      filter(Date >= as.POSIXct(start_date) & 
                Date <= as.POSIXct(end_date))
   
   # Generate title
   title_text <- paste0(model_name, " - ", 
                        format(as.Date(start_date), "%b %Y"), " to ",
                        format(as.Date(end_date), "%b %Y"))
   
   # Create the plot
   ggplot(period_data, aes(x = Date)) +
      # Predicted probability
      geom_line(aes(y = exceedance_probability), color = "steelblue", size = 0.8) +
      
      # Raw salinity (rescaled for secondary axis)
      geom_line(aes(y = Salinity / max(Salinity, na.rm = TRUE)), 
                color = "grey60", linetype = "solid", alpha = 0.7) +
      
      # Threshold line
      geom_hline(yintercept = salinity_threshold / max(period_data$Salinity, na.rm = TRUE), 
                 color = "red", linetype = "dashed", alpha = 0.8) +
      
      # True exceedances
      geom_point(
         data = period_data %>% filter(actual_exceedance == TRUE),
         aes(y = Salinity / max(Salinity, na.rm = TRUE)),
         color = "red", size = 1.5, alpha = 0.8
      ) +
      
      scale_y_continuous(
         name = "Exceedance Probability",
         limits = c(0, 1),
         sec.axis = sec_axis(~ . * max(period_data$Salinity, na.rm = TRUE), 
                             name = "Salinity (psu)")
      ) +
      
      labs(
         x = "Date",
         title = title_text,
         subtitle = paste0("Blue: predicted probability | Grey: observed salinity | Red points: actual exceedances (threshold: ", 
                           round(salinity_threshold, 2), ")")
      ) +
      theme_bw(base_size = 12) +
      theme(
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 45, hjust = 1)
      )
}


create_scatter_plot_with_prob <- function(data, model, x_var, y_var, prob_breaks = seq(0, 1, by = 0.1)) {
   library(ggnewscale)
   
   model_vars <- all.vars(formula(model))[-1]
   
   # Smart sample
   sampled_data <- data %>%
      mutate(is_oct_2016 = (Year == 2016 & Month == 10)) %>%
      group_by(is_oct_2016) %>%
      sample_n(
         size = ifelse(first(is_oct_2016), 
                       min(n(), sum(data$Year == 2016 & data$Month == 10)), 
                       min(n(), 5000)),
         replace = FALSE
      ) %>%
      ungroup() %>%
      select(-is_oct_2016)
   
   # Prediction grid
   grid_df <- expand.grid(
      x = seq(min(sampled_data[[x_var]], na.rm = TRUE), 
              max(sampled_data[[x_var]], na.rm = TRUE), length.out = 100),
      y = seq(min(sampled_data[[y_var]], na.rm = TRUE), 
              max(sampled_data[[y_var]], na.rm = TRUE), length.out = 100)
   )
   names(grid_df) <- c(x_var, y_var)
   
   for (v in model_vars) {
      if (!v %in% names(grid_df)) {
         grid_df[[v]] <- mean(data[[v]], na.rm = TRUE)
      }
   }
   
   if ("DayOfYear" %in% c(x_var, y_var) || "DayOfYear" %in% model_vars) {
      if (!"DayOfYear_sin" %in% names(grid_df)) {
         grid_df$DayOfYear_sin <- sin(2 * pi * grid_df$DayOfYear / 365.25)
      }
      if (!"DayOfYear_cos" %in% names(grid_df)) {
         grid_df$DayOfYear_cos <- cos(2 * pi * grid_df$DayOfYear / 365.25)
      }
   }
   
   grid_df$pred_prob <- predict(model, newdata = grid_df, type = "response")
   
   
   # Plot with *fixed probability breaks*
   p <- ggplot() +
      # Contour fill (first fill scale)
      geom_contour_filled(
         data = grid_df,
         aes_string(x = x_var, y = y_var, z = "pred_prob"),
         breaks = prob_breaks,
         alpha = 0.5
      ) +
      scale_fill_viridis_d(
         name = "Exceedance\nProbability",
         drop = FALSE
      ) +
      
      # Contour lines
      geom_contour(
         data = grid_df,
         aes_string(x = x_var, y = y_var, z = "pred_prob"),
         breaks = prob_breaks,
         color = "black",
         linewidth = 0.3
      ) +
      
      # Regular points (not Oct 2016)
      geom_point(
         data = subset(sampled_data, !(Year == 2016 & Month == 10)),
         aes(x = !!sym(x_var), y = !!sym(y_var), color = as.factor(actual_exceedance)),
         size = 1.5, alpha = 0.8
      ) +
      scale_color_manual(
         values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
         name = "Actual Exceedance",
         labels = c("No", "Yes")
      ) +
      
      # Start a new fill scale for Oct 2016 points
      ggnewscale::new_scale_fill() +
      
      # Oct 2016 points with exceedance color + black outline
      geom_point(
         data = subset(sampled_data, Year == 2016 & Month == 10),
         aes(x = !!sym(x_var), y = !!sym(y_var), fill = as.factor(actual_exceedance)),
         shape = 21, size = 1.5, stroke = 0.6, color = "black", alpha = 0.8
      ) +
      scale_fill_manual(
         values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
         name = "Actual Exceedance",
         labels = c("No", "Yes")
      ) +
      
      theme_minimal() +
      theme(
         panel.grid.minor = element_blank(),
         panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
         panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5),
         axis.title = element_text(size = 10),
         axis.text = element_text(size = 8),
         legend.position = "bottom",
         legend.title = element_text(size = 11, face = "bold"),
         legend.text = element_text(size = 10)
      ) +
      labs(
         x = gsub("_", " ", x_var),
         y = gsub("_", " ", y_var)
      )
   
   
   return(p)
}

create_legend_plot <- function(data, grid_df, x_var, y_var, prob_breaks) {
   
   model_vars <- all.vars(formula(model))[-1]
   
   # Smart sample
   sampled_data <- data %>%
      mutate(is_oct_2016 = (Year == 2016 & Month == 10)) %>%
      group_by(is_oct_2016) %>%
      sample_n(
         size = ifelse(first(is_oct_2016), 
                       min(n(), sum(data$Year == 2016 & data$Month == 10)), 
                       min(n(), 2000)),
         replace = FALSE
      ) %>%
      ungroup() %>%
      select(-is_oct_2016)
   
   # Prediction grid
   grid_df <- expand.grid(
      x = seq(min(sampled_data[[x_var]], na.rm = TRUE), 
              max(sampled_data[[x_var]], na.rm = TRUE), length.out = 100),
      y = seq(min(sampled_data[[y_var]], na.rm = TRUE), 
              max(sampled_data[[y_var]], na.rm = TRUE), length.out = 100)
   )
   names(grid_df) <- c(x_var, y_var)
   
   for (v in model_vars) {
      if (!v %in% names(grid_df)) {
         grid_df[[v]] <- mean(data[[v]], na.rm = TRUE)
      }
   }
   
   if ("DayOfYear" %in% c(x_var, y_var) || "DayOfYear" %in% model_vars) {
      if (!"DayOfYear_sin" %in% names(grid_df)) {
         grid_df$DayOfYear_sin <- sin(2 * pi * grid_df$DayOfYear / 365)
      }
      if (!"DayOfYear_cos" %in% names(grid_df)) {
         grid_df$DayOfYear_cos <- cos(2 * pi * grid_df$DayOfYear / 365)
      }
   }
   
   grid_df$pred_prob <- predict(model, newdata = grid_df, type = "response")
   
   ggplot() +
      # Contour fill
      geom_contour_filled(
         data = grid_df,
         aes_string(x = x_var, y = y_var, z = "pred_prob"),
         breaks = prob_breaks
      ) +
      
      # Contour outlines
      geom_contour(
         data = grid_df,
         aes_string(x = x_var, y = y_var, z = "pred_prob"),
         breaks = prob_breaks,
         color = "black",
         linewidth = 0.3
      ) +
      
      # Red/blue points (regular exceedance points)
      geom_point(
         data = data,
         aes(x = !!sym(x_var), y = !!sym(y_var), color = as.factor(actual_exceedance)),
         size = 4  # bigger than panel points
      ) +
      
      scale_fill_viridis_d(
         name = "Exceedance\nProbability",
         drop = FALSE
      ) +
      
      scale_color_manual(
         values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
         name = "Actual Exceedance",
         labels = c("No", "Yes")
      ) +
      
      theme_void() +
      theme(
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.title = element_text(size = 12, face = "bold"),
         legend.text = element_text(size = 11)
      )
}

