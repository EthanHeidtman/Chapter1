# =============================================================================
# Script Name:    ThresholdPerformancePlots.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Gathers the outputs from the covariance experiment runs created
#                 by RunWindowExperiments.R. 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================


analyze_probability_threshold_performance <- function(data,
                                                      actual_col = "Salinity",
                                                      predicted_col = "exceedance_probability", 
                                                      threshold_col = "salinity_threshold") {
   
   # Calculate actual exceedances and use 0.5 as natural probability threshold
   analysis_data <- data %>%
      mutate(
         actual_exceedance = !!sym(actual_col) > !!sym(threshold_col),
         predicted_exceedance = !!sym(predicted_col) > 0.5  # Natural threshold for probabilities
      )
   
   # Calculate confusion matrix metrics by experimental threshold
   confusion_metrics <- analysis_data %>%
      group_by(!!sym(threshold_col)) %>%
      summarise(
         # Confusion matrix components
         tp = sum(actual_exceedance & predicted_exceedance, na.rm = TRUE),
         fp = sum(!actual_exceedance & predicted_exceedance, na.rm = TRUE),
         tn = sum(!actual_exceedance & !predicted_exceedance, na.rm = TRUE),
         fn = sum(actual_exceedance & !predicted_exceedance, na.rm = TRUE),
         
         # Sample sizes and rates
         total_obs = n(),
         actual_exceedances = sum(actual_exceedance, na.rm = TRUE),
         predicted_exceedances = sum(predicted_exceedance, na.rm = TRUE),
         exceedance_rate = mean(actual_exceedance, na.rm = TRUE),
         prediction_rate = mean(predicted_exceedance, na.rm = TRUE),
         
         # Probability summaries (using if_else to filter cleanly)
         mean_prob_all = mean(!!sym(predicted_col), na.rm = TRUE),
         mean_prob_when_exceeded = mean(if_else(actual_exceedance, !!sym(predicted_col), NA_real_), na.rm = TRUE),
         mean_prob_when_not_exceeded = mean(if_else(!actual_exceedance, !!sym(predicted_col), NA_real_), na.rm = TRUE),
         median_prob_when_exceeded = median(if_else(actual_exceedance, !!sym(predicted_col), NA_real_), na.rm = TRUE),
         prob_separation = mean_prob_when_exceeded - mean_prob_when_not_exceeded,
         
         # Calibration metrics
         prob_above_50_when_exceeded = mean(if_else(actual_exceedance, !!sym(predicted_col) > 0.5, NA), na.rm = TRUE),
         prob_above_50_when_not_exceeded = mean(if_else(!actual_exceedance, !!sym(predicted_col) > 0.5, NA), na.rm = TRUE),
         
         .groups = "drop"
      ) %>%
      mutate(
         # Standard classification metrics
         sensitivity = tp / (tp + fn),           # True Positive Rate (Recall)
         specificity = tn / (tn + fp),           # True Negative Rate  
         precision = tp / (tp + fp),             # Positive Predictive Value
         npv = tn / (tn + fn),                   # Negative Predictive Value
         accuracy = (tp + tn) / (tp + fp + tn + fn),
         f1_score = 2 * (precision * sensitivity) / (precision + sensitivity),
         false_positive_rate = fp / (fp + tn),   # 1 - Specificity
         false_negative_rate = fn / (fn + tp),   # 1 - Sensitivity
         
         # Handle division by zero (replace NaN/Inf with defaults)
         sensitivity = ifelse(is.nan(sensitivity), 0, sensitivity),
         specificity = ifelse(is.nan(specificity), 1, specificity), 
         precision = ifelse(is.nan(precision), 0, precision),
         f1_score = ifelse(is.nan(f1_score), 0, f1_score)
      )
   
   
   # Create visualizations
   plots <- create_probability_threshold_plots(confusion_metrics, threshold_col)
   
   return(list(
      metrics = confusion_metrics,
      plots = plots
   ))
}

#' Create visualization plots for probability threshold analysis
create_probability_threshold_plots <- function(confusion_metrics, threshold_col) {
   
   # Plot 1: Key performance metrics
   key_metrics <- confusion_metrics %>%
      select(all_of(threshold_col), sensitivity, specificity, precision, f1_score, accuracy) %>%
      pivot_longer(cols = c(sensitivity, specificity, precision, f1_score, accuracy),
                   names_to = "metric", values_to = "value") %>%
      mutate(
         metric_label = case_when(
            metric == "sensitivity" ~ "Sensitivity (Recall)",
            metric == "specificity" ~ "Specificity",
            metric == "precision" ~ "Precision", 
            metric == "f1_score" ~ "F1 Score",
            metric == "accuracy" ~ "Accuracy"
         ),
         metric_type = case_when(
            metric %in% c("sensitivity", "precision", "f1_score") ~ "Detection Performance",
            metric %in% c("specificity", "accuracy") ~ "Overall Performance"
         )
      )
   
   p_key_metrics <- ggplot(key_metrics, 
                           aes(x = !!sym(threshold_col), y = value, 
                               color = metric_label, shape = metric_label)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      facet_wrap(~metric_type, ncol = 1) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_color_manual(values = c(
         "Sensitivity (Recall)" = "#2E8B57",
         "Specificity" = "#4682B4", 
         "Precision" = "#DC143C",
         "F1 Score" = "#FF8C00",
         "Accuracy" = "#8A2BE2"
      )) +
      labs(title = "Model Performance: Probability > 0.5 Predicts Exceedance",
           subtitle = "Natural interpretation: P > 0.5 means 'likely to exceed threshold'",
           x = "Experimental Salinity Threshold", 
           y = "Performance Metric",
           color = "Metric",
           shape = "Metric") +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill = "lightgray"))
   
   # Plot 2: Probability behavior and calibration
   prob_data <- confusion_metrics %>%
      select(all_of(threshold_col), mean_prob_when_exceeded, mean_prob_when_not_exceeded, 
             prob_above_50_when_exceeded, prob_above_50_when_not_exceeded) %>%
      pivot_longer(cols = c(mean_prob_when_exceeded, mean_prob_when_not_exceeded),
                   names_to = "condition", values_to = "mean_probability") %>%
      mutate(
         condition_label = case_when(
            condition == "mean_prob_when_exceeded" ~ "During Exceedances",
            condition == "mean_prob_when_not_exceeded" ~ "During Non-Exceedances"
         )
      )
   
   p_prob_behavior <- ggplot(prob_data, 
                             aes(x = !!sym(threshold_col), y = mean_probability, 
                                 color = condition_label, linetype = condition_label)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_y_continuous(labels = percent, limits = c(0, max(prob_data$mean_probability, na.rm = TRUE) * 1.1)) +
      scale_color_manual(values = c("During Exceedances" = "#DC143C", 
                                    "During Non-Exceedances" = "#4682B4")) +
      scale_linetype_manual(values = c("During Exceedances" = "solid", 
                                       "During Non-Exceedances" = "solid")) +
      labs(title = "Average Predicted Probabilities by Actual Outcome",
           subtitle = "Red dashed line at 0.5 = natural decision boundary",
           x = "Experimental Salinity Threshold", 
           y = "Mean Predicted Probability",
           color = "Actual Outcome",
           linetype = "Actual Outcome") +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom")
   
   # Plot 3: Calibration analysis
   calibration_data <- confusion_metrics %>%
      select(all_of(threshold_col), prob_above_50_when_exceeded, prob_above_50_when_not_exceeded) %>%
      pivot_longer(cols = c(prob_above_50_when_exceeded, prob_above_50_when_not_exceeded),
                   names_to = "condition", values_to = "rate_above_50") %>%
      mutate(
         condition_label = case_when(
            condition == "prob_above_50_when_exceeded" ~ "During Actual Exceedances",
            condition == "prob_above_50_when_not_exceeded" ~ "During Actual Non-Exceedances"
         ),
         ideal_rate = case_when(
            condition == "prob_above_50_when_exceeded" ~ 1.0,  # Should be 100% for good calibration
            condition == "prob_above_50_when_not_exceeded" ~ 0.0  # Should be 0% for good calibration
         )
      )
   
   p_calibration <- ggplot(calibration_data, 
                           aes(x = !!sym(threshold_col), y = rate_above_50, 
                               color = condition_label, shape = condition_label)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_line(aes(y = ideal_rate), linetype = "dashed", alpha = 0.7, size = 1) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_color_manual(values = c("During Actual Exceedances" = "#2E8B57", 
                                    "During Actual Non-Exceedances" = "#DC143C")) +
      labs(title = "Model Calibration: % Predictions > 0.5 by Actual Outcome",
           subtitle = "Dashed lines show ideal calibration (100% for exceedances, 0% for non-exceedances)",
           x = "Experimental Salinity Threshold", 
           y = "% Predictions Above 0.5",
           color = "Actual Outcome",
           shape = "Actual Outcome") +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom")
   
   # Plot 4: Task difficulty and model response
   task_data <- confusion_metrics %>%
      select(all_of(threshold_col), exceedance_rate, prediction_rate, prob_separation)
   
   p_task_difficulty <- task_data %>%
      pivot_longer(cols = c(exceedance_rate, prediction_rate, prob_separation),
                   names_to = "characteristic", values_to = "value") %>%
      mutate(
         char_label = case_when(
            characteristic == "exceedance_rate" ~ "Actual Exceedance Rate",
            characteristic == "prediction_rate" ~ "Model Prediction Rate (P > 0.5)", 
            characteristic == "prob_separation" ~ "Probability Separation"
         ),
         char_type = case_when(
            characteristic %in% c("exceedance_rate", "prediction_rate") ~ "Rates",
            characteristic == "prob_separation" ~ "Discrimination"
         )
      ) %>%
      ggplot(aes(x = !!sym(threshold_col), y = value, color = char_label)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      facet_wrap(~char_type, scales = "free_y", ncol = 1) +
      scale_y_continuous(labels = percent) +
      labs(title = "Task Difficulty and Model Response",
           subtitle = "How model behavior changes with experimental threshold",
           x = "Experimental Salinity Threshold",
           y = "Rate / Separation",
           color = "Characteristic") +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill = "lightgray"))
   
   # Plot 5: ROC curve
   p_roc <- ggplot(confusion_metrics,
                   aes(x = false_positive_rate, y = sensitivity)) +
      geom_path(size = 2, color = "#2E8B57", alpha = 0.8) +
      geom_point(aes(size = !!sym(threshold_col)), color = "#2E8B57", alpha = 0.9) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_x_continuous(labels = percent, limits = c(0, 1)) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_size_continuous(name = "Salinity\nThreshold") +
      labs(title = "ROC Curve Across Experimental Thresholds",
           subtitle = "Each point = different experimental threshold, classified at P > 0.5",
           x = "False Positive Rate", 
           y = "True Positive Rate (Sensitivity)") +
      theme_bw(base_size = 12) +
      theme(legend.position = "right")
   
   return(list(
      key_metrics = p_key_metrics,
      probability_behavior = p_prob_behavior,
      calibration = p_calibration,
      task_difficulty = p_task_difficulty,
      roc_curve = p_roc
   ))
}

# ============================================================================
# CONVENIENCE FUNCTIONS
# ============================================================================

#' Quick analysis using natural probability thresholds
analyze_natural_threshold_performance <- function(data, distribution_name = NULL) {
   
   # Filter to single distribution if specified
   if (!is.null(distribution_name)) {
      if ("distribution_family" %in% names(data)) {
         data <- data %>% filter(distribution_family == distribution_name)
         cat("Analyzing distribution:", distribution_name, "\n")
      }
   }
   
   analyze_probability_threshold_performance(
      data = data,
      actual_col = "Salinity",
      predicted_col = "exceedance_probability", 
      threshold_col = "salinity_threshold"
   )
}
