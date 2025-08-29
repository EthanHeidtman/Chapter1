# =============================================================================
# Script Name:    ComparePerformancePlots.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-28
# Last Updated:   2025-08-28
# Description:    Takes the output of the rolling window experiments and calcul-
#                 ates confusion matrix components and generates plots that 
#                 compare performance across the specified group (distribution,
#                 window size, threshold). 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(purrr)

#' Comprehensive confusion matrix analysis for model comparison
#' 
#' @param data Data frame containing actual values, predictions, and grouping variables
#' @param actual_col Column name for actual binary outcomes (string)
#' @param predicted_col Column name for predicted probabilities (string) 
#' @param group_col Column name for grouping variable to compare (string)
#' @param prob_thresholds Vector of probability thresholds for classification (default: c(0.3, 0.5, 0.7))
#' @param group_label Label for grouping variable in plots (default: uses group_col)
#' @return List containing metrics data frame and three ggplot objects
analyze_confusion_performance <- function(data, 
                                          actual_col, 
                                          predicted_col, 
                                          group_col,
                                          prob_thresholds = c(0.3, 0.5, 0.7),
                                          group_label = NULL) {
   
   if (is.null(group_label)) group_label <- group_col
   
   # Validate inputs
   required_cols <- c(actual_col, predicted_col, group_col)
   missing_cols <- setdiff(required_cols, names(data))
   if (length(missing_cols) > 0) {
      stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
   }
   
   # Calculate metrics for all thresholds and groups
   confusion_metrics <- map_dfr(prob_thresholds, function(thresh) {
      data %>%
         group_by(across(all_of(group_col))) %>%
         calculate_confusion_metrics(actual_col, predicted_col, thresh)
   })
   
   # Create visualizations
   plots <- list(
      key_metrics = create_key_metrics_plot(confusion_metrics, group_col, group_label),
      error_rates = create_error_rates_plot(confusion_metrics, group_col, group_label), 
      roc_comparison = create_roc_plot(confusion_metrics, group_col, group_label)
   )
   
   # Create summary table
   summary_table <- create_summary_table(confusion_metrics, group_col)
   
   return(list(
      metrics = confusion_metrics,
      summary = summary_table,
      plots = plots
   ))
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Calculate confusion matrix metrics for a single threshold
calculate_confusion_metrics <- function(data, actual_col, predicted_col, prob_threshold) {
   data %>%
      mutate(
         predicted_binary = !!sym(predicted_col) > prob_threshold,
         tp = !!sym(actual_col) & predicted_binary,
         fp = !!!sym(actual_col) & predicted_binary,
         tn = !!!sym(actual_col) & !predicted_binary,
         fn = !!sym(actual_col) & !predicted_binary
      ) %>%
      summarise(
         tp = sum(tp, na.rm = TRUE),
         fp = sum(fp, na.rm = TRUE),
         tn = sum(tn, na.rm = TRUE),
         fn = sum(fn, na.rm = TRUE),
         .groups = "keep"
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
         threshold = prob_threshold
      )
}

#' Create key performance metrics plot
create_key_metrics_plot <- function(confusion_metrics, group_col, group_label) {
   key_metrics <- confusion_metrics %>%
      select(all_of(group_col), threshold, sensitivity, specificity, precision, f1_score) %>%
      pivot_longer(cols = c(sensitivity, specificity, precision, f1_score),
                   names_to = "metric", values_to = "value") %>%
      mutate(
         metric_label = case_when(
            metric == "sensitivity" ~ "Sensitivity (Recall)",
            metric == "specificity" ~ "Specificity", 
            metric == "precision" ~ "Precision",
            metric == "f1_score" ~ "F1 Score"
         ),
         threshold_label = paste("Threshold:", threshold)
      )
   
   ggplot(key_metrics, 
          aes(x = reorder(!!sym(group_col), value), 
              y = value, fill = metric_label)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = percent(value, accuracy = 0.1)),
                position = position_dodge(width = 0.8),
                hjust = -0.1, size = 2.5, fontface = "bold") +
      facet_wrap(~threshold_label, ncol = 1) +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      scale_fill_manual(
         name = 'Metric',
         values = c("Sensitivity (Recall)" = "steelblue",
                    "Specificity" = "darkorange", 
                    "Precision" = "darkgreen",
                    "F1 Score" = "purple")
      ) +
      labs(title = paste("Classification Performance by", group_label, "and Threshold"),
           subtitle = "Key confusion matrix metrics across probability thresholds",
           x = group_label, y = "Performance") +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill = "lightgray"))
}

#' Create error rates plot
create_error_rates_plot <- function(confusion_metrics, group_col, group_label) {
   error_metrics <- confusion_metrics %>%
      select(all_of(group_col), threshold, false_positive_rate, false_negative_rate) %>%
      pivot_longer(cols = c(false_positive_rate, false_negative_rate),
                   names_to = "error_type", values_to = "value") %>%
      mutate(
         error_label = case_when(
            error_type == "false_positive_rate" ~ "False Positive Rate",
            error_type == "false_negative_rate" ~ "False Negative Rate"
         ),
         threshold_label = paste("Threshold:", threshold)
      )
   
   ggplot(error_metrics, 
          aes(x = reorder(!!sym(group_col), -value), 
              y = value, fill = error_label)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = percent(value, accuracy = 0.1)),
                position = position_dodge(width = 0.8),
                vjust = -0.3, size = 2.5, fontface = "bold") +
      facet_wrap(~threshold_label, scales = "free_y") +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
      scale_fill_manual(
         name = 'Error Type',
         values = c("False Positive Rate" = "tomato",
                    "False Negative Rate" = "firebrick")
      ) +
      labs(title = paste("Error Rates by", group_label, "and Threshold"),
           subtitle = "Lower is better for both error types",
           x = group_label, y = "Error Rate") +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "lightgray"))
}

#' Create ROC-style comparison plot
create_roc_plot <- function(confusion_metrics, group_col, group_label) {
   roc_data <- confusion_metrics %>%
      select(all_of(group_col), threshold, sensitivity, false_positive_rate) %>%
      mutate(threshold_label = paste("Prob >", threshold))
   
   ggplot(roc_data, 
          aes(x = false_positive_rate, y = sensitivity, 
              color = !!sym(group_col), shape = factor(threshold))) +
      geom_point(size = 4, alpha = 0.8) +
      geom_line(aes(group = !!sym(group_col)), alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_x_continuous(labels = percent, limits = c(0, 1)) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_shape_manual(name = "Threshold", values = c(16, 17, 15)) +
      labs(title = paste("ROC-Style Comparison Across", group_label),
           subtitle = "Points closer to top-left corner indicate better performance",
           x = "False Positive Rate", 
           y = "Sensitivity (True Positive Rate)",
           color = group_label) +
      theme_bw(base_size = 12) +
      theme(legend.position = "right")
}

#' Create summary table ranking groups by performance
create_summary_table <- function(confusion_metrics, group_col) {
   confusion_metrics %>%
      group_by(across(all_of(group_col))) %>%
      summarise(
         avg_sensitivity = mean(sensitivity, na.rm = TRUE),
         avg_specificity = mean(specificity, na.rm = TRUE),
         avg_precision = mean(precision, na.rm = TRUE),
         avg_f1_score = mean(f1_score, na.rm = TRUE),
         avg_accuracy = mean(accuracy, na.rm = TRUE),
         .groups = "drop"
      ) %>%
      arrange(desc(avg_f1_score))
}

# ============================================================================
# CONVENIENCE WRAPPER FUNCTIONS
# ============================================================================

#' Quick analysis for distribution comparison
compare_distributions <- function(data, 
                                  actual_col = "actual_exceedance",
                                  predicted_col = "exceedance_probability", 
                                  prob_thresholds = c(0.3, 0.5, 0.7)) {
   analyze_confusion_performance(data, actual_col, predicted_col, "distribution_family", 
                                 prob_thresholds, "Distribution")
}

#' Quick analysis for window size comparison  
compare_window_sizes <- function(data,
                                 actual_col = "actual_exceedance",
                                 predicted_col = "exceedance_probability",
                                 prob_thresholds = c(0.3, 0.5, 0.7)) {
   analyze_confusion_performance(data, actual_col, predicted_col, "window_length",
                                 prob_thresholds, "Window Size")
}

#' Quick analysis for threshold comparison
compare_thresholds <- function(data,
                               actual_col = "actual_exceedance", 
                               predicted_col = "exceedance_probability",
                               prob_thresholds = c(0.3, 0.5, 0.7)) {
   analyze_confusion_performance(data, actual_col, predicted_col, "salinity_threshold",
                                 prob_thresholds, "Salinity Threshold")
}
