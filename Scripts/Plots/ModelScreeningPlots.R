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
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

plot_model_performance <- function(data, 
                                   group_var = "distribution_family", 
                                   group_label = "Distribution Family",
                                   prob_threshold = 0.5) {
   
   # Calculate basic confusion matrix metrics
   confusion_metrics <- data %>%
      group_by(!!sym(group_var)) %>%
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
         ),
         
         .groups = "drop"
      )
   
   # Calculate calibration data for each group
   calculate_calibration <- function(group_data) {
      # Bin probabilities
      group_data <- group_data %>%
         mutate(prob_bin = cut(exceedance_probability, 
                               breaks = seq(0, 1, 0.05), 
                               include.lowest = TRUE))
      
      # Calculate observed frequency in each bin
      calibration_data <- group_data %>%
         group_by(prob_bin) %>%
         summarise(
            predicted_prob = mean(exceedance_probability, na.rm = TRUE),
            observed_freq = mean(as.numeric(actual_exceedance), na.rm = TRUE),
            n_obs = n(),
            .groups = "drop"
         ) %>%
         filter(n_obs >= 10)  # Only bins with sufficient observations
      
      return(calibration_data)
   }
   
   # Get calibration data for all groups
   calibration_data <- data %>%
      group_by(!!sym(group_var)) %>%
      group_modify(~calculate_calibration(.x)) %>%
      ungroup()
   
   # Determine factor order depending on grouping variable
   if (group_var == "distribution_family") {
      f1_order <- confusion_metrics %>%
         arrange(desc(f1_score)) %>%
         pull(!!sym(group_var)) %>%
         unique()
      
   } else if (group_var %in% c("salinity_threshold", "window_size")) {
      f1_order <- confusion_metrics %>%
         pull(!!sym(group_var)) %>%
         as.numeric() %>%
         unique() %>%
         sort()
   } else {
      # default = as-is order
      f1_order <- confusion_metrics %>%
         pull(!!sym(group_var)) %>%
         unique()
   }
   
   # Apply factor reordering
   confusion_metrics <- confusion_metrics %>%
      mutate(!!sym(group_var) := factor(!!sym(group_var), levels = f1_order))
   
   # Plot 1: Key metrics
   key_metrics_plot <- function() {
      key_metrics <- confusion_metrics %>%
         mutate(!!sym(group_var) := factor(!!sym(group_var), levels = f1_order)) %>%
         select(all_of(group_var), sensitivity, precision, f1_score, false_positive_rate, false_negative_rate) %>%
         pivot_longer(cols = c(sensitivity, precision, f1_score, false_positive_rate, false_negative_rate),
                      names_to = "metric", values_to = "value") %>%
           mutate(metric_label = case_when(
              metric == 'false_positive_rate' ~ 'False Positive Rate',
              metric == 'false_negative_rate' ~ 'False Negative Rate',
              metric == 'sensitivity' ~ 'Recall',
              metric == 'precision' ~ 'Precision',
              metric == "f1_score" ~ "F1 Score"
           )
          ) %>%
         mutate(metric_label = factor(metric_label,
                                      levels = c('False Positive Rate', 'False Negative Rate', 'Recall', 'Precision', 'F1 Score')))
      
      ggplot(key_metrics, 
             aes(x = !!sym(group_var), y = value, fill = metric_label)) +
         geom_col(position = position_dodge(width = 0.8), width = 0.7) +
         geom_text(aes(label = scales::percent(value, accuracy = 0.1)),
                   position = position_dodge(width = 0.8),
                   vjust = -0.3, size = 2.5, fontface = "bold") +
         scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
         scale_fill_manual(
            name = 'Metric',
            values = c("False Positive Rate" = "darkgreen",
                       'False Negative Rate' = 'darkgrey',
                       "Recall" = "steelblue",
                       "Precision" = "darkorange", 
                       "F1 Score" = "purple")
         ) +
         labs(title = paste("Classification Performance by", group_label),
              subtitle = paste("Confusion matrix metrics at", prob_threshold, "probability threshold"),
              x = group_label, y = "Performance") +
         theme_bw(base_size = 11) +
         theme(legend.position = "bottom",
               strip.background = element_rect(fill = "lightgray"))
   }
   
   # Plot 2: Probabilistic performance metrics
   prob_metrics_plot <- function() {
      prob_metrics <- confusion_metrics %>%
         select(all_of(group_var), brier_score, log_loss) %>%
         pivot_longer(cols = c(brier_score, log_loss),
                      names_to = "metric", values_to = "value") %>%
         mutate(
            metric_label = case_when(
               metric == "brier_score" ~ "Brier Score",
               metric == "log_loss" ~ "Log Loss"
            )
         )
      
      ggplot(prob_metrics, 
             aes(x = reorder(!!sym(group_var), -value), 
                 y = value, fill = metric_label)) +
         geom_col(position = position_dodge(width = 0.8), width = 0.7) +
         geom_text(aes(label = round(value, 3)),
                   position = position_dodge(width = 0.8),
                   vjust = -0.3, size = 2.5, fontface = "bold") +
         facet_wrap(~metric_label, scales = "free_y") +
         scale_fill_manual(
            values = c("Brier Score" = "navy", "Log Loss" = "darkred")
         ) +
         labs(title = paste("Probabilistic Performance by", group_label),
              subtitle = "Lower is better for both metrics",
              x = group_label, y = "Score") +
         theme_bw(base_size = 11) +
         theme(legend.position = "none",
               axis.text.x = element_text(angle = 45, hjust = 1),
               strip.background = element_rect(fill = "lightgray"))
   }
   
   # Plot 3: Calibration plot
   calibration_plot <- function() {
      # Force factor if group_var is salinity_threshold
      data <- calibration_data
      if (group_var == "salinity_threshold") {
         data[[group_var]] <- factor(data[[group_var]])
      }
      
      p <- ggplot(data, aes(x = predicted_prob, y = observed_freq)) +
         geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
         geom_point(aes(color = !!sym(group_var), size = n_obs), alpha = 0.7) +
         geom_smooth(aes(color = !!sym(group_var)), method = "loess", se = FALSE, size = 0.8) +
         scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
         scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
         labs(
            title = "Model Calibration",
            subtitle = "Perfect calibration follows the diagonal line",
            x = "Predicted Probability",
            y = "Observed Frequency",
            color = group_label,
            size = "# Observations"
         ) +
         theme_bw(base_size = 11) +
         theme(legend.position = "bottom")
      
      # Custom palette for distributions
      dist_colors <- c(
         "burr" = "steelblue",
         "gengamma" = "darkgreen",
         "gamma" = "darkgrey",
         "gpd" = "darkorange",
         "loglogistic" = "purple",
         "lognormal" = "brown"
      )
      
      # Always use discrete color scale, with special palette for distributions
      if (group_var == "distribution") {
         p <- p + scale_color_manual(values = dist_colors)
      } else {
         p <- p + scale_color_brewer(palette = "Dark2")  # or another palette for thresholds
      }
      
      return(p)
   }
   
   # Generate all plots
   key_plot <- key_metrics_plot()
   prob_plot <- prob_metrics_plot()
   cal_plot <- calibration_plot()
   
   # Combine plots
   combined_plot <- (key_plot | prob_plot) / cal_plot + 
      plot_layout(heights = c(1, 1))
   
   return(list(
      combined_plot = combined_plot,
      key_metrics_plot = key_plot, 
      prob_metrics_plot = prob_plot,
      calibration_plot = cal_plot,
      metrics_table = confusion_metrics,
      calibration_data = calibration_data
   ))
}

# Function 2: Time Period Analysis Plot (flexible date range)
plot_time_period_analysis <- function(data, 
                                      start_date, 
                                      end_date, 
                                      group_var = "distribution_family",
                                      title_suffix = NULL) {
   
   if (group_var == "salinity_threshold") {
      data[[group_var]] <- factor(data[[group_var]], levels = c(0.2, 0.3, 0.4, 0.6, 0.75, 1.0))
   }
   
   # Filter data for specified time period
   period_data <- data %>%
      filter(DateTime >= as.POSIXct(start_date) & 
                DateTime <= as.POSIXct(end_date))
   
   # Get salinity threshold (assume consistent)
   salinity_threshold <- unique(period_data$salinity_threshold)[1]
   
   # Create labels for distributions
   dist_labels <- c(
      "burr" = "Burr",
      "gamma" = "Gamma", 
      "gengamma" = "Generalized Gamma",
      "gpd" = "Generalized Pareto Distribution",
      "loglogistic" = "Log-Logistic",
      "lognormal" = "Lognormal"
   )
   
   # Generate title
   if (is.null(title_suffix)) {
      title_suffix <- paste("Analysis:", format(as.Date(start_date), "%b %Y"))
   }
   
   # Create the plot
   p <- ggplot(period_data, aes(x = DateTime)) +
      # Exceedance probability
      geom_line(aes(y = exceedance_probability, color = !!sym(group_var)), size = 0.7) +
      
      # Raw salinity (rescaled for secondary axis)
      geom_line(aes(y = Salinity / max(Salinity, na.rm = TRUE)), 
                color = "grey40", linetype = "dashed", alpha = 0.8) +
      
      # True exceedances
      geom_point(
         data = period_data %>% filter(Salinity > 1.0),
         aes(y = Salinity / max(Salinity, na.rm = TRUE)),
         color = "red", size = 1.4
      ) +
      
      facet_wrap(as.formula(paste("~", group_var)), ncol = 2,
                 labeller = labeller(.default = function(x) {
                    ifelse(x %in% names(dist_labels), dist_labels[x], x)
                 })) +
      
      # Properly aligned dual axis
      scale_y_continuous(
         name = "Exceedance Probability",
         limits = c(0, 1),
         sec.axis = sec_axis(~ . * max(period_data$Salinity, na.rm = TRUE), 
                             name = "Salinity (psu)")
      ) +
      
      labs(
         x = "Date",
         title = paste("Time Period", title_suffix, "- Exceedance Probability by Distribution"),
         subtitle = "Red points: observed exceedances. Grey dashed line: observed salinity. Colored lines: predicted exceedance probability"
      ) +
      theme_bw(base_size = 12) +
      theme(
         legend.position = "none",
         panel.grid.minor = element_blank(),
         strip.text = element_text(face = "bold"),
         axis.text.x = element_text(angle = 45, hjust = 1)
      )
   
   return(p)
}

