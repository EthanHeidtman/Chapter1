# =============================================================================
# Script Name:    CompareRarePerformancePlots.R
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

analyze_rare_event_performance <- function(data, 
                                           actual_col = "actual_exceedance",
                                           predicted_col = "exceedance_probability", 
                                           group_col = "distribution_family") {
   
   # 1. Focus on exceedance cases only
   exceedance_performance <- data %>%
      filter(!!sym(actual_col) == TRUE) %>%
      group_by(!!sym(group_col)) %>%
      summarise(
         n_exceedances = n(),
         min_prob = min(!!sym(predicted_col), na.rm = TRUE),
         q25_prob = quantile(!!sym(predicted_col), 0.25, na.rm = TRUE),
         median_prob = median(!!sym(predicted_col), na.rm = TRUE),
         q75_prob = quantile(!!sym(predicted_col), 0.75, na.rm = TRUE),
         max_prob = max(!!sym(predicted_col), na.rm = TRUE),
         mean_prob = mean(!!sym(predicted_col), na.rm = TRUE),
         prob_above_01 = mean(!!sym(predicted_col) > 0.01),
         prob_above_05 = mean(!!sym(predicted_col) > 0.05),
         prob_above_10 = mean(!!sym(predicted_col) > 0.10),
         .groups = "drop"
      ) %>%
      arrange(desc(mean_prob))
   
   # 2. Top percentile analysis
   top_percentile_analysis <- data %>%
      group_by(!!sym(group_col)) %>%
      mutate(
         prob_percentile = percent_rank(!!sym(predicted_col))
      ) %>%
      filter(prob_percentile >= 0.99) %>%  # Top 1%
      summarise(
         n_in_top1pct = n(),
         n_exceedances_in_top1pct = sum(!!sym(actual_col), na.rm = TRUE),
         precision_top1pct = mean(!!sym(actual_col), na.rm = TRUE),
         min_prob_top1pct = min(!!sym(predicted_col), na.rm = TRUE),
         .groups = "drop"
      ) %>%
      arrange(desc(precision_top1pct))
   
   # 3. Precision at different top-k selections
   precision_at_k <- map_dfr(c(50, 100, 200, 500, 1000), function(k) {
      data %>%
         group_by(!!sym(group_col)) %>%
         slice_max(order_by = !!sym(predicted_col), n = k, with_ties = FALSE) %>%
         summarise(
            k = k,
            precision_at_k = mean(!!sym(actual_col), na.rm = TRUE),
            n_caught = sum(!!sym(actual_col), na.rm = TRUE),
            min_prob_in_topk = min(!!sym(predicted_col), na.rm = TRUE),
            .groups = "drop"
         )
   })
   
   # 4. Area Under Precision-Recall Curve approximation
   pr_metrics <- data %>%
      group_by(!!sym(group_col)) %>%
      arrange(desc(!!sym(predicted_col))) %>%
      mutate(
         rank = row_number(),
         cumulative_tp = cumsum(!!sym(actual_col)),
         precision = cumulative_tp / rank,
         recall = cumulative_tp / sum(!!sym(actual_col), na.rm = TRUE)
      ) %>%
      # Sample key points for PR curve
      filter(rank <= 1000 | !!sym(actual_col) == TRUE) %>%
      group_by(!!sym(group_col)) %>%
      summarise(
         # Approximate AUPRC using trapezoid rule on key points
         auprc_approx = sum(diff(c(0, recall)) * head(precision, -1), na.rm = TRUE),
         max_precision = max(precision, na.rm = TRUE),
         precision_at_50pct_recall = precision[which.min(abs(recall - 0.5))][1],
         .groups = "drop"
      ) %>%
      arrange(desc(auprc_approx))
   
   # 5. Visualizations
   
   # A. Exceedance probability distributions
   p_exceedance_probs <- data %>%
      filter(!!sym(actual_col) == TRUE) %>%
      ggplot(aes(x = !!sym(predicted_col), fill = !!sym(group_col))) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      facet_wrap(as.formula(paste("~", group_col)), scales = "free_y") +
      scale_x_log10(labels = scales::percent) +
      labs(title = "Predicted Probabilities for Actual Exceedances",
           subtitle = "How well does each model assign high probabilities to true exceedances?",
           x = "Predicted Probability (log scale)", 
           y = "Count of Exceedances") +
      theme_bw() +
      theme(legend.position = "none")
   
   # B. Precision at top-k
   p_precision_at_k <- precision_at_k %>%
      ggplot(aes(x = k, y = precision_at_k, color = !!sym(group_col))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_log10() +
      scale_y_continuous(labels = percent) +
      labs(title = "Precision at Top-K Predictions",
           subtitle = "If you act on the top K highest probability predictions, what % are true exceedances?",
           x = "K (number of top predictions)", 
           y = "Precision",
           color = str_to_title(str_replace_all(group_col, "_", " "))) +
      theme_bw()
   
   # C. Recall analysis
   recall_analysis <- data %>%
      group_by(!!sym(group_col)) %>%
      arrange(desc(!!sym(predicted_col))) %>%
      mutate(
         rank = row_number(),
         cumulative_recalls = cumsum(!!sym(actual_col)) / sum(!!sym(actual_col))
      ) %>%
      filter(rank <= 2000) %>%  # Focus on top predictions
      ggplot(aes(x = rank, y = cumulative_recalls, color = !!sym(group_col))) +
      geom_line(size = 1) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(title = "Recall vs Number of Top Predictions",
           subtitle = "How many exceedances are caught in the top N predictions?",
           x = "Number of Top Predictions", 
           y = "Recall (% of exceedances caught)",
           color = str_to_title(str_replace_all(group_col, "_", " "))) +
      theme_bw()
   
   return(list(
      exceedance_performance = exceedance_performance,
      top_percentile_analysis = top_percentile_analysis,
      precision_at_k = precision_at_k,
      pr_metrics = pr_metrics,
      plots = list(
         exceedance_probs = p_exceedance_probs,
         precision_at_k = p_precision_at_k,
         recall_curve = recall_analysis
      )
   ))
}