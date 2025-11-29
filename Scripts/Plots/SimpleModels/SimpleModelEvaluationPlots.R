# =============================================================================
# Script Name:    SimpleModelEvaluationPlots.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-11-25
# Last Updated:   2025-11-25
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

plot_fold_performance <- function(fold_metrics, metric = "rmse") {
   
   metric_labels <- list(
      rmse = "RMSE",
      rsq = "R²",
      mae = "MAE"
   )
   
   fold_metrics %>%
      filter(.metric == metric) %>%
      mutate(fold_num = as.numeric(gsub("Fold", "", id))) %>%
      ggplot(aes(x = fold_num, y = .estimate, color = model, group = model)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      labs(
         title = paste(metric_labels[[metric]], "Evolution Across Expanding Window Folds"),
         x = "Fold Number",
         y = metric_labels[[metric]],
         color = "Model"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         legend.position = "bottom",
         plot.title = element_text(face = "bold", size = 14)
      )
}

plot_cv_summary <- function(cv_summary, metric = "rmse") {
   
   metric_labels <- list(
      rmse = "RMSE",
      rsq = "R²",
      mae = "MAE"
   )
   
   cv_summary %>%
      filter(.metric == metric) %>%
      ggplot(aes(x = reorder(model, mean), y = mean, fill = model)) +
      geom_col(alpha = 0.7, width = 0.6) +
      geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                    width = 0.2, linewidth = 1) +
      geom_text(aes(label = round(mean, 3)), vjust = -0.5, nudge_y = 0.02, 
                fontface = "bold") +
      labs(
         title = paste("Average CV", metric_labels[[metric]], "(±SE) Across Models"),
         x = "Model",
         y = paste("Mean", metric_labels[[metric]]),
         fill = "Model"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         legend.position = "none",
         plot.title = element_text(face = "bold", size = 14),
         axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
}

plot_obs_pred <- function(data, 
                          start_date = NULL, 
                          end_date = NULL, 
                          models = c("Elastic", "Lasso", "Ridge", 'RF', 'GAM'),
                          show_metrics = TRUE,
                          alpha = 0.3,
                          point_size = 0.5) {
   
   # Filter by date if specified
   plot_data <- data
   if (!is.null(start_date) & !is.null(end_date)) {
      plot_data <- plot_data %>%
         filter(DateTime >= as.POSIXct(start_date), 
                DateTime <= as.POSIXct(end_date))
   }
   
   # Reshape for plotting
   plot_data_long <- plot_data %>%
      select(DateTime, Salinity, Elastic, Lasso, Ridge, RF, GAM) %>%
      pivot_longer(cols = c(Elastic, Lasso, Ridge, RF, GAM),
                   names_to = "model", values_to = "predicted") %>%
      mutate(model = case_when(
         model == "Elastic" ~ "Elastic",
         model == "Lasso" ~ "Lasso",
         model == "Ridge" ~ "Ridge",
         model == 'RF' ~ 'RF',
         model == 'GAM' ~ 'GAM'
      )) %>%
      filter(model %in% models)
   
   # Calculate metrics
   metrics <- plot_data_long %>%
      group_by(model) %>%
      summarize(
         rmse = sqrt(mean((Salinity - predicted)^2)),
         rsq = cor(Salinity, predicted)^2,
         n = n(),
         .groups = "drop"
      )
   
   # Base plot
   p <- ggplot(plot_data_long, aes(x = Salinity, y = predicted)) +
      geom_point(alpha = alpha, size = point_size) +
      geom_abline(intercept = 0, slope = 1, color = "red", 
                  linetype = "dashed", linewidth = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
      facet_wrap(~model) +
      labs(
         title = "Observed vs Predicted Salinity",
         subtitle = if (!is.null(start_date)) {
            paste("Period:", start_date, "to", end_date, "|", 
                  format(nrow(plot_data), big.mark = ","), "observations")
         } else {
            paste("Full Time Series |", format(nrow(plot_data), big.mark = ","), "observations")
         },
         x = "Observed Salinity",
         y = "Predicted Salinity"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         strip.text = element_text(face = "bold", size = 11),
         plot.title = element_text(face = "bold", size = 14)
      )
   
   # Add metrics if requested
   if (show_metrics) {
      p <- p + geom_text(
         data = metrics,
         aes(x = -Inf, y = Inf, 
             label = paste0("RMSE: ", round(rmse, 2), 
                            "\nR²: ", round(rsq, 3),
                            "\nn: ", format(n, big.mark = ","))),
         hjust = -0.1, vjust = 1.2, size = 3, fontface = "bold"
      )
   }
   
   return(p)
}

plot_timeseries <- function(data,
                            start_date = NULL,
                            end_date = NULL,
                            models = c("Elastic", "Lasso", "Ridge", 'RF', 'GAM'),
                            show_residuals = FALSE) {
   
   # Filter by date
   plot_data <- data
   if (!is.null(start_date) & !is.null(end_date)) {
      plot_data <- plot_data %>%
         filter(DateTime >= as.POSIXct(start_date), 
                DateTime <= as.POSIXct(end_date))
   }
   
   # Reshape for plotting
   plot_data_long <- plot_data %>%
      select(DateTime, Salinity, Elastic, Lasso, Ridge, RF, GAM) %>%
      pivot_longer(cols = c(Elastic, Lasso, Ridge, RF, GAM),
                   names_to = "model", values_to = "predicted") %>%
      mutate(
         model = case_when(
            model == "Elastic" ~ "Elastic",
            model == "Lasso" ~ "Lasso",
            model == "Ridge" ~ "Ridge",
            model == 'RF' ~ 'RF',
            model == 'GAM' ~ 'GAM'
         ),
         residual = Salinity - predicted
      ) %>%
      filter(model %in% models)
   
   if (!show_residuals) {
      # Main time series plot
      p <- ggplot(plot_data_long, aes(x = DateTime)) +
         geom_line(aes(y = Salinity), color = "black", linewidth = 0.8, alpha = 0.7) +
         geom_line(aes(y = predicted, color = model), linewidth = 0.6, alpha = 0.8) +
         labs(
            title = "Observed vs Predicted Salinity Over Time",
            subtitle = if (!is.null(start_date)) {
               paste("Period:", start_date, "to", end_date)
            } else {
               "Full Time Series"
            },
            x = "Date",
            y = "Salinity",
            color = "Model"
         ) +
         scale_color_manual(
            values = c("Elastic" = "#E41A1C", "Lasso" = "#377EB8", "Ridge" = "#4DAF4A", 'RF' = 'thistle', 'GAM' = 'orange'),
            labels = c("Elastic", "Lasso", "Ridge", 'RF', 'GAM')
         ) +
         theme_minimal(base_size = 12) +
         theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 14)
         )
      
   } else {
      # Residuals plot
      p <- ggplot(plot_data_long, aes(x = DateTime, y = residual, color = model)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
         geom_line(linewidth = 0.5, alpha = 0.7) +
         facet_wrap(~model, ncol = 1) +
         labs(
            title = "Prediction Residuals Over Time",
            subtitle = if (!is.null(start_date)) {
               paste("Period:", start_date, "to", end_date)
            } else {
               "Full Time Series"
            },
            x = "Date",
            y = "Residual (Observed - Predicted)",
            color = "Model"
         ) +
         theme_minimal(base_size = 12) +
         theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", size = 14),
            strip.text = element_text(face = "bold")
         )
   }
   
   return(p)
}

plot_residual_diagnostics <- function(data, model_name = "Elastic") {

   
   plot_data <- data %>%
      mutate(
         predicted = .data[[model_name]],
         residual = Salinity - predicted
      )
   
   # Residuals vs fitted
   p1 <- ggplot(plot_data, aes(x = predicted, y = residual)) +
      geom_point(alpha = 0.3, size = 0.5) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(se = TRUE, color = "blue", linewidth = 0.8) +
      labs(
         title = paste(model_name, "- Residuals vs Fitted"),
         x = "Fitted Values",
         y = "Residuals"
      ) +
      theme_minimal(base_size = 12)
   
   # Q-Q plot
   p2 <- ggplot(plot_data, aes(sample = residual)) +
      stat_qq(alpha = 0.3, size = 0.5) +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
         title = paste(model_name, "- Normal Q-Q Plot"),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles"
      ) +
      theme_minimal(base_size = 12)
   
   # Histogram
   p3 <- ggplot(plot_data, aes(x = residual)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(
         title = paste(model_name, "- Residual Distribution"),
         x = "Residuals",
         y = "Count"
      ) +
      theme_minimal(base_size = 12)
   
   # Return list of plots
   return(list(
      residuals_vs_fitted = p1,
      qq_plot = p2,
      histogram = p3
   ))
}

plot_all_metrics_comparison <- function(cv_summary) {
   
   cv_summary %>%
      mutate(
         .metric = case_when(
            .metric == "rmse" ~ "RMSE",
            .metric == "rsq" ~ "R²",
            .metric == "mae" ~ "MAE",
            TRUE ~ .metric
         )
      ) %>%
      ggplot(aes(x = reorder(model, mean), y = mean, fill = model)) +
      geom_col(alpha = 0.7) +
      geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                    width = 0.2, linewidth = 0.8) +
      geom_text(aes(label = round(mean, 3)), vjust = -0.5, size = 3, fontface = "bold") +
      facet_wrap(~.metric, scales = "free_y") +
      labs(
         title = "Model Performance Comparison Across All Metrics",
         x = "Model",
         y = "Mean Value",
         fill = "Model"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         legend.position = "none",
         plot.title = element_text(face = "bold", size = 14),
         strip.text = element_text(face = "bold", size = 11),
         axis.text.x = element_text(angle = 45, hjust = 1)
      )
}
