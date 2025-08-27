# ============================================================================
# 1. OVERALL PERFORMANCE PLOTS
# ============================================================================

generate_performance_plots <- function(pred_df, model_name) {
   library(ggplot2)
   
   # A. Observed vs Predicted Scatter Plot
   p1 <- ggplot(pred_df, aes(x = Observed, y = Predicted)) +
      geom_point(aes(color = is_high), alpha = 0.6, size = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red2"),
                         labels = c("Normal", "High Salinity"),
                         name = "Event Type") +
      labs(title = paste(model_name, "- Observed vs Predicted"),
           subtitle = "Red line = perfect prediction, Blue line = actual relationship",
           x = "Observed Salinity", y = "Predicted Salinity") +
      theme_minimal() +
      theme(legend.position = "bottom")
   
   # B. Time Series Plot with Confidence Intervals
   p2 <- ggplot(pred_df, aes(x = DateTime)) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.3, fill = "lightblue") +
      geom_line(aes(y = Predicted), color = "blue", size = 0.8, alpha = 0.8) +
      geom_point(aes(y = Observed, color = is_high), size = 0.8, alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red2"),
                         labels = c("Normal", "High Salinity"),
                         name = "Observed") +
      labs(title = paste(model_name, "- Time Series Comparison"),
           subtitle = "Blue ribbon = 95% confidence interval, Blue line = predictions",
           x = "Date", y = "Salinity") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
   
   # C. Performance Metrics by Salinity Range
   pred_df$salinity_bin <- cut(pred_df$Observed, 
                               breaks = c(0, 0.1, 0.2, 0.5, 1.0, max(pred_df$Observed, na.rm = TRUE)),
                               labels = c("Very Low\n(0-0.1)", "Low\n(0.1-0.2)", 
                                          "Moderate\n(0.2-0.5)", "High\n(0.5-1.0)", "Very High\n(>1.0)"))
   
   bin_stats <- pred_df %>%
      group_by(salinity_bin) %>%
      summarise(
         n = n(),
         mae = mean(abs(Observed - Predicted), na.rm = TRUE),
         rmse = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
         r2 = cor(Observed, Predicted, use = "complete.obs")^2,
         .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(mae, rmse, r2), names_to = "metric", values_to = "value")
   
   p3 <- ggplot(bin_stats, aes(x = salinity_bin, y = value, fill = metric)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_text(aes(label = round(value, 3)), position = position_dodge(width = 0.9), 
                vjust = -0.3, size = 3) +
      facet_wrap(~metric, scales = "free_y", labeller = labeller(metric = c(mae = "MAE", rmse = "RMSE", r2 = "R²"))) +
      scale_fill_viridis_d(name = "Metric") +
      labs(title = paste(model_name, "- Performance by Salinity Range"),
           x = "Salinity Range", y = "Metric Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
   
   return(list(scatter = p1, timeseries = p2, performance_bins = p3))
}
