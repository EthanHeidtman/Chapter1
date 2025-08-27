# ============================================================================
# 2. HIGH SALINITY EVENT ANALYSIS
# ============================================================================

generate_high_salinity_plots <- function(pred_df, model_name) {
   
   library(ggplot2)
   
   # Filter high salinity events
   high_events <- pred_df %>% filter(is_high == TRUE)
   
   if (nrow(high_events) == 0) {
      warning("No high salinity events found with current threshold")
      return(list())
   }
   
   # A. High Salinity Event Performance
   p1 <- ggplot(high_events, aes(x = Observed, y = Predicted)) +
      geom_point(color = "red2", alpha = 0.7, size = 2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "darkred", alpha = 0.3) +
      labs(title = paste(model_name, "- High Salinity Events Only"),
           subtitle = paste("n =", nrow(high_events), "events above", salinity_threshold),
           x = "Observed Salinity", y = "Predicted Salinity") +
      theme_minimal()
   
   # B. Error Analysis for High Events
   high_events$error <- high_events$Observed - high_events$Predicted
   high_events$abs_error <- abs(high_events$error)
   high_events$percent_error <- (high_events$error / high_events$Observed) * 100
   
   p2 <- ggplot(high_events, aes(x = DateTime, y = error)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(aes(size = abs_error), color = "red2", alpha = 0.7) +
      geom_smooth(se = TRUE, color = "darkred", alpha = 0.3) +
      scale_size_continuous(name = "Absolute\nError", range = c(1, 4)) +
      labs(title = paste(model_name, "- High Salinity Event Errors Over Time"),
           subtitle = "Point size = magnitude of error",
           x = "Date", y = "Prediction Error (Observed - Predicted)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   # C. Distribution of Errors for High Events
   p3 <- ggplot(high_events, aes(x = percent_error)) +
      geom_histogram(bins = 20, fill = "red2", alpha = 0.7, color = "darkred") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
      geom_vline(xintercept = median(high_events$percent_error, na.rm = TRUE), 
                 linetype = "solid", color = "darkred", size = 1) +
      labs(title = paste(model_name, "- Distribution of % Errors for High Salinity Events"),
           subtitle = paste("Median error:", round(median(high_events$percent_error, na.rm = TRUE), 1), "%"),
           x = "Percent Error (%)", y = "Count") +
      theme_minimal()
   
   # D. Worst Predictions Analysis
   worst_predictions <- high_events %>%
      arrange(desc(abs_error)) %>%
      head(10)
   
   p4 <- ggplot(worst_predictions, aes(x = reorder(format(DateTime, "%Y-%m-%d"), abs_error))) +
      geom_col(aes(y = abs_error), fill = "red2", alpha = 0.8) +
      geom_text(aes(y = abs_error, label = paste("Obs:", round(Observed, 3), "\nPred:", round(Predicted, 3))), 
                hjust = -0.1, size = 3) +
      coord_flip() +
      labs(title = paste(model_name, "- Top 10 Worst High Salinity Predictions"),
           x = "Date", y = "Absolute Error") +
      theme_minimal()
   
   return(list(high_scatter = p1, high_errors = p2, error_dist = p3, worst_predictions = p4))
}