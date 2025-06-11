# ============================================================================
# 3. CORRELATION AND RELATIONSHIP ANALYSIS
# ============================================================================

generate_correlation_plots <- function(pred_df, model_name) {
   
   # A. Correlation Matrix Plot
   cor_data <- pred_df %>% 
      select(Observed, Predicted) %>%
      na.omit()
   
   if (ncol(cor_data) >= 2) {
      cor_matrix <- cor(cor_data)
      
      # Create correlation plot
      p1 <- corrplot(cor_matrix, method = "color", type = "upper", 
                     order = "hclust", tl.cex = 0.8, tl.col = "black",
                     title = paste(model_name, "- Correlation Matrix"))
   } else {
      p1 <- NULL
   }
   
   # B. Prediction Accuracy vs Observed Value
   pred_df$abs_error <- abs(pred_df$Observed - pred_df$Predicted)
   
   p2 <- ggplot(pred_df, aes(x = Observed, y = abs_error)) +
      geom_point(aes(color = is_high), alpha = 0.6) +
      geom_smooth(method = "loess", se = TRUE, color = "red", alpha = 0.3) +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red2"),
                         labels = c("Normal", "High Salinity"),
                         name = "Event Type") +
      labs(title = paste(model_name, "- Prediction Error vs Observed Value"),
           subtitle = "Shows if model struggles with certain salinity ranges",
           x = "Observed Salinity", y = "Absolute Error") +
      theme_minimal() +
      theme(legend.position = "bottom")
   
   # C. Quantile-Quantile Plot
   p3 <- ggplot(pred_df, aes(sample = Predicted - Observed)) +
      stat_qq(color = "steelblue", alpha = 0.7) +
      stat_qq_line(color = "red", linetype = "dashed", size = 1) +
      labs(title = paste(model_name, "- Q-Q Plot of Residuals"),
           subtitle = "Tests if residuals are normally distributed",
           x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
   
   return(list(correlation = p1, error_vs_observed = p2, qq_plot = p3))
}