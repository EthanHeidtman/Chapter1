# ============================================================================
# 5. RESIDUAL ANALYSIS
# ============================================================================

generate_residual_plots <- function(pred_df, model_name) {
   
   pred_df$residuals <- pred_df$Observed - pred_df$Predicted
   pred_df$std_residuals <- scale(pred_df$residuals)[,1]
   
   # A. Residuals vs Fitted
   p1 <- ggplot(pred_df, aes(x = Predicted, y = residuals)) +
      geom_point(aes(color = is_high), alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.3) +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red2"),
                         labels = c("Normal", "High Salinity"),
                         name = "Event Type") +
      labs(title = paste(model_name, "- Residuals vs Fitted Values"),
           x = "Fitted Values", y = "Residuals") +
      theme_minimal() +
      theme(legend.position = "bottom")
   
   # B. Standardized Residuals
   p2 <- ggplot(pred_df, aes(x = DateTime, y = std_residuals)) +
      geom_point(aes(color = is_high), alpha = 0.6) +
      geom_hline(yintercept = c(-2, 0, 2), linetype = c("dashed", "solid", "dashed"), 
                 color = c("red", "black", "red")) +
      geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.3) +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red2"),
                         labels = c("Normal", "High Salinity"),
                         name = "Event Type") +
      labs(title = paste(model_name, "- Standardized Residuals Over Time"),
           x = "Date", y = "Standardized Residuals") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
   
   # C. Residual Distribution
   p3 <- ggplot(pred_df, aes(x = residuals)) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.8, color = "darkblue") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = mean(pred_df$residuals, na.rm = TRUE), 
                 linetype = "solid", color = "darkred", size = 1) +
      labs(title = paste(model_name, "- Distribution of Residuals"),
           subtitle = paste("Mean residual:", round(mean(pred_df$residuals, na.rm = TRUE), 4)),
           x = "Residuals", y = "Count") +
      theme_minimal()
   
   return(list(residuals_fitted = p1, residuals_time = p2, residual_dist = p3))
}
