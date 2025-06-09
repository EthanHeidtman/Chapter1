# Function to get detailed diagnostics for the final model
model_diagnostics <- function(final_model, data, salinity_threshold) {
   
   cat("=== DETAILED DIAGNOSTIC ANALYSIS ===\n")
   
   # 1. Model Summary
   cat("\n1. MODEL SUMMARY:\n")
   print(summary(model))
   
   # 2. Get predictions and residuals
   predictions <- predict(model, newdata = data)
   residuals <- data$Salinity - predictions
   
   # 3. Detailed performance metrics
   cat("\n2. DETAILED PERFORMANCE METRICS:\n")
   eval_results <- evaluate_model(model, data, threshold, "linear")
   
   cat(sprintf("Overall RMSE: %.4f\n", eval_results$overall_rmse))
   cat(sprintf("Overall R²: %.4f\n", eval_results$overall_r2))
   cat(sprintf("Low R²: %.4f\n", eval_results$low_r2))
   cat(sprintf("High Salinity RMSE: %.4f\n", eval_results$high_salinity_rmse))
   cat(sprintf("High Salinity R²: %.4f\n", eval_results$high_salinity_r2))
   cat(sprintf("High Salinity Count: %d\n", eval_results$high_salinity_count))
   cat(sprintf("Hit Rate: %.3f\n", eval_results$hit_rate))
   cat(sprintf("False Alarm Rate: %.3f\n", eval_results$false_alarm_rate))
   
   # 4. Residual analysis
   cat("\n3. RESIDUAL ANALYSIS:\n")
   residual_stats <- list(
      mean_residual = mean(residuals, na.rm = TRUE),
      median_residual = median(residuals, na.rm = TRUE),
      residual_sd = sd(residuals, na.rm = TRUE),
      residual_range = range(residuals, na.rm = TRUE)
   )
   
   cat(sprintf("Mean Residual: %.4f\n", residual_stats$mean_residual))
   cat(sprintf("Median Residual: %.4f\n", residual_stats$median_residual))
   cat(sprintf("Residual SD: %.4f\n", residual_stats$residual_sd))
   cat(sprintf("Residual Range: [%.4f, %.4f]\n", 
               residual_stats$residual_range[1], residual_stats$residual_range[2]))
   
   # 5. Variable importance (based on t-statistics)
   cat("\n4. VARIABLE IMPORTANCE (|t-statistics|):\n")
   coef_summary <- summary(model)$coefficients
   t_stats <- abs(coef_summary[, "t value"])
   importance_order <- order(t_stats, decreasing = TRUE)
   
   for (i in importance_order) {
      cat(sprintf("%-30s: |t| = %.3f, p = %.4f\n", 
                  rownames(coef_summary)[i], t_stats[i], coef_summary[i, "Pr(>|t|)"]))
   }
   
   return(list(
      eval_results = eval_results,
      residual_stats = residual_stats,
      variable_importance = t_stats[importance_order],
      predictions = predictions,
      residuals = residuals
   ))
}