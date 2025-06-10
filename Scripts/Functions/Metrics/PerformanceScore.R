# Function to compute composite model score
performance_score <- function(model_results, weights = performance_criteria$weights) {
   
   # More lenient validity check - only require basic functionality
   if (is.null(model_results$model_validity) || 
       is.na(model_results$high_salinity_count) || 
       model_results$high_salinity_count == 0) {
      cat("Debug - Returning -Inf due to basic validity failure\n")
      return(-Inf)
   }
   
   # Detection capability score (0-1, higher is better)
   # Use default values if metrics are NA
   hit_rate_val <- ifelse(is.na(model_results$hit_rate), 0, model_results$hit_rate)
   csi_val <- ifelse(is.na(model_results$critical_success_index), 0, model_results$critical_success_index)
   far_val <- ifelse(is.na(model_results$false_alarm_rate), 0.5, model_results$false_alarm_rate)
   
   detection_score <- (
      0.4 * hit_rate_val +
         0.3 * csi_val +
         0.2 * pmax(0, 1 - far_val) +
         0.1 * pmax(0, hit_rate_val - far_val)  # TSS approximation
   )
   
   # High salinity accuracy score (0-1, higher is better)
   rmse_val <- ifelse(is.na(model_results$high_salinity_rmse), 1, model_results$high_salinity_rmse)
   r2_val <- ifelse(is.na(model_results$high_salinity_r2), 0, model_results$high_salinity_r2)
   bias_val <- ifelse(is.na(model_results$high_salinity_bias), 0, model_results$high_salinity_bias)
   
   high_sal_rmse_score <- 1 / (1 + rmse_val * 5)
   high_sal_r2_score <- pmax(0, r2_val)
   high_sal_bias_score <- 1 / (1 + abs(bias_val) * 10)
   
   accuracy_score <- (0.4 * high_sal_rmse_score + 0.4 * high_sal_r2_score + 0.2 * high_sal_bias_score)
   
   # Reliability score (0-1, higher is better)
   vol_error_val <- ifelse(is.na(model_results$volume_error), 0, model_results$volume_error)
   
   reliability_score <- (
      0.6 * pmax(0, 1 - far_val) +
         0.4 * pmax(0, 1 / (1 + abs(vol_error_val)))
   )
   
   # Overall performance score (0-1, higher is better)
   overall_r2_val <- ifelse(is.na(model_results$overall_r2), 0, model_results$overall_r2)
   overall_rmse_val <- ifelse(is.na(model_results$overall_rmse), 1, model_results$overall_rmse)
   
   overall_score <- (
      0.6 * pmax(0, overall_r2_val) +
         0.4 * (1 / (1 + overall_rmse_val))
   )
   
   # Model stability score (based on skill metrics)
   nse_val <- ifelse(is.null(model_results$skill_metrics$nash_sutcliffe) || 
                        is.na(model_results$skill_metrics$nash_sutcliffe), 0, 
                     model_results$skill_metrics$nash_sutcliffe)
   skill_val <- ifelse(is.null(model_results$skill_metrics$skill_score) || 
                          is.na(model_results$skill_metrics$skill_score), 0, 
                       model_results$skill_metrics$skill_score)
   
   stability_score <- (
      0.6 * pmax(0, nse_val) +
         0.4 * pmax(0, skill_val)
   )
   
   # Parsimony score (estimated complexity penalty)
   parsimony_score <- 0.8  # Placeholder - adjust based on actual model complexity
   
   # Calculate weighted composite score
   composite_score <- (
      weights["high_sal_detection"] * detection_score +
         weights["high_sal_accuracy"] * accuracy_score +
         weights["high_sal_reliability"] * reliability_score +
         weights["overall_performance"] * overall_score +
         weights["model_stability"] * stability_score +
         weights["parsimony"] * parsimony_score
   )
   
   return(as.numeric(composite_score))
}