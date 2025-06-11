# Function to compute composite model score
performance_score <- function(model_results, weights = performance_criteria$weights, model_type = 'linear') {
   
   # Is the model valid? Pretty weak constraint
   if (is.null(model_results$high_salinity_count) || 
       is.na(model_results$high_salinity_count) || 
       model_results$high_salinity_count == 0) {
      return(-Inf)  # Still need some high salinity events
   }
   
   # Use safe defaults for missing values
   safe_value <- function(val, default = 0) {
      ifelse(is.null(val) || is.na(val), default, val)
   }
   
   # Detection capability score (0-1, higher is better)
   hit_rate_val <- safe_value(model_results$hit_rate, 0)
   csi_val <- safe_value(model_results$critical_success_index, 0)
   far_val <- safe_value(model_results$false_alarm_rate, 0.5)
   
   detection_score <- pmax(0, pmin(1, 
                             0.4 * hit_rate_val +
                             0.3 * csi_val +
                             0.2 * (1 - far_val) +
                             0.1 * pmax(0, hit_rate_val - far_val)  # TSS
   ))
   
   # High salinity accuracy score (0-1, higher is better)
   rmse_val <- safe_value(model_results$high_salinity_rmse, 1)
   r2_val <- safe_value(model_results$high_salinity_r2, 0)
   bias_val <- safe_value(model_results$high_salinity_bias, 0)
   
   # Convert RMSE to 0-1 score (lower RMSE = higher score)
   rmse_score <- 1 / (1 + rmse_val * 2)  # Less harsh penalty
   r2_score <- pmax(0, r2_val)
   bias_score <- 1 / (1 + abs(bias_val) * 5)  # Less harsh penalty
   
   accuracy_score <- pmax(0, pmin(1, 0.4 * rmse_score + 0.4 * r2_score + 0.2 * bias_score))
   
   # Reliability score (0-1, higher is better)
   vol_error_val <- safe_value(model_results$volume_error, 0)
   reliability_score <- pmax(0, pmin(1, 0.6 * (1 - far_val) + 0.4 * (1 / (1 + vol_error_val))))
   
   # Overall performance score (0-1, higher is better)
   overall_r2_val <- safe_value(model_results$overall_r2, 0)
   overall_rmse_val <- safe_value(model_results$overall_rmse, 1)
   
   overall_score <- pmax(0, pmin(1, 0.6 * overall_r2_val + 0.4 * (1 / (1 + overall_rmse_val))))
   
   # Model stability score
   nse_val <- safe_value(model_results$skill_metrics$nash_sutcliffe, 0)
   skill_val <- safe_value(model_results$skill_metrics$skill_score, 0)
   
   stability_score <- pmax(0, pmin(1, 0.6 * pmax(0, nse_val) + 0.4 * pmax(0, skill_val)))
   
   # Parsimony score 
   # For linear models, use number of coefficients
   if (model_type == "linear") {
      # Estimate complexity from model object if available
      # Simple heuristic: penalize based on typical predictor counts
      if (!is.null(model_results$n_predictors)) {
         n_pred <- model_results$n_predictors
      } else {
         n_pred <- 2  
      }
      
      # Score decreases as predictors increase (diminishing returns)
      # Score ranges from ~0.3 (15+ predictors) to 1.0 (1 predictor)
      parsimony_score <- 1 / (1 + (n_pred - 1) * 0.1)
      
   } else if (model_type == "gam") {
      # For GAM models, consider both number of terms and smoothing complexity
      if (!is.null(model_results$edf_sum)) {
         edf_sum <- model_results$edf_sum  # Effective degrees of freedom
      } else {
         edf_sum <- 10  # Default assumption for GAM
      }
      
      # More severe penalty for GAMs due to smoothing terms
      parsimony_score <- 1 / (1 + (edf_sum - 1) * 0.05)
      
   } else if (model_type == "threshold") {
      # Threshold models have regime complexity
      if (!is.null(model_results$n_regimes)) {
         n_regimes <- model_results$n_regimes
      } else {
         n_regimes <- 2  # Default assumption
      }
      
      # Penalty for multiple regimes plus parameters per regime
      complexity <- n_regimes * 3  # Assume ~3 parameters per regime
      parsimony_score <- 1 / (1 + (complexity - 3) * 0.08)
      
   } else {
      # Default for unknown model types
      parsimony_score <- 0.7
   }
   
   # Ensure between 0.1 and 1.0
   parsimony_score <- pmax(0.1, pmin(1.0, parsimony_score))
   
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