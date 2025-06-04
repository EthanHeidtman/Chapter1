# Function to calculate composite performance score
performance_score <- function(model_results, weights = performance_criteria$weights) {
   
   # Handle missing values
   if (is.na(model_results$high_salinity_rmse) || model_results$high_salinity_count < performance_criteria$thresholds$min_high_sal_count) {
      return(-Inf)  # Penalize models that can't predict high salinity
   }
   
   # Calculate individual components (normalize to 0-1 scale where higher is better)
   high_sal_rmse_score <- 1 / (1 + model_results$high_salinity_rmse)  # Lower RMSE is better
   high_sal_r2_score <- pmax(0, model_results$high_salinity_r2)       # Higher R2 is better
   overall_r2_score <- pmax(0, model_results$overall_r2)              # Higher R2 is better
   overall_rmse_score <- 1 / (1 + model_results$overall_rmse)         # Lower RMSE is better
   
   # Parsimony score (fewer predictors is better, estimated from model complexity)
   n_predictors <- length(attr(terms(model_results$model), "term.labels"))
   parsimony_score <- exp(-n_predictors / 10)  # Exponential penalty for complexity
   
   # Weighted composite score
   composite_score <- (
      weights["high_sal_rmse"] * high_sal_rmse_score +
         weights["high_sal_r2"] * high_sal_r2_score +
         weights["overall_r2"] * overall_r2_score +
         weights["overall_rmse"] * overall_rmse_score +
         weights["parsimony"] * parsimony_score
   )
   
   return(as.numeric(composite_score))
}
