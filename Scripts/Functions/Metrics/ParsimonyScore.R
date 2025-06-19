# Function to calculate parsimony score depending on model type
calculate_parsimony_score <- function(model_results, model_type) {
   
   if (model_type == "gam" && !is.null(model_results$complexity_metrics)) {
      # For GAM models, consider effective degrees of freedom
      edf_sum <- safe_value(model_results$complexity_metrics$edf_sum, 10)
      n_smooth <- safe_value(model_results$complexity_metrics$n_smooth_terms, 3)
      has_tensor <- safe_value(model_results$complexity_metrics$has_tensor_terms, FALSE)
      
      # Base penalty on effective degrees of freedom
      edf_penalty <- 1 / (1 + (edf_sum - 3) * 0.03)
      
      # Additional penalty for tensor terms (more complex)
      tensor_penalty <- ifelse(has_tensor, 0.9, 1.0)
      
      # Convergence bonus
      converged <- safe_value(model_results$complexity_metrics$converged, TRUE)
      convergence_bonus <- ifelse(converged, 1.0, 0.8)
      
      parsimony_score <- edf_penalty * tensor_penalty * convergence_bonus
      
   } else if (model_type == "linear" && !is.null(model_results$complexity_metrics)) {
      # For linear models, use number of predictors
      n_pred <- safe_value(model_results$complexity_metrics$n_predictors, 3)
      parsimony_score <- 1 / (1 + (n_pred - 1) * 0.08)
      
   } else {
      # Default parsimony score
      parsimony_score <- 0.7
   }
   
   # Ensure score is between 0.1 and 1.0
   return(pmax(0.1, pmin(1.0, parsimony_score)))
}