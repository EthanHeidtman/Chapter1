# Function to calculate model complexity based on number and types of terms
calculate_model_complexity <- function(model, model_type) {
   complexity <- list()
   
   if (model_type == "gam" && inherits(model, "gam")) {
      # GAM-specific complexity metrics
      complexity$edf_sum <- sum(model$edf)  # Total effective degrees of freedom
      complexity$edf_individual <- model$edf  # EDF for each smooth term
      complexity$n_smooth_terms <- length(model$smooth)
      complexity$n_linear_terms <- length(model$coefficients) - length(model$smooth)
      complexity$total_parameters <- length(model$coefficients)
      
      # Check for tensor product terms
      smooth_classes <- sapply(model$smooth, function(x) class(x)[1])
      complexity$has_tensor_terms <- any(grepl("tensor", smooth_classes, ignore.case = TRUE))
      
      # Convergence information
      complexity$converged <- model$converged
      complexity$gcv_score <- model$gcv.ubre
      
   } else if (model_type == "linear" && inherits(model, "lm")) {
      # Linear model complexity metrics
      complexity$n_predictors <- length(model$coefficients) - 1  # Exclude intercept
      complexity$total_parameters <- length(model$coefficients)
      complexity$df_residual <- model$df.residual
      
   } else {
      # Generic complexity measures
      if (inherits(model, c("lm", "glm"))) {
         complexity$total_parameters <- length(coef(model))
      } else {
         complexity$total_parameters <- NA
      }
   }
   
   return(complexity)
}