# Function to fit Generalized Additive Models (GAMs)

fit_gam <- function(data, linear_formula, linear_predictors, weights = NULL, strategy = 'baseline', family = gaussian) {
   
   # Load package for GAM modeling
   require(mgcv)
   
   # Build the GAM formula based on the identified strategy (default = baseline)
   gam_formula <- build_gam_formula(linear_formula, linear_predictors, strategy)
   
   # Fit the model with error handling
   tryCatch({
      if(is.null(weights)) {
         gam_model <- mgcv::gam(gam_formula$formula, data = data, family = family) # Family = distribution, weights = weighting scheme
      } else {
         gam_model <- mgcv::gam(gam_formula$formula, data = data, family = family, weights = weights)
      }
      
      return(list(
         model = gam_model,
         formula = gam_formula,
         strategy = strategy,
         family = family,
         weights_used = !is.null(weights)
      ))
      
   }, error = function(e) {
      warning(sprintf("GAM fitting failed: %s", e$message))
      return(list(
         model = NULL,
         formula = gam_formula,
         strategy = strategy,
         family = family,
         weights_used = !is.null(weights),
         error = e$message
      ))
   })
   
}
