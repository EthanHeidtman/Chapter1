# Function to fit Generalized Additive Models (GAMs)
fit_gam <- function(data, linear_formula, linear_predictors, weights = NULL, strategy = 'baseline', family) {
   
   # Load package for GAM modeling
   require(mgcv)
   
   # Build the GAM formula based on the identified strategy (default = baseline)
   gam_formula <- build_gam_formula(linear_formula, linear_predictors, strategy)
   
   # Fit the model with error handling
   tryCatch({
      
      # Build arguments list
      gam_args <- list(
         formula = gam_formula$formula,   # The formula we built
         data = data,
         family = family                  # The distribution to fit to
      )
      
      # Only include weights if they are not NULL
      if (!is.null(weights)) {
         # Check that weights are numeric and the right length
         stopifnot(is.numeric(weights), length(weights) == nrow(data))
         gam_args$weights <- weights # The weighting scheme we defined
      }
      
      # Fit the model with the arguments we defined
      gam_model <- do.call(mgcv::gam, gam_args)
      
      # Gather the results and the arguments
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
