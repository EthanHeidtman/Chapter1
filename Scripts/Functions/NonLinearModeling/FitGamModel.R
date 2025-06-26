# Function to fit Generalized Additive Models (GAMs)
fit_gam <- function(data, linear_formula, linear_predictors, strategy, weight, distribution, 
                    weight_schemes, gam_strategies, distributions, salinity_threshold, stage_num, strip = TRUE) {
   
   # Load package for GAM modeling
   require(mgcv, quietly = TRUE)
   
   model_id <- paste("stage", stage_num, strategy, weight, distribution, sep = "_")
   
   # Get the specific objects we need
   weights_obj <- weight_schemes[[weight]]
   strategy_obj <- gam_strategies[[strategy]]
   family_obj <- distributions[[distribution]]
   
   tryCatch({
      # Build the GAM formula based on the identified strategy (default = baseline)
      gam_formula <- build_gam_formula(linear_formula, linear_predictors, strategy_obj)
      
      # Build arguments list for GAM
      gam_args <- list(
         formula = gam_formula$formula,   # The formula we built
         data = data,
         family = family_obj              # The distribution to fit to
      )
      
      # Only include weights if they are not NULL
      if (!is.null(weights_obj)) {
         # Check that weights are numeric and the right length
         stopifnot(is.numeric(weights_obj), length(weights_obj) == nrow(data))
         gam_args$weights <- weights_obj # The weighting scheme we defined
      }
      
      # Fit the model with the arguments we defined
      gam_model <- do.call(mgcv::gam, gam_args)
      
      # Evaluate the model
      eval_result <- evaluate_model(gam_model, data, salinity_threshold, model_type = "gam")
      
      # Remove bulky components, keep only evaluation
      if (strip) {
         gam_model$model <- NULL
         gam_model$residuals <- NULL
         gam_model$fitted.values <- NULL
         gam_model$y <- NULL
         gam_model$linear.predictors <- NULL
         gam_model$weights <- NULL
         gam_model$prior.weights <- NULL
         gam_model$qr <- NULL
         gam_model$call <- NULL
         environment(gam_model$formula) <- baseenv() # Most important, cleans the environment to save space
         if (!is.null(gam_model$terms)) {
            environment(gam_model$terms) <- baseenv()
         }
      }
      
      # Combine results
      result <- c(eval_result, list(
         model = gam_model,
         formula = gam_formula,
         strategy = strategy,
         weight_scheme = weight,
         distribution = distribution
      ))
      result$score <- performance_score(result)
      
      return(list(model_id = model_id, result = result))
      
   }, error = function(e) {
      warning(paste("Model", model_id, "failed:", e$message))
      return(NULL)
   })
   
}