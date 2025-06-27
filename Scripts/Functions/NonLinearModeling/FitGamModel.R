# Function to fit Generalized Additive Models (GAMs)
fit_gam <- function(data, linear_formula, linear_predictors, strategy, weight, distribution, 
                    weight_schemes, gam_strategies, distributions, salinity_threshold, stage_num, 
                    strip = TRUE, use_ar = FALSE, ar_order = 1, use_qgam = FALSE, quantile = 0.5,
                    time_var = NULL, group_var = NULL) {
   
   # Load required packages
   require(mgcv, quietly = TRUE)
   if (use_qgam) require(qgam, quietly = TRUE)
   
   model_type <- "gam"
   if (use_ar && use_qgam) {
      model_type <- "qgamm_ar"
   } else if (use_ar) {
      model_type <- "gamm_ar"
   } else if (use_qgam) {
      model_type <- "qgam"
   }
   
   model_id <- paste("stage", stage_num, strategy, weight, distribution, sep = "_")
   
   # Get the specific objects we need
   weights_obj <- weight_schemes[[weight]]
   strategy_obj <- gam_strategies[[strategy]]
   family_obj <- distributions[[distribution]]
   
   tryCatch({
      # Build the GAM formula based on the identified strategy (default = baseline)
      gam_formula <- build_gam_formula(linear_formula, linear_predictors, strategy_obj)
      
      # Prepare data for autoregressive models (ensure proper ordering)
      if (use_ar) {
         # Sort data by time if time_var is provided
         if (!is.null(time_var) && time_var %in% names(data)) {
            data <- data[order(data[[time_var]]), ]
         }
         
         # Reorder weights if they exist and we sorted the data
         if (!is.null(weights_obj) && !is.null(time_var)) {
            # Assuming weights were in the same order as original data
            weights_obj <- weights_obj[order(data[[time_var]])]
         }
      }
      
      # Fit the appropriate model type
      if (use_qgam && !use_ar) {
         # Pure QGAM
         gam_args <- list(
            formula = gam_formula$formula,
            data = data,
            quantile = quantile,
            err = 0.05  # Convergence tolerance
         )
         
         # Only include weights if they are not NULL
         if (!is.null(weights_obj)) {
            # Check that weights are numeric and the right length
            stopifnot(is.numeric(weights_obj), length(weights_obj) == nrow(data))
            gam_args$weights <- weights_obj # The weighting scheme we defined
         }
         
         gam_model <- do.call(qgam::qgam, gam_args)
         
      } else if (use_ar && !use_qgam) {
         # GAMM with autoregressive terms
         # Set up correlation structure
         if (ar_order == 1) {
            if (!is.null(group_var) && group_var %in% names(data)) {
               # AR(1) within groups (e.g., within years)
               cor_structure <- nlme::corAR1(form = as.formula(paste("~", time_var, "|", group_var)))
            } else {
               # Simple AR(1)
               cor_structure <- nlme::corAR1()
            }
         } else {
            # Higher order AR - more complex
            if (!is.null(group_var) && group_var %in% names(data)) {
               cor_structure <- nlme::corARMA(p = ar_order, form = as.formula(paste("~", time_var, "|", group_var)))
            } else {
               cor_structure <- nlme::corARMA(p = ar_order)
            }
         }
         
         gam_args <- list(
            formula = gam_formula$formula,
            data = data,
            family = family_obj,
            correlation = cor_structure
         )
         
         # Add weights if they exist
         if (!is.null(weights_obj)) {
            gam_args$weights <- weights_obj
         }
         gam_model <- do.call(mgcv::gamm, gam_args)
         gam_model <- gam_model$gam # Keep just the GAM part for consistency
         
      } else if (use_ar && use_qgam) {
         # Combined QGAM with AR structure (2 stage approach)
         
         # First, fit a standard GAM with AR to get the correlation structure
         if (ar_order == 1) {
            if (!is.null(group_var) && group_var %in% names(data)) {
               cor_structure <- nlme::corAR1(form = as.formula(paste("~", time_var, "|", group_var)))
            } else {
               cor_structure <- nlme::corAR1()
            }
         } else {
            if (!is.null(group_var) && group_var %in% names(data)) {
               cor_structure <- nlme::corARMA(p = ar_order, form = as.formula(paste("~", time_var, "|", group_var)))
            } else {
               cor_structure <- nlme::corARMA(p = ar_order)
            }
         }
         
         # Fit initial GAMM to get AR parameters
         initial_gamm <- mgcv::gamm(
            formula = gam_formula$formula,
            data = data,
            family = family_obj,
            correlation = cor_structure,
            weights = weights_obj
         )
         
         # Extract AR parameters and create transformed data
         ar_params <- coef(initial_gamm$lme, unconstrained = FALSE)
         
         # For simplicity, we'll use the QGAM with the residuals from the initial fit
         # This is an approximation - a full implementation would be more complex
         
         # Create augmented formula with lagged residuals
         data$ar_residuals <- c(0, residuals(initial_gamm$gam)[-nrow(data)])
         
         # Modify formula to include AR term
         gam_formula_ar <- update(gam_formula$formula, . ~ . + s(ar_residuals, k = 5))
         
         # Fit QGAM with AR approximation
         gam_args <- list(
            formula = gam_formula_ar,
            data = data,
            quantile = quantile,
            err = 0.05
         )
         
         if (!is.null(weights_obj)) {
            gam_args$weights <- weights_obj
         }
         
         gam_model <- do.call(qgam::qgam, gam_args)
         
         # Store AR information for later use
         gam_model$ar_params <- ar_params
         gam_model$ar_order <- ar_order
         
      } else {
         # Standard GAM
         gam_args <- list(
            formula = gam_formula$formula,
            data = data,
            family = family_obj
         )
         
         if (!is.null(weights_obj)) {
            stopifnot(is.numeric(weights_obj), length(weights_obj) == nrow(data))
            gam_args$weights <- weights_obj
         }
         
         gam_model <- do.call(mgcv::gam, gam_args)
      }
      
   
      # Evaluate the model
      eval_result <- evaluate_model(gam_model, data, salinity_threshold, model_type = "gam")
      
      # Remove bulky components, keep only evaluation
      if (strip) {
         gam_model <- strip_gam_model(gam_model, model_type)
      }
      
      # Combine results
      result <- c(eval_result, list(
         model = gam_model,
         formula = gam_formula,
         strategy = strategy,
         weight_scheme = weight,
         distribution = distribution,
         model_type = model_type,
         quantile = if(use_qgam) quantile else NA,
         ar_order = if(use_ar) ar_order else NA
      ))
      result$score <- performance_score(result)
      
      return(list(model_id = model_id, result = result))
      
   }, error = function(e) {
      warning(paste("Model", model_id, "failed:", e$message))
      return(NULL)
   })
   
}


# Helper function to strip models based on type
strip_gam_model <- function(model, model_type) {
   if (model_type %in% c("qgam")) {
      # QGAM-specific stripping
      model$model <- NULL
      model$residuals <- NULL
      model$fitted.values <- NULL
      model$y <- NULL
      model$linear.predictors <- NULL
      model$weights <- NULL
      model$prior.weights <- NULL
      model$call <- NULL
      if (!is.null(model$formula)) {
         environment(model$formula) <- baseenv()
      }
      if (!is.null(model$terms)) {
         environment(model$terms) <- baseenv()
      }
   } else {
      # Standard GAM stripping (your original code)
      model$model <- NULL
      model$residuals <- NULL
      model$fitted.values <- NULL
      model$y <- NULL
      model$linear.predictors <- NULL
      model$weights <- NULL
      model$prior.weights <- NULL
      model$qr <- NULL
      model$call <- NULL
      if (!is.null(model$formula)) {
         environment(model$formula) <- baseenv()
      }
      if (!is.null(model$terms)) {
         environment(model$terms) <- baseenv()
      }
   }
   return(model)
}