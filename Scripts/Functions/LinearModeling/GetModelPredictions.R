
# Function to get the predictions made by a specific model
get_model_predictions <- function(model, data, model_type = "linear") {
   tryCatch({
      if (model_type == 'gam') {
         if (inherits(model, 'gam')) {
            # Use predict.gam which handles smooth terms properly
            predictions <- predict(model, newdata = data, type = "response")
            
            # Handle different family distributions appropriately
            family_name <- model$family$family
            if (family_name %in% c("Gamma", "quasi")) {
               # Ensure predictions are positive for log-link families
               predictions <- pmax(predictions, 1e-6)
            } else if (family_name == "Tweedie") {
               # Tweedie predictions should be positive
               predictions <- pmax(predictions, 1e-6)
            }
            
            return(predictions)
         } else {
            warning("Model type specified as 'gam' but model is not a GAM object")
            return(rep(NA, nrow(data)))
         }
      } else if (model_type == 'linear') {
         if (inherits(model, "lm")) {
            return(predict(model, newdata = data))
         } else {
            warning("Model type specified as 'linear' but model is not an lm object")
            return(rep(NA, nrow(data)))
         }
      } else if (model_type == 'threshold') {
         # Handle threshold models (which are lists containing multiple models)
         predictions <- numeric(nrow(data))
         
         if ("LowDischargeRegime" %in% names(data)) {
            # Discharge-based threshold model
            low_idx <- data$LowDischargeRegime
            high_idx <- !data$LowDischargeRegime
            
            if (sum(low_idx, na.rm = TRUE) > 0) {
               predictions[low_idx] <- predict(model$low_regime, newdata = data[low_idx, ])
            }
            if (sum(high_idx, na.rm = TRUE) > 0) {
               predictions[high_idx] <- predict(model$high_regime, newdata = data[high_idx, ])
            }
            
         } else if ("IsHighStress" %in% names(data)) {
            # Stress-based threshold model
            normal_idx <- !data$IsHighStress
            stress_idx <- data$IsHighStress
            
            if (sum(normal_idx, na.rm = TRUE) > 0) {
               predictions[normal_idx] <- predict(model$normal_regime, newdata = data[normal_idx, ])
            }
            if (sum(stress_idx, na.rm = TRUE) > 0) {
               predictions[stress_idx] <- predict(model$stress_regime, newdata = data[stress_idx, ])
            }
         }
      } else {
         # If no specified type, try generic predict method
         return(predict(model, newdata = data))
      }
   }, error = function(e) {
      warning(paste("Error in get_model_predictions:", e$message))
      return(rep(NA, nrow(data)))
   })
}