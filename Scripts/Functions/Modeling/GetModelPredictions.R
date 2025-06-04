
# Function to get the predictions made by a specific model
get_model_predictions <- function(model, data, model_type = "linear") {
   
   if (model_type == "linear" || model_type == "gam") {
      # Standard predict method works for both lm and gam objects
      predictions <- predict(model, newdata = data)
      
   } else if (model_type == "threshold") {
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
   }
}