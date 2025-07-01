get_predictions <- function(model, data, model_type = "linear", threshold = salinity_threshold) {
   tryCatch({
      if (model_type == 'gam') {
         if (inherits(model, 'gam')) {
            # For GAM models, handle confidence intervals properly
            family_name <- model$family$family
            
            if (family_name %in% c("Gamma", "quasi") && model$family$link == "log") {
               # For log-link families, get predictions on link scale first
               preds_link <- predict(model, newdata = data, type = "link", se.fit = TRUE)
               link_pred <- preds_link$fit
               link_se <- preds_link$se.fit
               
               # Transform to response scale
               predicted <- exp(link_pred)
               predicted <- pmax(predicted, 1e-6)  # Ensure positive
               
               # Calculate confidence intervals on link scale then transform
               lower_ci <- if(all(is.na(link_se))) rep(NA, length(predicted)) else exp(link_pred - 1.96 * link_se)
               upper_ci <- if(all(is.na(link_se))) rep(NA, length(predicted)) else exp(link_pred + 1.96 * link_se)
               
            } else {
               # For other families (Tweedie, gaussian, etc.), use response scale directly
               preds <- predict(model, newdata = data, type = "response", se.fit = TRUE)
               predicted <- preds$fit
               se <- preds$se.fit
               
               # Ensure positive predictions for appropriate families
               if (family_name == "Tweedie") {
                  predicted <- pmax(predicted, 1e-6)
               }
               
               # Standard confidence intervals
               lower_ci <- if(all(is.na(se))) rep(NA, length(predicted)) else predicted - 1.96 * se
               upper_ci <- if(all(is.na(se))) rep(NA, length(predicted)) else predicted + 1.96 * se
            }
            
            pred_df <- data.frame(
               DateTime = data$DateTime,
               Observed = data$Salinity,
               Predicted = predicted,
               lower_ci = lower_ci,
               upper_ci = upper_ci,
               is_high = data$Salinity > threshold,
               stringsAsFactors = FALSE
            )
            
            return(pred_df)
         } else {
            warning("Model type specified as 'gam' but model is not a GAM object")
            return(NULL)
         }
      } else if (model_type == 'linear') {
         if (inherits(model, "lm")) {
            preds <- predict(model, newdata = data, se.fit = TRUE)
            predicted <- preds$fit
            se <- preds$se.fit
            
            pred_df <- data.frame(
               DateTime = data$DateTime,
               Observed = data$Salinity,
               Predicted = predicted,
               lower_ci = if(all(is.na(se))) rep(NA, length(predicted)) else predicted - 1.96 * se,
               upper_ci = if(all(is.na(se))) rep(NA, length(predicted)) else predicted + 1.96 * se,
               is_high = data$Salinity > threshold,
               stringsAsFactors = FALSE
            )
            
            return(pred_df)
         } else {
            warning("Model type specified as 'linear' but model is not an lm object")
            return(rep(NA, nrow(data)))
         }
      }
   }, error = function(e) {
      warning(paste("Error in get_model_predictions:", e$message))
      return(rep(NA, nrow(data)))
   })
}
