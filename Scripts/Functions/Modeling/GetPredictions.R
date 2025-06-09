
# Function to gather the model predictions with confidence intervals
get_predictions <- function(model, data, model_type = "linear", threshold = salinity_threshold) {
   
   if (model_type == "linear") {
      # Get predictions with standard errors for linear models
      preds <- predict(model, newdata = data, se.fit = TRUE)
      predicted <- preds$fit
      se <- preds$se.fit
      
   } else if (model_type == "gam") {
      # GAM predictions with standard errors
      preds <- predict(model, newdata = data, se.fit = TRUE)
      predicted <- preds$fit
      se <- preds$se.fit
      
   } else if (model_type == "threshold") {
      # Threshold models - get predictions without SE for now
      predicted <- get_model_predictions(model, data, model_type)
      se <- rep(NA, length(predicted))  # Standard errors not easily available for threshold models
   }
   
   # Create prediction dataframe
   pred_df <- data.frame(
      date_time = data$DateTime,
      observed = data$Salinity,
      predicted = predicted,
      lower_ci = if(all(is.na(se))) rep(NA, length(predicted)) else predicted - 1.96 * se,
      upper_ci = if(all(is.na(se))) rep(NA, length(predicted)) else predicted + 1.96 * se,
      is_high = data$Salinity > threshold,
      stringsAsFactors = FALSE
   )
   
   return(pred_df)
}