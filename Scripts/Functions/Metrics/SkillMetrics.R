# Function to calculate skill-based metrics
calculate_skill_metrics <- function(observed, predicted, reference_pred = NULL) {
   # If no reference provided, use climatological mean as reference
   if (is.null(reference_pred)) {
      reference_pred <- rep(mean(observed, na.rm = TRUE), length(observed))
   }
   
   # Model skill score relative to reference
   mse_model <- mean((observed - predicted)^2, na.rm = TRUE)
   mse_reference <- mean((observed - reference_pred)^2, na.rm = TRUE)
   
   skill_score <- ifelse(mse_reference == 0, 0, 1 - (mse_model / mse_reference))
   
   # Nash-Sutcliffe Efficiency
   obs_var <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
   nse <- ifelse(obs_var == 0, 0, 1 - (sum((observed - predicted)^2, na.rm = TRUE) / obs_var))
   
   return(list(
      skill_score = skill_score,
      nash_sutcliffe = nse
   ))
}
