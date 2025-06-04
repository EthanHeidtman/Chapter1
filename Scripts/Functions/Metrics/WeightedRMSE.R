
# Function to calculate weighted RMSE, giving emphasis to higher values
weighted_rmse <- function(observed, predicted) {
   # Remove NA values
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   # Weights proportional to observed values
   weights <- obs_clean / mean(obs_clean, na.rm = TRUE)
   
   weighted_squared_errors <- weights * (obs_clean - pred_clean)^2
   return(sqrt(mean(weighted_squared_errors, na.rm = TRUE)))
}