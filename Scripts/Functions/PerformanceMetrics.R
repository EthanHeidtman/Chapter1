# Function to calculate weighted RMSE, giving emphasis to higher values
weighted_rmse <- function(observed, predicted) {
   # Weights proportional to observed values
   weights <- observed / mean(observed, na.rm = TRUE)
   
   weighted_squared_errors <- weights * (observed - predicted)^2
   return(sqrt(mean(weighted_squared_errors, na.rm = TRUE)))
}

# Function to calculate metrics for high salinity events only
extreme_event_metrics <- function(observed, predicted, threshold) {
   # Subset to high salinity events
   high_idx <- observed > threshold
   
   if (sum(high_idx) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA))
   }
   
   obs_high <- observed[high_idx]
   pred_high <- predicted[high_idx]
   
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   
   return(list(rmse = rmse, mae = mae, bias = bias))
}

# Function to perform model evaluation
evaluate_model <- function(model, data, threshold) {
   predicted <- predict(model, newdata = data)
   observed <- data$Salinity
   
   # Overall metrics
   overall_rmse <- sqrt(mean((observed - predicted)^2))
   weighted_rmse_val <- weighted_rmse(observed, predicted)
   
   # High salinity metrics
   high_metrics <- extreme_event_metrics(observed, predicted, threshold)
   
   # R-squared for high salinity events
   high_idx <- observed > threshold
   if (sum(high_idx) > 1) {
      high_r2 <- 1 - sum((observed[high_idx] - predicted[high_idx])^2) / 
         sum((observed[high_idx] - mean(observed[high_idx]))^2)
   } else {
      high_r2 <- NA
   }
   
   return(list(
      overall_rmse = overall_rmse,
      weighted_rmse = weighted_rmse_val,
      high_salinity_rmse = high_metrics$rmse,
      high_salinity_mae = high_metrics$mae,
      high_salinity_bias = high_metrics$bias,
      high_salinity_r2 = high_r2
   ))
}