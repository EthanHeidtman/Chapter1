# Function to calculate metrics for high salinity events only
extreme_event_metrics <- function(observed, predicted, threshold) {
   # Remove NA values first
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0,
                  mape = NA, hit_rate = 0, false_alarm_rate = 0.5, 
                  critical_success_index = 0, volume_error = 0))
   }
   
   # Subset to high salinity events for accuracy metrics
   high_idx <- obs_clean > threshold
   high_count <- sum(high_idx)
   
   if (high_count == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0,
                  mape = NA, hit_rate = 0, false_alarm_rate = 0.5,
                  critical_success_index = 0, volume_error = 0))
   }
   
   obs_high <- obs_clean[high_idx]
   pred_high <- pred_clean[high_idx]
   
   # Accuracy metrics for high salinity events only
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   
   # MAPE with protection against zero division
   mape_val <- if(all(obs_high > 0)) {
      mean(abs((obs_high - pred_high) / obs_high)) * 100
   } else {
      NA
   }
   
   # Get detection metrics using existing function
   detection_metrics <- calculate_detection_metrics(obs_clean, pred_clean, threshold)
   
   # Volume error for high salinity events
   if (high_count > 0) {
      obs_volume <- sum(obs_high)
      pred_volume <- sum(pred_high)
      vol_error <- abs(pred_volume - obs_volume) / obs_volume
   } else {
      vol_error <- 0
   }
   
   return(list(
      rmse = rmse, mae = mae, bias = bias, count = high_count,
      mape = mape_val,
      hit_rate = detection_metrics$hit_rate,
      false_alarm_rate = detection_metrics$false_alarm_rate,
      critical_success_index = detection_metrics$critical_success_index,
      volume_error = vol_error
   ))
}