# Function to calculate metrics for high salinity events only
extreme_event_metrics <- function(observed, predicted, threshold) {
   # Remove NA values first
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0,
                  mape = NA, peak_flow_criteria = NA, 
                  hit_rate = NA, false_alarm_rate = NA, critical_success_index = NA,
                  volume_error = NA))
   }
   
   # Subset to high salinity events for accuracy metrics
   high_idx <- obs_clean > threshold
   
   if (sum(high_idx) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0,
                  mape = NA, peak_flow_criteria = NA,
                  hit_rate = NA, false_alarm_rate = NA, critical_success_index = NA,
                  volume_error = NA))
   }
   
   obs_high <- obs_clean[high_idx]
   pred_high <- pred_clean[high_idx]
   
   # Accuracy metrics for high salinity events only
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   
   # Detection metrics using all data but threshold-based
   mape_val <- mape(obs_high, pred_high)  # Only high salinity values
   pfc_val <- peak_flow_criteria(obs_clean, pred_clean)  # All data for peak detection
   tep_metrics <- threshold_exceedance_metrics(obs_clean, pred_clean, threshold)  # All data for detection
   vol_error <- volume_error(obs_clean, pred_clean, threshold)
   
   return(list(rmse = rmse, mae = mae, bias = bias, count = length(obs_high),
               mape = mape_val, peak_flow_criteria = pfc_val,
               hit_rate = tep_metrics$hit_rate, 
               false_alarm_rate = tep_metrics$false_alarm_rate,
               critical_success_index = tep_metrics$critical_success_index,
               volume_error = vol_error))
}
