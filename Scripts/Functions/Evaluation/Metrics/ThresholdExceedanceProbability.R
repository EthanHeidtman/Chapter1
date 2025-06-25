
# Threshold Exceedance Probability (TEP) metrics
threshold_exceedance_metrics <- function(observed, predicted, threshold) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(hit_rate = NA, false_alarm_rate = NA, critical_success_index = NA))
   }
   
   # Binary classification: does it exceed threshold?
   obs_exceed <- obs_clean > threshold
   pred_exceed <- pred_clean > threshold
   
   # Confusion matrix elements
   true_positives <- sum(obs_exceed & pred_exceed)
   false_positives <- sum(!obs_exceed & pred_exceed)
   true_negatives <- sum(!obs_exceed & !pred_exceed)
   false_negatives <- sum(obs_exceed & !pred_exceed)
   
   # Calculate metrics
   hit_rate <- if(sum(obs_exceed) > 0) true_positives / sum(obs_exceed) else NA
   false_alarm_rate <- if(sum(!obs_exceed) > 0) false_positives / sum(!obs_exceed) else NA
   critical_success_index <- true_positives / (true_positives + false_positives + false_negatives)
   
   return(list(
      hit_rate = hit_rate,
      false_alarm_rate = false_alarm_rate,
      critical_success_index = critical_success_index
   ))
}