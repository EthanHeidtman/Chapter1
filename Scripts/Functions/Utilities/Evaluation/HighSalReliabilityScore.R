# Function to calculate high salinity reliability score
calculate_high_sal_reliability <- function(obs, pred, threshold) {
   high_obs <- obs > threshold
   high_pred <- pred > threshold
   
   # Confusion matrix components
   hits <- sum(high_obs & high_pred)           # True positives
   false_alarms <- sum(!high_obs & high_pred)  # False positives
   total_non_events <- sum(!high_obs)
   total_predictions <- sum(high_pred)
   
   # Reliability metrics
   if (total_non_events > 0) {
      false_alarm_rate <- false_alarms / total_non_events
   } else {
      false_alarm_rate <- 0
   }
   
   if (total_predictions > 0) {
      precision <- hits / total_predictions  # Positive predictive value
   } else {
      precision <- 0
   }
   
   # Critical Success Index (threat score) - accounts for hits, misses, and false alarms
   misses <- sum(high_obs & !high_pred)
   if ((hits + misses + false_alarms) > 0) {
      csi <- hits / (hits + misses + false_alarms)
   } else {
      csi <- 0
   }
   
   # Reliability score combines precision and CSI (both penalize false alarms)
   reliability_score <- 0.6 * precision + 0.4 * csi
   
   return(list(
      score = reliability_score,
      precision = precision,
      false_alarm_rate = false_alarm_rate,
      csi = csi,
      hits = hits,
      false_alarms = false_alarms,
      total_predictions = total_predictions
   ))
}
