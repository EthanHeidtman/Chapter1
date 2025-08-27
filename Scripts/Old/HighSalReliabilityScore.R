# Function to calculate combined detection-reliability F-score for high salinity events
calculate_high_sal_reliability <- function(obs, pred, threshold, beta = 2) {
   high_obs <- obs > threshold
   high_pred <- pred > threshold
   
   # Confusion matrix components
   hits <- sum(high_obs & high_pred)           # True positives
   misses <- sum(high_obs & !high_pred)        # False negatives
   false_alarms <- sum(!high_obs & high_pred)  # False positives
   true_negatives <- sum(!high_obs & !high_pred)
   
   # Totals for rate calculations
   total_events <- sum(high_obs)               # Actual high salinity events
   total_predictions <- sum(high_pred)         # Predicted high salinity events
   total_non_events <- sum(!high_obs)          # Actual low salinity events
   
   # Core metrics
   if (total_events > 0) {
      sensitivity <- hits / total_events        # Detection rate: P(predict high | actually high)
   } else {
      sensitivity <- 1  # Perfect if no events to detect
   }
   
   if (total_predictions > 0) {
      precision <- hits / total_predictions     # Reliability: P(actually high | predict high)
   } else {
      precision <- 1  # Perfect if no predictions made
   }
   
   if (total_non_events > 0) {
      false_alarm_rate <- false_alarms / total_non_events  # P(predict high | actually low)
   } else {
      false_alarm_rate <- 0
   }
   
   # F-beta score: combines detection and reliability with configurable weighting
   # beta > 1 emphasizes sensitivity (detection), beta < 1 emphasizes precision (reliability)
   # beta = 1 gives equal weight (standard F1 score)
   if (precision + sensitivity > 0) {
      f_beta_score <- (1 + beta^2) * (precision * sensitivity) / ((beta^2 * precision) + sensitivity)
   } else {
      f_beta_score <- 0  # No detection or reliability
   }
   
   return(list(
      score = f_beta_score,                    # Combined detection-reliability score (0-1)
      sensitivity = sensitivity,               # Detection rate (0-1)
      precision = precision,                   # Reliability rate (0-1)
      false_alarm_rate = false_alarm_rate,     # False alarm rate (0-1)
      beta = beta,                             # Beta parameter used
      hits = hits,                             # Count of correct high predictions
      misses = misses,                         # Count of missed high events
      false_alarms = false_alarms,             # Count of incorrect high predictions
      total_events = total_events,             # Total actual high salinity events
      total_predictions = total_predictions    # Total high salinity predictions
   ))
}