# Function to calculate detection and reliability metrics
calculate_detection_metrics <- function(observed, predicted, threshold) {
   # Handle edge case of no valid data
   valid_idx <- !is.na(observed) & !is.na(predicted)
   if (sum(valid_idx) == 0) {
      return(list(
         hit_rate = 0, false_alarm_rate = 0.5, critical_success_index = 0,
         probability_of_detection = 0, bias_score = NA, threat_score = 0,
         true_skill_statistic = 0, true_positive = 0, false_positive = 0,
         true_negative = 0, false_negative = 0
      ))
   }
   
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   # Binary classification for detection analysis
   obs_binary <- obs_clean > threshold
   pred_binary <- pred_clean > threshold
   
   # Confusion matrix components
   true_positive <- sum(obs_binary & pred_binary, na.rm = TRUE)
   false_positive <- sum(!obs_binary & pred_binary, na.rm = TRUE)
   true_negative <- sum(!obs_binary & !pred_binary, na.rm = TRUE)
   false_negative <- sum(obs_binary & !pred_binary, na.rm = TRUE)
   
   # Calculate detection metrics with safe divisions
   hit_rate <- ifelse(sum(obs_binary) > 0, 
                      true_positive / sum(obs_binary), 0)
   
   false_alarm_rate <- ifelse(sum(!obs_binary) > 0,
                              false_positive / sum(!obs_binary), 0)
   
   # Critical Success Index (CSI)
   csi <- ifelse((true_positive + false_positive + false_negative) > 0,
                 true_positive / (true_positive + false_positive + false_negative), 0)
   
   pod <- hit_rate  # Probability of Detection same as hit rate
   
   # Bias score
   bias_score <- ifelse(sum(obs_binary) > 0,
                        sum(pred_binary) / sum(obs_binary), NA)
   
   threat_score <- csi  # Same as CSI
   tss <- pod - false_alarm_rate  # True Skill Statistic
   
   return(list(
      hit_rate = hit_rate,
      false_alarm_rate = false_alarm_rate,
      critical_success_index = csi,
      probability_of_detection = pod,
      bias_score = bias_score,
      threat_score = threat_score,
      true_skill_statistic = tss,
      true_positive = true_positive,
      false_positive = false_positive,
      true_negative = true_negative,
      false_negative = false_negative
   ))
}
