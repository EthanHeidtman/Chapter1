# Function to calculate detection and reliability metrics
calculate_detection_metrics <- function(observed, predicted, threshold) {
   # Binary classification for detection analysis
   obs_binary <- observed > threshold
   pred_binary <- predicted > threshold
   
   # Confusion matrix components
   true_positive <- sum(obs_binary & pred_binary, na.rm = TRUE)
   false_positive <- sum(!obs_binary & pred_binary, na.rm = TRUE)
   true_negative <- sum(!obs_binary & !pred_binary, na.rm = TRUE)
   false_negative <- sum(obs_binary & !pred_binary, na.rm = TRUE)
   
   # Calculate detection metrics
   hit_rate <- ifelse(sum(obs_binary, na.rm = TRUE) > 0, 
                      true_positive / sum(obs_binary, na.rm = TRUE), 0)
   
   false_alarm_rate <- ifelse(sum(!obs_binary, na.rm = TRUE) > 0,
                              false_positive / sum(!obs_binary, na.rm = TRUE), 0)
   
   # Critical Success Index (CSI) - accounts for both hits and false alarms
   csi <- ifelse((true_positive + false_positive + false_negative) > 0,
                 true_positive / (true_positive + false_positive + false_negative), 0)
   
   # Probability of Detection (POD) - same as hit rate but more formal name
   pod <- hit_rate
   
   # Bias score - ratio of predicted to observed events
   bias_score <- ifelse(sum(obs_binary, na.rm = TRUE) > 0,
                        sum(pred_binary, na.rm = TRUE) / sum(obs_binary, na.rm = TRUE), NA)
   
   # Threat Score (same as CSI)
   threat_score <- csi
   
   # True Skill Statistic (TSS) - POD - FAR
   tss <- pod - false_alarm_rate
   
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
