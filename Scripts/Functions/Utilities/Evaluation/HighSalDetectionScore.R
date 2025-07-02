# Function to calculate high salinity detection score
calculate_high_sal_detection <- function(obs, pred, threshold) {
   high_obs <- obs > threshold
   high_pred <- pred > threshold
   
   if (sum(high_obs) == 0) {
      return(list(score = 0, hit_rate = 0, miss_rate = 1, total_events = 0))
   }
   
   # Confusion matrix components
   hits <- sum(high_obs & high_pred)           # True positives
   misses <- sum(high_obs & !high_pred)        # False negatives
   total_events <- sum(high_obs)
   
   # Detection metrics
   hit_rate <- hits / total_events             # Sensitivity/Recall
   miss_rate <- misses / total_events          # 1 - hit_rate
   
   # Detection score is simply hit rate
   detection_score <- hit_rate
   
   return(list(
      score = detection_score,
      hit_rate = hit_rate,
      miss_rate = miss_rate,
      hits = hits,
      misses = misses,
      total_events = total_events
   ))
}