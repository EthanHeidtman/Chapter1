
# Volume Error for extreme events
volume_error <- function(observed, predicted, threshold) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   # Focus on values above threshold
   exceed_idx <- obs_clean > threshold
   
   if (sum(exceed_idx) == 0) return(NA)
   
   obs_volume <- sum(obs_clean[exceed_idx])
   pred_volume <- sum(pred_clean[exceed_idx])
   
   volume_error <- (pred_volume - obs_volume) / obs_volume * 100
   return(volume_error)
}
