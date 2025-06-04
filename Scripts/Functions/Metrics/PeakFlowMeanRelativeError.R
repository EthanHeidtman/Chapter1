
# Peak Flow Criteria (PFC)
peak_flow_criteria <- function(observed, predicted, threshold_percentile = 95) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   # Define threshold as percentile of observed data
   threshold <- quantile(obs_clean, threshold_percentile/100, na.rm = TRUE)
   
   # Find peak events
   peak_idx <- obs_clean >= threshold
   
   if (sum(peak_idx) == 0) return(NA)
   
   obs_peaks <- obs_clean[peak_idx]
   pred_peaks <- pred_clean[peak_idx]
   
   # Calculate relative error for peaks
   relative_errors <- abs(obs_peaks - pred_peaks) / obs_peaks
   return(mean(relative_errors) * 100)
}