# Mean Absolute Percentage Error (MAPE) - sensitive to extreme values
mape <- function(observed, predicted) {
   valid_idx <- !is.na(observed) & !is.na(predicted) & observed != 0
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   return(mean(abs((obs_clean - pred_clean) / obs_clean)) * 100)
}