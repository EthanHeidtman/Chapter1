# Function to calculate high salinity accuracy score  
calculate_high_sal_accuracy <- function(obs, pred, threshold) {
   high_idx <- obs > threshold
   
   if (sum(high_idx) < 2) {
      return(list(score = 0, r2 = NA, rmse = NA, mae = NA, mape = NA, bias = 0, count = sum(high_idx)))
   }
   
   obs_high <- obs[high_idx]
   pred_high <- pred[high_idx]
   
   # Error metrics for high salinity events
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   
   # MAPE (Mean Absolute Percentage Error) - handle near-zero values
   mape <- mean(abs((obs_high - pred_high) / pmax(obs_high, 0.01))) * 100
   
   # R-squared for high salinity events
   obs_mean <- mean(obs_high)
   tss <- sum((obs_high - obs_mean)^2)
   rss <- sum((obs_high - pred_high)^2)
   r2 <- 1 - rss/tss
   
   # Accuracy score based on R² (capped at 0 for negative R²)
   accuracy_score <- max(0, r2)
   
   return(list(
      score = accuracy_score,
      r2 = r2,
      rmse = rmse,
      mae = mae,
      mape = mape,
      bias = bias,
      count = sum(high_idx)
   ))
}