# Function to calculate overall performance score
calculate_overall_performance <- function(obs, pred) {
   # Overall error metrics
   rmse <- sqrt(mean((obs - pred)^2))
   mae <- mean(abs(obs - pred))
   bias <- mean(pred - obs)
   
   # MAPE for overall performance
   mape <- mean(abs((obs - pred) / pmax(obs, 0.01))) * 100
   
   # Overall R-squared
   obs_mean <- mean(obs)
   tss <- sum((obs - obs_mean)^2)
   rss <- sum((obs - pred)^2)
   r2 <- 1 - rss/tss
   
   # Nash-Sutcliffe Efficiency (same as R² in this case)
   nse <- r2
   
   # Performance score based on R² (capped at 0)
   performance_score <- max(0, r2)
   
   return(list(
      score = performance_score,
      r2 = r2,
      nse = nse,
      rmse = rmse,
      mae = mae,
      mape = mape,
      bias = bias
   ))
}