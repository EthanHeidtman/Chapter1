calculate_high_sal_accuracy <- function(obs, pred, threshold, weights = c(r2 = 0.4, rmse = 0.2, mae = 0.2, bias = 0.2)) {
   high_idx <- obs > threshold
   
   if (sum(high_idx) < 2) {
      return(list(score = 0, r2 = NA, rmse = NA, mae = NA, bias = 0, kge = NA, count = sum(high_idx)))
   }
   
   obs_high <- obs[high_idx]
   pred_high <- pred[high_idx]
   
   # Metrics
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   obs_mean <- mean(obs_high)
   r2 <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
   nse <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
   
   # Kling-Gupta Efficiency (KGE)
   cc <- cor(pred_high, obs_high)
   alpha <- sd(pred_high) / sd(obs_high)
   beta <- mean(pred_high) / mean(obs_high)
   kge <- 1 - sqrt((cc - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
   
   # Normalize RMSE and MAE (assuming salinity range is 0–2 PSU)
   rmse_norm <- 1 - min(rmse / 2, 1)
   mae_norm <- 1 - min(mae / 2, 1)
   bias_penalty <- 1 - min(abs(bias) / 1, 1)  # assumes bias range ±1 is meaningful
   
   # Cap R² at [0, 1]
   r2_adj <- max(0, min(r2, 1))
   
   # Weighted score
   score <- (
      weights["r2"]   * r2_adj +
      weights["rmse"] * rmse_norm +
      weights["mae"]  * mae_norm +
      weights["bias"] * bias_penalty
   )
   
   return(list(
      score = as.numeric(score),
      r2 = r2,
      rmse = rmse,
      mae = mae,
      bias = bias,
      kge = kge,
      count = sum(high_idx)
   ))
}
