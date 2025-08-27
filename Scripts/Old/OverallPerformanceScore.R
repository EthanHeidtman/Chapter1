calculate_overall_performance <- function(obs, pred, weights = c(r2 = 0.4, rmse = 0.2, mae = 0.2, bias = 0.2)) {
   # Basic metrics
   rmse <- sqrt(mean((obs - pred)^2))
   mae <- mean(abs(obs - pred))
   bias <- mean(pred - obs)
   obs_mean <- mean(obs)
   r2 <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
   nse <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
   
   # Kling-Gupta Efficiency (KGE)
   cc <- cor(pred, obs)
   alpha <- sd(pred) / sd(obs)
   beta <- mean(pred) / mean(obs)
   kge <- 1 - sqrt((cc - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
   
   # Normalize RMSE and MAE (assuming 0–2 PSU)
   rmse_norm <- 1 - min(rmse / 2, 1)
   mae_norm <- 1 - min(mae / 2, 1)
   bias_penalty <- 1 - min(abs(bias) / 1, 1)  # +/-1 is considered bad
   
   # Cap R² to [0, 1]
   r2_adj <- max(0, min(r2, 1))
   
   # Weighted composite score
   score <- (
      weights["r2"]   * r2_adj +
         weights["rmse"] * rmse_norm +
         weights["mae"]  * mae_norm +
         weights["bias"] * bias_penalty
   )
   
   return(list(
      score = as.numeric(score),
      r2 = r2,
      nse = nse,
      rmse = rmse,
      mae = mae,
      bias = bias,
      kge = kge
   ))
}
