
# Function to perform model evaluation
evaluate_model <- function(model, data, threshold, model_type = "linear") {
   
   # Get predictions based on model type
   predicted <- get_model_predictions(model, data, model_type)
   observed <- data$Salinity
   
   # Remove rows where either observed or predicted is NA
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(
         overall_rmse = NA,
         weighted_rmse = NA,
         overall_r2 = NA,
         high_salinity_rmse = NA,
         high_salinity_mae = NA,
         high_salinity_bias = NA,
         high_salinity_r2 = NA,
         high_salinity_count = 0,
         high_salinity_mape = NA,
         peak_flow_criteria = NA,
         hit_rate = NA,
         false_alarm_rate = NA,
         critical_success_index = NA,
         volume_error = NA,
         total_observations = 0
      ))
   }
   
   # Overall metrics
   overall_rmse <- sqrt(mean((obs_clean - pred_clean)^2))
   weighted_rmse_val <- weighted_rmse(obs_clean, pred_clean)
   
   # Overall R-squared
   tss <- sum((obs_clean - mean(obs_clean))^2)
   rss <- sum((obs_clean - pred_clean)^2)
   overall_r2 <- 1 - rss/tss
   
   # High salinity metrics
   high_metrics <- extreme_event_metrics(obs_clean, pred_clean, threshold)
   
   # R-squared for high salinity events
   high_idx <- obs_clean > threshold
   if (sum(high_idx) > 1) {
      obs_high <- obs_clean[high_idx]
      pred_high <- pred_clean[high_idx]
      tss_high <- sum((obs_high - mean(obs_high))^2)
      rss_high <- sum((obs_high - pred_high)^2)
      high_r2 <- 1 - rss_high/tss_high
   } else {
      high_r2 <- NA
   }
   
   return(list(
      overall_rmse = overall_rmse,
      weighted_rmse = weighted_rmse_val,
      overall_r2 = overall_r2,
      high_salinity_rmse = high_metrics$rmse,
      high_salinity_mae = high_metrics$mae,
      high_salinity_bias = high_metrics$bias,
      high_salinity_r2 = high_r2,
      high_salinity_count = high_metrics$count,
      high_salinity_mape = high_metrics$mape,
      peak_flow_criteria = high_metrics$peak_flow_criteria,
      hit_rate = high_metrics$hit_rate,
      false_alarm_rate = high_metrics$false_alarm_rate,
      critical_success_index = high_metrics$critical_success_index,
      volume_error = high_metrics$volume_error,
      total_observations = length(obs_clean)
   ))
}