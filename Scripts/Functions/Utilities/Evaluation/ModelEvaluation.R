# Function to perform model evaluation
evaluate_model1.0 <- function(model, data, threshold = performance_criteria$thresholds$high_salinity_threshold, 
                           model_type = "linear") {
   
   # Get predictions based on model type
   predicted <- get_predictions(model, data, model_type)
   observed <- data$Salinity
   
   # Remove rows where either observed or predicted is NA
   valid_idx <- !is.na(observed) & !is.na(predicted$Predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx, which(colnames(predicted) == 'Predicted')]
   
   if (length(obs_clean) == 0) {
      return(list(
         overall_rmse = NA, overall_r2 = NA,
         high_salinity_rmse = NA, high_salinity_mae = NA, high_salinity_bias = NA,
         high_salinity_r2 = NA, high_salinity_count = 0,
         hit_rate = 0, false_alarm_rate = 0.5, critical_success_index = 0,
         volume_error = 0, skill_metrics = list(nash_sutcliffe = 0, skill_score = 0),
         total_observations = 0, model_validity = FALSE
      ))
   }
   
   # Overall performance metrics
   overall_rmse <- sqrt(mean((obs_clean - pred_clean)^2))
   
   # Overall R-squared
   tss <- sum((obs_clean - mean(obs_clean))^2)
   rss <- sum((obs_clean - pred_clean)^2)
   overall_r2 <- ifelse(tss == 0, 0, 1 - rss/tss)
   
   # High salinity event metrics
   high_metrics <- extreme_event_metrics(obs_clean, pred_clean, threshold)
   
   # R-squared for high salinity events
   high_idx <- obs_clean > threshold
   if (sum(high_idx) > 1) {
      obs_high <- obs_clean[high_idx]
      pred_high <- pred_clean[high_idx]
      tss_high <- sum((obs_high - mean(obs_high))^2)
      rss_high <- sum((obs_high - pred_high)^2)
      high_r2 <- ifelse(tss_high == 0, 0, 1 - rss_high/tss_high)
   } else {
      high_r2 <- NA
   }
   
   # Skill metrics using your existing function
   skill_metrics <- calculate_skill_metrics(obs_clean, pred_clean)
   
   # Model complexity metrics (important for GAMs)
   complexity_metrics <- calculate_model_complexity(model, model_type)
   
   # Model validity check
   min_high_sal_count <- if (exists("performance_criteria") && 
                             !is.null(performance_criteria$thresholds$min_high_sal_count)) {
      performance_criteria$thresholds$min_high_sal_count
   } else {
      5  # Default minimum
   }
   
   model_validity <- (
      !is.na(high_metrics$count) &&
         high_metrics$count >= min_high_sal_count &&
         !is.na(overall_r2) &&
         is.finite(overall_rmse)
   )
   
   # Residual analysis for GAMs
   residual_metrics <- if (model_type == "gam") {
      calculate_gam_residual_metrics(model, obs_clean, pred_clean)
   } else {
      list()
   }
   
   return(list(
      # Overall metrics
      overall_rmse = overall_rmse,
      overall_r2 = overall_r2,
      overall_mae = mean(abs(obs_clean - pred_clean)), 
      
      # High salinity metrics
      high_salinity_rmse = high_metrics$rmse,
      high_salinity_mae = high_metrics$mae,
      high_salinity_bias = high_metrics$bias,
      high_salinity_r2 = high_r2,
      high_salinity_count = high_metrics$count,
      high_salinity_mape = high_metrics$mape,
      hit_rate = high_metrics$hit_rate,
      false_alarm_rate = high_metrics$false_alarm_rate,
      critical_success_index = high_metrics$critical_success_index,
      volume_error = high_metrics$volume_error,
      
      # Skill metrics
      skill_metrics = skill_metrics,
      
      # Model complexity (important for GAMs)
      complexity_metrics = complexity_metrics,
      
      # GAM-specific residual metrics
      residual_metrics = residual_metrics, 
      
      # Meta information
      total_observations = length(obs_clean),
      high_salinity_fraction = high_metrics$count / length(obs_clean),
      model_validity = model_validity
   ))
}
