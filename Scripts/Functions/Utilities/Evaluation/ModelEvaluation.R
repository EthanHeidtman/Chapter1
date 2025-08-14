# Function to perform model evaluation
evaluate_model <- function(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "gam") {
   
   # Get predictions based on model type
   predicted <- get_predictions(model, data, model_type)
   observed <- data$Salinity
   
   # Clean data
   valid_idx <- !is.na(observed) & !is.na(predicted$Predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx, which(colnames(predicted) == 'Predicted')]
   
   if (length(obs_clean) == 0) {
      return(NULL)
   }
   
   # Calculate component metrics (no scoring, just raw metrics)
   accuracy_results <- calculate_high_sal_accuracy(obs_clean, pred_clean, threshold)
   reliability_results <- calculate_high_sal_reliability(obs_clean, pred_clean, threshold)
   overall_results <- calculate_overall_performance(obs_clean, pred_clean)
   
   # Model validity check
   model_validity <- (
      accuracy_results$count >= 5 &&
         !is.na(overall_results$r2) &&
         is.finite(overall_results$rmse)
   )
   
   # Return comprehensive evaluation with raw metrics
   return(list(
      # Overall performance metrics
      overall_r2 = overall_results$r2,
      overall_rmse = overall_results$rmse,
      overall_mae = overall_results$mae,
      overall_bias = overall_results$bias,
      overall_kge = overall_results$kge,
      
      # High salinity performance metrics
      high_sal_r2 = accuracy_results$r2,
      high_sal_rmse = accuracy_results$rmse,
      high_sal_mae = accuracy_results$mae,
      high_sal_bias = accuracy_results$bias,
      high_sal_kge = accuracy_results$kge,
      high_sal_count = accuracy_results$count,
      
      # High salinity detection metrics
      hit_rate = reliability_results$sensitivity,
      miss_rate = 1 - reliability_results$sensitivity,
      false_alarm_rate = reliability_results$false_alarm_rate,
      precision = reliability_results$precision,
      
      # Meta information
      total_observations = length(obs_clean),
      high_salinity_fraction = accuracy_results$count / length(obs_clean),
      model_validity = model_validity,
      threshold_used = threshold
   ))
}