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
   
   # Calculate component scores
   #detection_results <- calculate_high_sal_detection(obs_clean, pred_clean, threshold)
   accuracy_results <- calculate_high_sal_accuracy(obs_clean, pred_clean, threshold)
   reliability_results <- calculate_high_sal_reliability(obs_clean, pred_clean, threshold)
   overall_results <- calculate_overall_performance(obs_clean, pred_clean)
   complexity_results <- calculate_complexity(model, model_type)
   
   # Calculate weighted composite score
   composite_score <- (
      #performance_weights['high_sal_detection'] * detection_results$score +
      performance_weights['high_sal_accuracy'] * accuracy_results$score +
      performance_weights['high_sal_reliability'] * reliability_results$score +
      performance_weights['overall_performance'] * overall_results$score +
      performance_weights['complexity'] * complexity_results$score
   )
   
   # Model validity check
   model_validity <- (
      accuracy_results$count >= 5 &&
         !is.na(overall_results$r2) &&
         is.finite(overall_results$rmse)
   )
   
   # Return comprehensive but interpretable evaluation
   return(list(
      # THE MAIN SCORE
      composite_score = as.numeric(composite_score),
      
      # Component scores (all 0-1, higher is better)
      #high_sal_detection_score = detection_results$score,
      high_sal_accuracy_score = accuracy_results$score,
      high_sal_reliability_score = reliability_results$score,
      overall_performance_score = overall_results$score,
      complexity_score = complexity_results$score,
      
      # High salinity confusion matrix metrics
      hit_rate = reliability_results$sensitivity,
      miss_rate = 1 - reliability_results$sensitivity,
      false_alarm_rate = reliability_results$false_alarm_rate,
      precision = reliability_results$precision,
      #csi = reliability_results$csi,
      
      # High salinity error metrics
      high_sal_r2 = accuracy_results$r2,
      high_sal_rmse = accuracy_results$rmse,
      high_sal_mae = accuracy_results$mae,
      high_sal_bias = accuracy_results$bias,
      high_sal_count = accuracy_results$count,
      high_sal_kge = accuracy_results$kge,
      high_sal_nse = accuracy_results$nse,
      
      # Overall error metrics
      overall_r2 = overall_results$r2,
      overall_rmse = overall_results$rmse,
      overall_mae = overall_results$mae,
      overall_bias = overall_results$bias,
      overall_kge = overall_results$kge,
      overall_nse = overall_results$nse,
      
      # Model complexity metrics
      model_edf = complexity_results$edf,
      model_n_smooths = complexity_results$n_smooths,
      
      # Meta information
      total_observations = length(obs_clean),
      high_salinity_fraction = accuracy_results$count / length(obs_clean),
      model_validity = model_validity,
      threshold_used = threshold
   ))
}