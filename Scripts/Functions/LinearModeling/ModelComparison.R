
# Function to compare multiple models
compare_models <- function(models, model_types, data, threshold, model_names = NULL, Group) {
   
   # Create Model names if none are given
   if (is.null(model_names)) {
      model_names <- paste0("Model_", 1:length(models))
   }
   
   # Initialize results data frame
   results <- data.frame()
   
   for (i in 1:length(models)) {
      eval_results <- evaluate_model(models[[i]], data, threshold, model_types[i])
      
      # Add the results to the results dataframe
      results <- rbind(results, data.frame(
         Group = as.factor(Group),
         Model = model_names[i],
         Type = model_types[i],
         Overall_RMSE = eval_results$overall_rmse,
         Weighted_RMSE = eval_results$weighted_rmse,
         Overall_R2 = eval_results$overall_r2,
         High_Sal_RMSE = eval_results$high_salinity_rmse,
         High_Sal_MAE = eval_results$high_salinity_mae,
         High_Sal_Bias = eval_results$high_salinity_bias,
         High_Sal_R2 = eval_results$high_salinity_r2,
         High_Sal_Mape = eval_results$high_salinity_mape,
         High_Sal_PeakFlow = eval_results$peak_flow_criteria,
         High_Sal_HitRate = eval_results$hit_rate,
         High_Sal_FalseAlarm = eval_results$false_alarm_rate,
         High_Sal_CriticalSuccess = eval_results$critical_success_index,
         High_Sal_VolumeError = eval_results$volume_error,
         High_Sal_Count = eval_results$high_salinity_count,
         Total_Obs = eval_results$total_observations,
         stringsAsFactors = FALSE
      ))
   }
   return(results)
}
