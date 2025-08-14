test_predictor_group <- function(current_formula, predictor_group, data, group_name) {
   
   cat(sprintf("\n=== TESTING %s PREDICTORS ===\n", toupper(group_name)))
   
   models <- list()
   results_list <- list()
   
   # Test each predictor individually
   for (i in seq_along(predictor_group)) {
      predictor <- predictor_group[i]
      
      # Skip if predictor doesn't exist in data
      if (!predictor %in% names(data)) {
         cat(sprintf("Warning: %s not found in data, skipping\n", predictor))
         next
      }
      
      # Build formula
      if (current_formula == 'Salinity ~') {
         formula_str <- paste(current_formula, predictor)
      } else formula_str <- paste(current_formula, "+", predictor)
      
      # Fit model
      tryCatch({
         model <- lm(as.formula(formula_str), data = data)
         models[[predictor]] <- model
         
         # Evaluate model
         eval_result <- evaluate_model(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "linear")
         eval_result$model <- model
         eval_result$formula <- formula_str
         
         if (!eval_result$model_validity) {
            cat(sprintf("Skipping %s due to invalid model results\n", predictor))
            next
         }
         
         results_list[[predictor]] <- eval_result
         
         # Display key metrics for manual comparison
         cat(sprintf(
            "%s: Overall R²=%.3f | RMSE=%.3f | High Sal R²=%.3f RMSE=%.3f HR=%.2f FA=%.2f\n",
            predictor,
            eval_result$overall_r2,
            eval_result$overall_rmse,
            eval_result$high_sal_r2,
            eval_result$high_sal_rmse,
            eval_result$hit_rate,
            eval_result$false_alarm_rate
         ))
         
      }, error = function(e) {
         cat(sprintf("Error fitting model with %s: %s\n", predictor, e$message))
      })
   }
   
   # Check if any models were successfully fitted
   if (length(results_list) == 0) {
      cat(sprintf("No valid models fitted for %s group\n", group_name))
      return(list(
         group_name = group_name,
         models = list(),
         results = results_list,
         summary_table = data.frame()
      ))
   }
   
   # Create summary table for easy comparison (no ranking)
   summary_df <- data.frame(
      Predictor = names(results_list),
      Overall_R2 = sapply(results_list, function(x) x$overall_r2),
      Overall_RMSE = sapply(results_list, function(x) x$overall_rmse),
      High_Sal_R2 = sapply(results_list, function(x) x$high_sal_r2),
      High_Sal_RMSE = sapply(results_list, function(x) x$high_sal_rmse),
      Hit_Rate = sapply(results_list, function(x) x$hit_rate),
      False_Alarm_Rate = sapply(results_list, function(x) x$false_alarm_rate),
      High_Sal_Count = sapply(results_list, function(x) x$high_sal_count),
      stringsAsFactors = FALSE
   )
   
   # Display summary for easy comparison
   cat("\nSUMMARY TABLE:\n")
   print(summary_df, row.names = FALSE, digits = 3)
   
   # Return results without best predictor selection
   return(list(
      group_name = group_name,
      models = models,
      results = results_list,
      summary_table = summary_df
   ))
}