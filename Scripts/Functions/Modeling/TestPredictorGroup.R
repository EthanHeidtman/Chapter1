# Function to test predictor group systematically
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
         eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
         eval_result$model <- model
         eval_result$formula <- formula_str
         eval_result$score <- performance_score(eval_result)
         
         results_list[[predictor]] <- eval_result
         
         cat(sprintf("%s: High Sal RMSE = %.3f, Overall R2 = %.3f, Low R2 = %.3f, High Salinity MAPE = %.3f, Score = %.3f\n", 
                     predictor, eval_result$high_salinity_rmse, eval_result$overall_r2, eval_result$low_r2, eval_result$high_salinity_mape,  eval_result$score))
         
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
         results = list(),
         ranked_predictors = character(0),
         best_predictor = NA_character_,  
         best_score = -Inf,
         summary_table = data.frame()
      ))
   }
   
   # Rank results by performance score
   scores <- sapply(results_list, function(x) x$score)
   ranked_indices <- order(scores, decreasing = TRUE)
   #cat(sprintf("Debug: predictor names = %s\n", paste(names(scores), collapse = ", ")))
   
   # Return results
   return(list(
      group_name = group_name,
      models = models,
      results = results_list,
      ranked_predictors = names(scores)[ranked_indices],
      best_predictor = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]],
      summary_table = data.frame(
         Predictor = names(scores)[ranked_indices],
         Score = scores[ranked_indices],
         High_Sal_RMSE = sapply(results_list[ranked_indices], function(x) x$high_salinity_rmse),
         Overall_R2 = sapply(results_list[ranked_indices], function(x) x$overall_r2),
         Low_R2 = sapply(results_list[ranked_indices], function(x) x$low_r2),
         High_Sal_MAPE = sapply(results_list[ranked_indices], function(x) x$high_salinity_mape),
         stringsAsFactors = FALSE
      )
   ))
}