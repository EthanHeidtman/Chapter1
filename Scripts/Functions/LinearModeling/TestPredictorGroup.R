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
         eval_result <- evaluate_model(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "linear")
         eval_result$model <- model
         eval_result$formula <- formula_str
         
         if (!eval_result$model_validity) {
            cat(sprintf("Skipping %s due to invalid model results\n", predictor))
            next
         }
         
         results_list[[predictor]] <- eval_result
         
         cat(sprintf(
            "%s: High Sal RMSE = %.3f | High MAPE = %.1f%% | Overall R² = %.3f | NSE = %.3f | Score = %.3f\n",
            predictor,
            eval_result$high_sal_rmse,
            eval_result$high_sal_mae,
            eval_result$overall_r2,
            eval_result$overall_nse,
            eval_result$composite_score
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
         results = list(),
         ranked_predictors = character(0),
         best_predictor = NA_character_,  
         best_score = -Inf,
         summary_table = data.frame()
      ))
   }
   
   # Rank results by performance score
   scores <- sapply(results_list, function(x) x$composite_score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   # Determine best predictors depending on group type
   if (group_name == "temporal") {
      best_predictors <- names(scores)[ranked_indices[1:2]]
      best_scores <- scores[ranked_indices[1:2]]
   } else {
      best_predictors <- names(scores)[ranked_indices[1]]
      best_scores <- scores[ranked_indices[1]]
   }
   
   # Return results
   return(list(
      group_name = group_name,
      models = models,
      results = results_list,
      ranked_predictors = names(scores)[ranked_indices],
      best_predictor = best_predictors,
      best_score = best_scores,
      summary_table = data.frame(
         Predictor = ranked_indices,
         Score = scores[ranked_indices],
         High_Sal_RMSE = sapply(results_list[ranked_indices], function(x) x$high_sal_rmse),
         High_Sal_MAE = sapply(results_list[ranked_indices], function(x) x$high_sal_mae),
         Overall_R2 = sapply(results_list[ranked_indices], function(x) x$overall_r2),
         NSE = sapply(results_list[ranked_indices], function(x) x$overall_nse),
         KGE = sapply(results_list[ranked_indices], function(x) x$overall_kge),
         stringsAsFactors = FALSE
      )
   ))
}