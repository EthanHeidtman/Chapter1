# Function to test combinations of best predictors
test_predictor_combinations <- function(base_formula, predictor_list, data, max_combinations = 30) {
   
   cat("\n=== TESTING PREDICTOR COMBINATIONS ===\n")
   
   # Filter out any NA or invalid predictors
   valid_predictors <- predictor_list[!is.na(predictor_list) & predictor_list != ""]
   
   if (length(valid_predictors) < 2) {
      cat("Not enough valid predictors for combinations. Need at least 2.\n")
      return(list(
         models = list(),
         results = list(),
         ranked_combinations = character(0),
         best_combination = NA,
         best_score = -Inf,
         summary_table = data.frame()
      ))
   }
   
   # Extract existing predictors from base_formula to avoid duplicates
   existing_predictors <- character(0)
   if (base_formula != "Salinity ~") {
      # Extract predictors from base formula
      formula_parts <- strsplit(base_formula, "~")[[1]][2]
      formula_parts <- trimws(formula_parts)
      if (formula_parts != "") {
         existing_predictors <- trimws(strsplit(formula_parts, "\\+")[[1]])
      }
   }
   
   # Remove predictors that are already in the base formula
   valid_predictors <- setdiff(valid_predictors, existing_predictors)
   
   if (length(valid_predictors) < 1) {
      cat("All predictors already in base formula. No combinations to test.\n")
      return(list(
         models = list(),
         results = list(),
         ranked_combinations = character(0),
         best_combination = NA,
         best_score = -Inf,
         summary_table = data.frame()
      ))
   }
   
   # Generate combinations (start with pairs, then triplets, etc.)
   models <- list()
   results_list <- list()
   
   # Test pairwise combinations
   for (i in 1 : (length(valid_predictors) - 1)) {
      for (j in (i + 1) : length(valid_predictors)) {
         # Check for oddly named interaction terms
         if (grepl('_x_', valid_predictors[j])) {
            combo_name <- paste(valid_predictors[i], valid_predictors[j], sep = " + ")
            preds <- strsplit(valid_predictors[j], "_x_")[[1]]
            interaction_str <- paste(preds, collapse = ' * ')
            formula_str <- paste(base_formula, valid_predictors[i], '+', interaction_str)
         } else {
            combo_name <- paste(valid_predictors[i], valid_predictors[j], sep = " + ")
            formula_str <- paste(base_formula, valid_predictors[i], "+", valid_predictors[j])
         }
         
         tryCatch({
            model <- lm(as.formula(formula_str), data = data)
            models[[combo_name]] <- model
            
            # Evaluate model
            eval_result <- evaluate_model(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "linear")
            eval_result$model <- model
            eval_result$formula <- formula_str
            
            if (!eval_result$model_validity) {
               cat(sprintf("Skipping %s due to invalid model results\n", predictor))
               next
            }
            
            results_list[[combo_name]] <- eval_result
            
            cat(sprintf(
               "%s: High Sal RMSE = %.3f | High MAPE = %.1f%% | Overall R² = %.3f | NSE = %.3f | Score = %.3f\n",
               combo_name,
               eval_result$high_sal_rmse,
               eval_result$high_sal_mape,
               eval_result$overall_r2,
               eval_result$overall_nse,
               eval_result$composite_score
            ))
            
         }, error = function(e) {
            cat(sprintf("Error with combination %s: %s\n", combo_name, e$message))
         })
      }
   }
   
   # Test best triplet combinations (top 5 pairs + next best individual)
   if (length(results_list) > 0) {
      scores <- sapply(results_list, function(x) x$composite_score)
      top_pairs <- names(sort(scores, decreasing = TRUE))[1:min(3, length(scores))]
      
      for (pair_name in top_pairs) {
         for (additional_pred in predictor_list) {
            current_formula <- results_list[[pair_name]]$formula
            
            # Check if this predictor is already in the pair
            if (grepl(additional_pred, pair_name, fixed = TRUE)) next
            
            triplet_name <- paste(pair_name, additional_pred, sep = " + ")
            
            # Check for oddly named interaction terms
            if (grepl('_x_', additional_pred)) {
               additional_pred <- strsplit(additional_pred, '_x_')[[1]]
               interaction_str <- paste(preds, collapse = ' * ')
               formula_str <- paste(current_formula, '+', interaction_str)
            } else {
               formula_str <- paste(current_formula, "+", additional_pred)
            }
         
            tryCatch({
               model <- lm(as.formula(formula_str), data = data)
               models[[triplet_name]] <- model
               
               # Evaluate model
               eval_result <- evaluate_model(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "linear")
               eval_result$model <- model
               eval_result$formula <- formula_str
               
               if (!eval_result$model_validity) {
                  cat(sprintf("Skipping %s due to invalid model results\n", predictor))
                  next
               }
               
               results_list[[triplet_name]] <- eval_result
               
               cat(sprintf(
                  "%s: High Sal RMSE = %.3f | High MAPE = %.1f%% | Overall R² = %.3f | NSE = %.3f | Score = %.3f\n",
                  triplet_name,
                  eval_result$high_sal_rmse,
                  eval_result$high_sal_mape,
                  eval_result$overall_r2,
                  eval_result$overall_nse,
                  eval_result$composite_score
               ))
            }, error = function(e) {
               cat(sprintf("Error with triplet %s: %s\n", triplet_name, e$message))
            })
         }
      }
   }
   
   # Check if any combinations were successful
   if (length(results_list) == 0) {
      cat("No valid combinations found\n")
      return(list(
         models = list(),
         results = list(),
         ranked_combinations = character(0),
         best_combination = NA,
         best_score = -Inf,
         summary_table = data.frame()
      ))
   }
   
   # Return ranked results
   scores <- sapply(results_list, function(x) x$composite_score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   # Return results
   return(list(
      models = models,
      results = results_list,
      ranked_predictors = names(scores)[ranked_indices],
      best_combination = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]],
      summary_table = data.frame(
         Predictor = ranked_indices,
         Score = scores[ranked_indices],
         High_Sal_RMSE = sapply(results_list[ranked_indices], function(x) x$high_sal_rmse),
         High_Sal_MAPE = sapply(results_list[ranked_indices], function(x) x$high_sal_mape),
         Overall_R2 = sapply(results_list[ranked_indices], function(x) x$overall_r2),
         NSE = sapply(results_list[ranked_indices], function(x) x$overall_nse),
         stringsAsFactors = FALSE
      )
   ))
}