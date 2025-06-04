# Function to test combinations of best predictors
test_predictor_combinations <- function(base_formula, predictor_list, data, max_combinations = 10) {
   
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
   
   # Generate combinations (start with pairs, then triplets, etc.)
   models <- list()
   results_list <- list()
   
   # Test pairwise combinations
   for (i in 1:(length(valid_predictors)-1)) {
      for (j in (i+1):length(valid_predictors)) {
         
         combo_name <- paste(valid_predictors[i], valid_predictors[j], sep = "_+_")
         formula_str <- paste(base_formula, "+", valid_predictors[i], "+", valid_predictors[j])
         
         tryCatch({
            model <- lm(as.formula(formula_str), data = data)
            models[[combo_name]] <- model
            
            eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
            eval_result$model <- model
            eval_result$formula <- formula_str
            eval_result$score <- performance_score(eval_result)
            
            results_list[[combo_name]] <- eval_result
            
            cat(sprintf("%s: Score = %.3f\n", combo_name, eval_result$score))
            
         }, error = function(e) {
            cat(sprintf("Error with combination %s: %s\n", combo_name, e$message))
         })
      }
   }
   
   # Test best triplet combinations (top 5 pairs + next best individual)
   if (length(results_list) > 0) {
      scores <- sapply(results_list, function(x) x$score)
      top_pairs <- names(sort(scores, decreasing = TRUE))[1:min(3, length(scores))]
      
      for (pair_name in top_pairs) {
         for (additional_pred in predictor_list) {
            
            # Check if this predictor is already in the pair
            if (grepl(additional_pred, pair_name, fixed = TRUE)) next
            
            triplet_name <- paste(pair_name, additional_pred, sep = "_+_")
            current_formula <- results_list[[pair_name]]$formula
            formula_str <- paste(current_formula, "+", additional_pred)
            
            tryCatch({
               model <- lm(as.formula(formula_str), data = data)
               models[[triplet_name]] <- model
               
               eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
               eval_result$model <- model
               eval_result$formula <- formula_str
               eval_result$score <- performance_score(eval_result)
               
               results_list[[triplet_name]] <- eval_result
               
               cat(sprintf("%s: Score = %.3f\n", triplet_name, eval_result$score))
               
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
   scores <- sapply(results_list, function(x) x$score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   return(list(
      models = models,
      results = results_list,
      ranked_combinations = names(scores)[ranked_indices],
      best_combination = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]],
      summary_table = data.frame(
         Combination = names(scores)[ranked_indices],
         Score = scores[ranked_indices],
         High_Sal_RMSE = sapply(results_list[ranked_indices], function(x) x$high_salinity_rmse),
         Overall_R2 = sapply(results_list[ranked_indices], function(x) x$overall_r2),
         stringsAsFactors = FALSE
      )
   ))
}