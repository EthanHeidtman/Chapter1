# function to systematically test interactions between best predictors
test_interactions <- function(current_formula, best_predictors, data) {
   
   cat(sprintf("=== TESTING SYSTEMATIC INTERACTIONS ===\n"))
   cat(sprintf("Base formula: %s\n", current_formula))
   cat(sprintf("Best predictors to test: %s\n", paste(best_predictors, collapse = ", ")))
   
   results <- list()
   models <- list()
   
   # Remove any NA predictors
   best_predictors <- best_predictors[!is.na(best_predictors)]
   
   if (length(best_predictors) < 2) {
      cat("Not enough predictors for interaction testing\n")
      return(list(results = results, models = models, best_interaction = NA, best_score = -Inf))
   }
   
   # Test all pairwise interactions
   cat("Testing pairwise interactions...\n")
   pairwise_combos <- combn(best_predictors, 2, simplify = FALSE)
   
   for (i in seq_along(pairwise_combos)) {
      pred1 <- pairwise_combos[[i]][1]
      pred2 <- pairwise_combos[[i]][2]
      interaction_term <- paste(pred1, pred2, sep = " : ")
      interaction_name <- paste(pred1, pred2, sep = "_x_")
      
      test_formula <- paste(current_formula, "+", interaction_term)
      
      tryCatch({
         model <- lm(as.formula(test_formula), data = data)
         
         # Evaluate model
         eval_result <- evaluate_model(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "linear")
         eval_result$model <- model
         eval_result$formula <- test_formula
         
         if (!eval_result$model_validity) {
            cat(sprintf("Skipping %s due to invalid model results\n", predictor))
            next
         }
         
         results[[interaction_name]] <- list(
            formula = test_formula,
            interaction_term = interaction_term,
            metrics = eval_result
         )
         models[[interaction_name]] <- model
         
         cat(sprintf(
            "%s: High Sal RMSE = %.3f | High MAPE = %.1f%% | Overall R² = %.3f | NSE = %.3f | Score = %.3f\n",
            interaction_name,
            eval_result$high_sal_rmse,
            eval_result$high_sal_mape,
            eval_result$overall_r2,
            eval_result$overall_nse,
            eval_result$composite_score
         ))
         
      }, error = function(e) {
         cat(sprintf("Error with interaction %s: %s\n", interaction_name, e$message))
      })
   }
   
   # Test three-way interactions if we have enough predictors and max_interactions >= 3
   if (length(best_predictors) >= 3) {
      # && max_interactions >= 3
      cat("Testing three-way interactions...\n")
      threeway_combos <- combn(best_predictors, 3, simplify = FALSE)
      max_threeway <- length(threeway_combos)
      
      for (i in seq_len(max_threeway)) {
         pred1 <- threeway_combos[[i]][1]
         pred2 <- threeway_combos[[i]][2]
         pred3 <- threeway_combos[[i]][3]
         interaction_term <- paste(pred1, pred2, pred3, sep = " : ")
         interaction_name <- paste(pred1, pred2, pred3, sep = "_x_")
         
         test_formula <- paste(current_formula, "+", interaction_term)
         
         tryCatch({
            model <- lm(as.formula(test_formula), data = data)
            
            # Evaluate model
            eval_result <- evaluate_model(model, data, threshold = salinity_threshold, performance_weights = performance_criteria$weights, model_type = "linear")
            eval_result$model <- model
            eval_result$formula <- test_formula
            
            if (!eval_result$model_validity) {
               cat(sprintf("Skipping %s due to invalid model results\n", predictor))
               next
            }
            
            results[[interaction_name]] <- list(
               formula = test_formula,
               interaction_term = interaction_term,
               metrics = eval_result
            )
            models[[interaction_name]] <- model
            
            cat(sprintf(
               "%s: High Sal RMSE = %.3f | High MAPE = %.1f%% | Overall R² = %.3f | NSE = %.3f | Score = %.3f\n",
               interaction_name,
               eval_result$high_sal_rmse,
               eval_result$high_sal_mape,
               eval_result$overall_r2,
               eval_result$overall_nse,
               eval_result$composite_score
            ))
            
         }, error = function(e) {
            cat(sprintf("Error with three-way interaction %s: %s\n", interaction_name, e$message))
         })
      }
   }
   
   # Find best interaction
   best_interaction <- NA
   best_score <- -Inf
   
   if (length(results) > 0) {
      scores <- sapply(results, function(x) x$metrics$composite_score)
      best_idx <- which.max(scores)
      best_interaction <- names(results)[best_idx]
      best_score <- scores[best_idx]
      
      cat(sprintf("Best interaction: %s (Score: %.3f)\n", best_interaction, best_score))
   }
   
   return(list(
      results = results,
      models = models,
      best_interaction = best_interaction,
      best_score = best_score
   ))
}