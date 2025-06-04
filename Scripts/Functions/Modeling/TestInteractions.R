# Function to test interactions systematically
test_interactions <- function(base_model_formula, interaction_list, data) {
   
   cat("\n=== TESTING INTERACTION TERMS ===\n")
   
   models <- list()
   results_list <- list()
   
   for (interaction_name in names(interaction_list)) {
      predictors <- interaction_list[[interaction_name]]
      
      # Create interaction term
      interaction_term <- paste(predictors, collapse = " * ")
      formula_str <- paste(base_model_formula, "+", interaction_term)
      
      tryCatch({
         model <- lm(as.formula(formula_str), data = data)
         models[[interaction_name]] <- model
         
         eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
         eval_result$model <- model
         eval_result$formula <- formula_str
         eval_result$score <- performance_score(eval_result)
         
         results_list[[interaction_name]] <- eval_result
         
         cat(sprintf("%s: Score = %.3f\n", interaction_name, eval_result$score))
         
      }, error = function(e) {
         cat(sprintf("Error with interaction %s: %s\n", interaction_name, e$message))
      })
   }
   
   # Check if any interactions were successful
   if (length(results_list) == 0) {
      cat("No valid interactions found\n")
      return(list(
         models = list(),
         results = list(),
         ranked_interactions = character(0),
         best_interaction = NA,
         best_score = -Inf
      ))
   }
   
   # Return results
   scores <- sapply(results_list, function(x) x$score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   return(list(
      models = models,
      results = results_list,
      ranked_interactions = names(scores)[ranked_indices],
      best_interaction = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]]
   ))
}