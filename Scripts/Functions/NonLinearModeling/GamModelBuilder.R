gam_model_builder <- function(data, linear_model, response_var, salinity_threshold) {
   
   cat("======= Generalized Additive Model Approach =======\n")
   cat("Starting from the best linear model to better capture salinity events\n\n")
   
   # Initialize results storage
   results <- list()
   
   # Extract linear model formula and predictors
   linear_formula <- formula(linear_model)
   linear_predictors <- all.vars(linear_formula)[-1]  # Remove response variable
   
   # Step 1: Create different weighting schemes
   cat("Step 1: Creating weighting schemes for extreme events...\n")
   weight_schemes <- list(
      'none' = NULL,
      "quantile" = create_extreme_weights(data[[response_var]], "quantile_progressive"),
      "exponential" = create_extreme_weights(data[[response_var]], "exponential"),
      "binary" = create_extreme_weights(data[[response_var]], "binary_extreme")
   )

   # Step 2: Define GAM enhancement strategies
   cat("Step 2: Defining GAM enhancement strategies...\n")
   gam_strategies <- list(
      "baseline" = "linear",                       # Reproduce linear model exactly
      "smooth_all" = "smooth_all",                 # Smooth all continuous predictors
      "smooth_flow" = "smooth_flow",               # Smooth only flow variables
      "smooth_stress" = "smooth_stress",           # Smooth only stress variables
      "smooth_tide" = "smooth_tide",               # Smooth only tide variables
      "tensor_flow_stress" = "tensor",             # Tensor product of flow and stress
      "tensor_flow_tide" = "tensor",               # Tensor product of flow and tide
      "mixed_interactions" = "mixed_interactions"  # Strategic mix of smooth and tensor terms
   )
   
   # Step 3: Define alternative distributions
   cat("Step 3: Defining alternative distributions...\n")
   distributions <- list(
      "gaussian" = gaussian(),
      "gamma" = Gamma(link = "log"),
      "tweedie" = tw(),
      "quasi" = quasi(link = "identity", variance = "mu^2")
   )
   
   # Step 4: Systematic model fitting
   cat("Step 4: Systematic model fitting...\n\n")
   
   model_counter <- 0
   total_models <- length(gam_strategies) * length(weight_schemes) * length(distributions)
   
   for(strategy_name in names(gam_strategies)) {
      for(weight_name in names(weight_schemes)) {
         for(dist_name in names(distributions)) {
            
            model_counter <- model_counter + 1
            model_id <- paste(strategy_name, weight_name, dist_name, sep = "_")
            
            cat(sprintf("  [%d/%d] Fitting: %s\n", model_counter, total_models, model_id))
            
            # Fit the GAM model
            gam_result <- fit_gam(
               data = data,
               linear_formula = linear_formula,
               linear_predictors = linear_predictors,
               weights = weight_schemes[[weight_name]],
               strategy = gam_strategies[[strategy_name]],
               family = distributions[[dist_name]]
            )
            
            # Evaluate if model fitted successfully
            if(!is.null(gam_result$model)) {
               eval_result <- evaluate_model(gam_result$model, data, salinity_threshold, "gam")
               eval_result$model <- gam_result$model
               eval_result$formula <- gam_result$formula
               eval_result$strategy <- strategy_name
               eval_result$weight_scheme <- weight_name
               eval_result$distribution <- dist_name
               eval_result$score <- performance_score(eval_result)
               
               results[[model_id]] <- eval_result
            } else {
               cat(sprintf("WARNING: Model %s failed to fit\n", model_id))
            }
         }
      }
   }
   
   # Step 5: Summarize results and select best model
   cat("\nStep 5: Model fitting complete. Summarizing results...\n")
   
   # Create summary table of all models
   summary_table <- data.frame(
      model_id = names(results),
      strategy = sapply(results, function(x) x$strategy),
      weights = sapply(results, function(x) x$weight_scheme),
      distribution = sapply(results, function(x) x$distribution),
      score = sapply(results, function(x) x$score),
      stringsAsFactors = FALSE
   )
   
   # Sort by performance score (assuming higher is better)
   summary_table <- summary_table[order(summary_table$score, decreasing = TRUE), ]
   
   cat("\nTop 10 performing models:\n")
   print(head(summary_table, 10))
   
   # Get the best model
   best_model_id <- summary_table$model_id[1]
   best_result <- results[[best_model_id]]
   
   # Create summary statistics similar to linear model builder
   model_summary <- list(
      best_model_id = best_model_id,
      strategy = best_result$strategy,
      weight_scheme = best_result$weight_scheme,
      distribution = best_result$distribution,
      n_models_tested = length(results),
      n_successful_fits = sum(sapply(results, function(x) !is.null(x$model))),
      score_range = range(summary_table$score),
      top_strategies = names(sort(table(summary_table$strategy[1:min(10, nrow(summary_table))]), decreasing = TRUE))
   )
   
   # Extract formula as character string
   formula_char <- deparse(best_result$formula)
   if(length(formula_char) > 1) {
      formula_char <- paste(formula_char, collapse = " ")
   }
   
   # Extract predictor names from the best model
   predictors <- all.vars(best_result$formula)[-1]  # Remove response variable
   
   # Create stage results (record of the modeling process)
   stage_results <- list(
      weight_schemes = names(weight_schemes),
      gam_strategies = names(gam_strategies),
      distributions = names(distributions),
      total_combinations = length(weight_schemes) * length(gam_strategies) * length(distributions),
      successful_fits = summary_table$model_id[!is.na(summary_table$score)],
      failed_fits = setdiff(expand.grid(names(gam_strategies), names(weight_schemes), names(distributions))$Var1, 
                            summary_table$model_id),
      performance_by_strategy = aggregate(score ~ strategy, data = summary_table, FUN = mean, na.rm = TRUE),
      performance_by_weights = aggregate(score ~ weights, data = summary_table, FUN = mean, na.rm = TRUE),
      performance_by_distribution = aggregate(score ~ distribution, data = summary_table, FUN = mean, na.rm = TRUE),
      all_results_table = summary_table,
      detailed_results = results
   )
   
   # Return structured output matching linear model builder format
   final_result <- list(
      model = best_result$model,
      formula = formula_char,
      predictors = predictors,
      evaluation = best_result,  # This contains all the evaluation metrics
      score = best_result$score,
      stage_results = stage_results,
      summary = model_summary
   )
   
   class(final_result) <- "gam_model_builder_result"
   
   return(final_result)
}