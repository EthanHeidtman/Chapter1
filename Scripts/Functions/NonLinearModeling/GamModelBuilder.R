gam_model_builder <- function(data, linear_model, response_var, salinity_threshold) {
   
   cat("======= Generalized Additive Model Approach =======\n")
   cat("Starting from the best linear model to better capture salinity events\n\n")
   
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
      "quasi" = quasi(link = "identity", variance = "mu^2"),
      'scat' = scat()
   )
   
   # Step 4: Define testing phases (to save computational time)
   cat("Step 4: Defining testing phases...\n")
   stages <- list(
      list(
         stage_num = 1,
         name = "Strategy Screening",
         strategies = names(gam_strategies),
         weights = "quantile",
         distributions = "gaussian"
      ),
      list(
         stage_num = 2, 
         name = "Distribution Testing",
         strategies = NULL,  # Will be filled from stage 1 results
         weights = "quantile",
         distributions = names(distributions)
      ),
      list(
         stage_num = 3,
         name = "Weight Scheme Testing", 
         strategies = NULL,  # Will be filled from stage 2 results
         weights = names(weight_schemes),
         distributions = NULL  # Will be filled from stage 2 results
      )
   )
   
   # Initialize results storage
   results <- list()
   all_performance <- list()
   
   # Step 5: Systematic model fitting
   cat("Step 5: Systematic model fitting...\n\n")
   
   for (stage in stages) {
      cat(sprintf("\n=== STAGE %d: %s ===\n", stage$stage_num, stage$name))
      
      # Skip if configuration is incomplete (shouldn't happen for stage 1)
      if(is.null(stage$strategies) || is.null(stage$distributions)) {
         cat("Skipping stage due to incomplete configuration\n")
         next
      }
      
      # Calculate and display model count for this stage
      total_models <- length(stage$strategies) * 
         length(stage$weights) * 
         length(stage$distributions)
      cat(sprintf("Fitting %d models for Stage %d...\n\n", total_models, stage$stage))
      
      # Fit all models for current stage
      stage_results <- list()
      model_counter <- 0
      
      for(strategy_name in stage$strategies) {
         for(weight_name in stage$weights) {
            for(dist_name in stage$distributions) {
               
               model_counter <- model_counter + 1
               model_id <- paste("stage", stage$stage_num, strategy_name, weight_name, dist_name, sep = "_")
               
               cat(sprintf("  [%d/%d] Fitting: %s_%s_%s\n", 
                           model_counter, total_models, strategy_name, weight_name, dist_name))
               
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
                  
                  stage_results[[model_id]] <- eval_result
                  results[[model_id]] <- eval_result
               } else {
                  cat(sprintf("WARNING: Model %s failed to fit\n", model_id))
               }
            }
         }
      }
      
      # Analyze stage results
      cat(sprintf("\n=== STAGE %d ANALYSIS ===\n", stage$stage_num))
      
      if(length(stage_results) == 0) {
         cat("ERROR: No models fitted successfully in this stage!\n")
         break
      }
      
      # Create performance summary for this stage
      stage_performance <- data.frame(
         model_id = names(stage_results),
         strategy = sapply(stage_results, function(x) x$strategy),
         weight_scheme = sapply(stage_results, function(x) x$weight_scheme),
         distribution = sapply(stage_results, function(x) x$distribution),
         score = sapply(stage_results, function(x) x$score),
         stage = stage$stage_num,
         stringsAsFactors = FALSE
      )
      
      # Sort by performance score
      stage_performance <- stage_performance[order(-stage_performance$score), ]
      all_performance[[paste0("stage_", stage$stage_num)]] <- stage_performance
      
      # Display top performers
      cat("Top performers for Stage", stage$stage_num, ":\n")
      top_n <- min(10, nrow(stage_performance))
      for(i in 1:top_n) {
         cat(sprintf("  %d. %s_%s_%s (score: %.4f)\n", 
                     i,
                     stage_performance$strategy[i],
                     stage_performance$weight_scheme[i],
                     stage_performance$distribution[i],
                     stage_performance$score[i]))
      }
      
      # Configure next stage based on current results
      if(stage$stage_num == 1) {
         # Select top 3 strategies for stage 2
         strategy_scores <- aggregate(stage_performance$score, 
                                      by = list(strategy = stage_performance$strategy),
                                      FUN = mean, na.rm = TRUE)
         strategy_scores <- strategy_scores[order(-strategy_scores$x), ]
         top_strategies <- strategy_scores$strategy[1:min(3, nrow(strategy_scores))]
         
         stages[[2]]$strategies <- top_strategies
         
         cat(sprintf("\nSelected strategies for Stage 2: %s\n", 
                     paste(top_strategies, collapse = ", ")))
         
      } else if(stage_config$stage == 2) {
         # Select top 2 strategy-distribution combos for stage 3
         top_combos <- stage_performance[1:min(2, nrow(stage_performance)), ]
         
         stages[[3]]$strategies <- unique(top_combos$strategy)
         stages[[3]]$distributions <- unique(top_combos$distribution)
         
         cat(sprintf("\nSelected for Stage 3:\n"))
         cat(sprintf("  Strategies: %s\n", paste(stages[[3]]$strategies, collapse = ", ")))
         cat(sprintf("  Distributions: %s\n", paste(stages[[3]]$distributions, collapse = ", ")))
      }
      
      # # Save progress after each stage
      # save(results, all_performance, stages, 
      #      file = paste0("gam_results_stage_", stage$stage_num, ".RData"))
      # 
      # cat(sprintf("\nStage %d complete. Progress saved.\n", stage$stage_num))
      # cat(rep("-", 60), "\n")
   }
   
   # Final summary
   cat("\n", rep("=", 80), "\n")
   cat("ALL STAGES COMPLETE!\n")
   cat(rep("=", 80), "\n")
   
   # Overall summary
   for(stage_name in names(all_performance)) {
      perf_data <- all_performance[[stage_name]]
      stage_number <- unique(perf_data$stage)[1]
      
      cat(sprintf("Stage %d: %d models, best score: %.4f (%s_%s_%s)\n", 
                  stage_number,
                  nrow(perf_data),
                  perf_data$score[1],
                  perf_data$strategy[1],
                  perf_data$weight_scheme[1],
                  perf_data$distribution[1]))
   }
   
   # Step 6: Summarize results and select best model
   cat("\nStep 6: Model fitting complete. Summarizing results...\n")
   
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
   formula_char <- deparse(best_result$formula$formula)
   if(length(formula_char) > 1) {
      formula_char <- paste(formula_char, collapse = " ")
   }
   
   # Extract predictor names from the best model
   predictors <- all.vars(best_result$formula$formula)[-1]  # Remove response variable
   
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