gam_model_builder <- function(data, linear_model, response_var, salinity_threshold) {
   
   cat("======= Generalized Additive Model Approach =======\n")
   cat("Starting from the best linear model to better capture salinity events\n\n")
   
   # Extract linear model formula and predictors
   # Extract and clean linear formula and predictors
   linear_formula <- formula(linear_model)
   environment(linear_formula) <- .GlobalEnv # detach the model from the model environment
   linear_predictors <- all.vars(linear_formula)[-1]
   
   # Create minimal data object - only necessary columns
   required_cols <- unique(c('DateTime', response_var, linear_predictors))
   data <- data[, required_cols, drop = FALSE]
   
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
      # "tensor_flow_tide" = "tensor",               # Tensor product of flow and tide
      "mixed_interactions" = "mixed_interactions"  # Strategic mix of smooth and tensor terms
   )
   
   # Step 3: Define alternative distributions
   cat("Step 3: Defining alternative distributions...\n")
   distributions <- list(
      "gaussian" = gaussian(),                                
      "gamma" = Gamma(link = "log"),                          
      "tweedie" = tw()                                       # Exponential Tweedie family distributions (more flexible)
      # "quasi" = quasi(link = "identity", variance = "mu^2"),  # Quasi-family distribution
      # 'scat' = scat()                                         # scaled-t, for heavy tailed response variables
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
   stage_times <- numeric(length(stages))
   
   # Step 5: Systematic model fitting
   cat("Step 5: Systematic model fitting...\n\n")
   
   for (stage_idx in seq_along(stages)) {
      stage <- stages[[stage_idx]]
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
      
      start_time <- Sys.time()
      
      for(strategy_name in stage$strategies) {
         for(weight_name in stage$weights) {
            for(dist_name in stage$distributions) {
               
               model_counter <- model_counter + 1
               model_id <- paste("stage", stage$stage_num, strategy_name, weight_name, dist_name, sep = "_")
               
               cat(sprintf("  [%d/%d] Fitting: %s_%s_%s\n", 
                           model_counter, total_models, strategy_name, weight_name, dist_name))
               
               # Fit the GAM model with strip = TRUE for speed
               gam_result <- fit_gam(
                  data = data,
                  linear_formula = linear_formula,
                  linear_predictors = linear_predictors,
                  strategy = strategy_name,
                  weight = weight_name,
                  distribution = dist_name,
                  weight_schemes = weight_schemes,
                  gam_strategies = gam_strategies,
                  distributions = distributions,
                  salinity_threshold = salinity_threshold,
                  stage_num = stage$stage_num,
                  strip = TRUE  # Strip models during optimization phase
               )
            
               # Evaluate if model fitted successfully
               if(!is.null(gam_result)) {
                  stage_results[[model_id]] <- gam_result$result
                  results[[model_id]] <- gam_result$result
               } else {
                  cat(sprintf("WARNING: Model %s failed to fit\n", model_id))
               }
               
               # Garbage collection after every few models to keep memory usage down
               if(model_counter %% 3 == 0) {  # More frequent GC
                  gc(verbose = FALSE)
                  # Clear any lingering large objects
                  if(exists("gam_result")) rm(gam_result)
               }
            }
         }
      }
      
      stage_times[stage_idx] <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      
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
         top_strategies <- strategy_scores$strategy[1:min(4, nrow(strategy_scores))]
         
         stages[[2]]$strategies <- top_strategies
         
         cat(sprintf("\nSelected strategies for Stage 2: %s\n", 
                     paste(top_strategies, collapse = ", ")))
         
      } else if(stage$stage_num == 2) {
         # Select top 2 strategy-distribution combos for stage 3
         top_combos <- stage_performance[1:min(2, nrow(stage_performance)), ]
         
         stages[[3]]$strategies <- unique(top_combos$strategy)
         stages[[3]]$distributions <- unique(top_combos$distribution)
         
         cat(sprintf("\nSelected for Stage 3:\n"))
         cat(sprintf("  Strategies: %s\n", paste(stages[[3]]$strategies, collapse = ", ")))
         cat(sprintf("  Distributions: %s\n", paste(stages[[3]]$distributions, collapse = ", ")))
      }
      
      # Force garbage collection after each stage
      gc(verbose = FALSE)
   }
   
   # Final summary
   cat("\n", rep("=", 80), "\n")
   cat("ALL STAGES COMPLETE!\n")
   cat(rep("=", 80), "\n")
   
   # Display stage runtimes
   cat("\nStage Runtimes (minutes):\n")
   for(i in seq_along(stage_times)) {
      if(i <= length(stages)) {
         cat(sprintf("  Stage %d (%s): %.2f min\n", i, stages[[i]]$name, stage_times[i]))
      }
   }
   
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
   
   # Get the best model configuration
   best_model_id <- summary_table$model_id[1]
   best_result <- results[[best_model_id]]
   
   # Step 7: Refit the best model without stripping for final use
   cat("\nStep 7: Refitting best model with full components for final use...\n")
   cat(sprintf("Best configuration: %s_%s_%s (score: %.4f)\n", 
               best_result$strategy, best_result$weight_scheme, best_result$distribution, best_result$score))
   
   # Refit the best model without stripping
   final_gam_result <- fit_gam(
      data = data,
      linear_formula = linear_formula,
      linear_predictors = linear_predictors,
      strategy = best_result$strategy,
      weight = best_result$weight_scheme,
      distribution = best_result$distribution,
      weight_schemes = weight_schemes,
      gam_strategies = gam_strategies,
      distributions = distributions,
      salinity_threshold = salinity_threshold,
      stage_num = 99,  # Special stage number for final refit
      strip = FALSE    # Keep all model components
   )
   
   if(!is.null(final_gam_result)) {
      # Use the full model for final results
      best_result$model <- final_gam_result$result$model
      best_result$formula <- final_gam_result$result$formula
      cat("Best model successfully refitted with full components.\n")
   } else {
      cat("WARNING: Final refit failed, using stripped model from optimization.\n")
   }
   
   # Extract formula as character string
   if ("gam" %in% class(best_result$model)) {
      formula_char <- as.character(best_result$model$formula)[c(2,1,3)]
      formula_char <- paste(formula_char[c(2,1,3)], collapse = " ")
   } else {
      formula_char <- deparse(best_result$formula)
      if(length(formula_char) > 1) {
         formula_char <- paste(formula_char, collapse = " ")
      }
   }
   
   # Extract predictor names from the best model
   predictors <- all.vars(best_result$model$formula)[-1]  # Remove response variable
   
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
      stage_times = stage_times,
      total_runtime = sum(stage_times)
   )
   
   # Return structured output matching linear model builder format
   final_result <- list(
      model = best_result$model,
      formula = formula_char,
      predictors = predictors,
      evaluation = c(
         best_result[which(names(best_result) == 'overall_rmse') : which(names(best_result) == 'total_observations')],
         list(
            model_type = "gam",
            # Preserve GAM-specific information
            strategy = best_result$strategy,
            weight_scheme = best_result$weight_scheme,
            distribution = best_result$distribution
         )
      ),
      score = best_result$score,
      stage_results = stage_results,
      summary = list(                          
         total_predictors = length(predictors),
         final_score = best_result$score,
         model_type = "gam",                   
         build_method = "optimized_systematic_gam",
         strategy = best_result$strategy,
         weight_scheme = best_result$weight_scheme,
         distribution = best_result$distribution,
         n_models_tested = length(results),
         n_successful_fits = sum(sapply(results, function(x) !is.null(x$model))),
         total_runtime_minutes = sum(stage_times),
         average_models_per_minute = length(results) / sum(stage_times)
      )
   )
   
   class(final_result) <- "gam_model_builder_result"
   
   cat(sprintf("\nOptimization complete! Tested %d models in %.2f minutes (%.1f models/min)\n",
               length(results), sum(stage_times), length(results) / sum(stage_times)))
   
   return(final_result)
}