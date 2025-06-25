fit_gam_worker <- function(strategy_name, weight_name, dist_name, 
                           data_df, response_col, formula_obj, predictors_vec, 
                           salinity_thresh, stage_number) {
   
   # Load required packages inside worker
   if (!requireNamespace("mgcv", quietly = TRUE)) {
      return(list(success = FALSE, error = "mgcv not available"))
   }
   
   tryCatch({
      # Reconstruct minimal objects inside worker - no external dependencies
      response_values <- data_df[[response_col]]
      
      # Simple weight creation functions (avoid external function calls)
      create_weights_local <- function(values, type) {
         if (type == "quantile") {
            q95 <- quantile(values, 0.95, na.rm = TRUE)
            weights <- ifelse(values >= q95, 2, 1)
         } else if (type == "exponential") {
            q90 <- quantile(values, 0.90, na.rm = TRUE)
            weights <- ifelse(values >= q90, exp((values - q90) / sd(values, na.rm = TRUE)), 1)
         } else if (type == "binary") {
            q95 <- quantile(values, 0.95, na.rm = TRUE)
            weights <- ifelse(values >= q95, 3, 1)
         } else {
            weights <- NULL
         }
         return(weights)
      }
      
      local_weight_schemes <- list(
         'none' = NULL,
         "quantile" = create_weights_local(response_values, "quantile"),
         "exponential" = create_weights_local(response_values, "exponential"),
         "binary" = create_weights_local(response_values, "binary")
      )
      
      local_gam_strategies <- list(
         "baseline" = "linear",
         "smooth_all" = "smooth_all",
         "smooth_flow" = "smooth_flow",
         "smooth_stress" = "smooth_stress",
         "smooth_tide" = "smooth_tide",
         "tensor_flow_stress" = "tensor",
         "tensor_flow_tide" = "tensor",
         "mixed_interactions" = "mixed_interactions"
      )
      
      local_distributions <- list(
         "gaussian" = gaussian(),
         "gamma" = Gamma(link = "log"),
         "tweedie" = tw(),
         "quasi" = quasi(link = "identity", variance = "mu^2"),
         "scat" = scat()
      )
      
      # Call your fit_gam function
      result <- fit_gam(
         strategy = strategy_name,
         weight = weight_name, 
         distribution = dist_name,
         data = data_df,
         linear_formula = formula_obj,
         linear_predictors = predictors_vec,
         weight_schemes = local_weight_schemes,
         gam_strategies = local_gam_strategies,
         distributions = local_distributions,
         salinity_threshold = salinity_thresh,
         stage_num = stage_number,
         strip = TRUE
      )
      
      # Return only essential information to minimize memory transfer
      if (!is.null(result) && !is.null(result$result)) {
         return(list(
            model_id = result$model_id,
            strategy = result$result$strategy,
            weight_scheme = result$result$weight_scheme,
            distribution = result$result$distribution,
            score = result$result$score,
            success = TRUE
         ))
      } else {
         return(list(success = FALSE, error = "fit_gam returned NULL"))
      }
      
   }, error = function(e) {
      return(list(success = FALSE, error = as.character(e$message)))
   })
}




parallel_gam_model_builder <- function(data, linear_model, response_var, salinity_threshold,
                                       workers = 6,
                                       parallel_plan = 'multisession') {
   
   cat("======= Generalized Additive Model Approach =======\n")
   cat("Starting from the best linear model to better capture salinity events\n\n")
   
   # Parallelization packages
   library(furrr)
   library(purrr)
   
   # Set up parallelization plan
   plan(multisession, workers = workers)
   options(future.globals.maxSize = 2 * 1024^3)  # 2 gb max
   
   # Extract and clean linear formula and predictors
   linear_formula <- formula(linear_model)
   environment(linear_formula) <- .GlobalEnv # detach the model from the model environment
   linear_predictors <- all.vars(linear_formula)[-1]
   
   # Create minimal data object - only necessary columns
   required_cols <- unique(c(response_var, linear_predictors))
   data_minimal <- data[, required_cols, drop = FALSE]
   
   # Step 1: Create weighting schemes
   cat("Step 1: Creating weighting schemes for extreme events...\n")
   weight_schemes <- list(
      'none' = NULL,
      "quantile" = create_extreme_weights(data[[response_var]], "quantile_progressive"),
      "exponential" = create_extreme_weights(data[[response_var]], "exponential"),
      "binary" = create_extreme_weights(data[[response_var]], "binary_extreme")
   )
   
   # Step 2: Define GAM strategies
   cat("Step 2: Defining GAM enhancement strategies...\n")
   gam_strategies <- list(
      "baseline" = "linear",
      "smooth_all" = "smooth_all",
      "smooth_flow" = "smooth_flow",
      "smooth_stress" = "smooth_stress",
      "smooth_tide" = "smooth_tide",
      "tensor_flow_stress" = "tensor",
      "tensor_flow_tide" = "tensor",
      "mixed_interactions" = "mixed_interactions"
   )
   
   # Step 3: Distributions
   cat("Step 3: Defining alternative distributions...\n")
   distributions <- list(
      "gaussian" = gaussian(),
      "gamma" = Gamma(link = "log"),
      "tweedie" = tw(),
      "quasi" = quasi(link = "identity", variance = "mu^2"),
      "scat" = scat()
   )
   
   # Step 4: Testing stages
   cat("Step 4: Defining testing phases...\n")
   stages <- list(
      list(stage_num = 1, 
           name = "Strategy Screening", 
           strategies = names(gam_strategies), 
           weights = "quantile", 
           distributions = "gaussian"),
      list(stage_num = 2, 
           name = "Distribution Testing", 
           strategies = NULL,
           weights = "quantile", 
           distributions = names(distributions)),
      list(stage_num = 3, 
           name = "Weight Scheme Testing", 
           strategies = NULL, 
           weights = names(weight_schemes), 
           distributions = NULL)
   )
   
   # Initialize scheduling and lists
   stage_scheduling <- c(1, Inf, 2)
   stage_times <- numeric(length(stages))
   results <- list()
   all_performance <- list()

   # Attempt parallel processing with minimal memory footprint
   for (stage_idx in seq_along(stages)) {
      stage <- stages[[stage_idx]]
      cat(sprintf("\n=== STAGE %d: %s ===\n", stage$stage_num, stage$name))
      
      if (is.null(stage$strategies) || is.null(stage$distributions)) {
         cat("Skipping stage due to incomplete configuration\n")
         next
      }
      
      # Create parameter combinations
      combos <- expand.grid(
         strategy = stage$strategies,
         weight = stage$weights,
         distribution = stage$distributions,
         stringsAsFactors = FALSE
      )
      
      total_models <- nrow(combos)
      cat(sprintf("Fitting %d models for Stage %d...\n", total_models, stage$stage_num))
      
      start_time <- Sys.time()
      model_results <- NULL
      
      # Try parallel processing with very strict controls
      tryCatch({
         plan(multisession, workers = min(workers, 2))
         options(future.globals.maxSize = 500 * 1024^2)  # Only 500MB
         
         # Use future_map with explicit globals specification
         model_results <- future_map(1:total_models, 
                                     function(i) {
                                        fit_gam_worker(
                                           strategy_name = combos$strategy[i],
                                           weight_name = combos$weight[i],
                                           dist_name = combos$distribution[i],
                                           data_df = data_minimal,
                                           response_col = response_var,
                                           formula_obj = linear_formula,
                                           predictors_vec = linear_predictors,
                                           salinity_thresh = salinity_threshold,
                                           stage_number = stage$stage_num
                                        )
                                     },
                                     .options = furrr_options(
                                        seed = TRUE, 
                                        scheduling = 1,
                                        globals = c("fit_gam_worker", "data_minimal", "linear_formula", 
                                                    "linear_predictors", "response_var", "salinity_threshold")
                                     )
         )
         
         plan(sequential)
         
      }, error = function(e) {
         cat("Parallel processing failed:", e$message, "\n")
         cat("Falling back to sequential processing...\n")
         plan(sequential)
         
         # Sequential processing
         model_results <- vector("list", total_models)
         for (i in 1:total_models) {
            cat("Processing model", i, "of", total_models, "\n")
            model_results[[i]] <- fit_gam_worker(
               strategy_name = combos$strategy[i],
               weight_name = combos$weight[i],
               dist_name = combos$distribution[i],
               data_df = data_minimal,
               response_col = response_var,
               formula_obj = linear_formula,
               predictors_vec = linear_predictors,
               salinity_thresh = salinity_threshold,
               stage_number = stage$stage_num
            )
            gc()
         }
      })
   
      stage_times[stage_idx] <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      
      # Process results
      successful_results <- model_results[sapply(model_results, function(x) isTRUE(x$success))]
      
      if (length(successful_results) == 0) {
         cat("ERROR: No models fitted successfully in this stage!\n")
         break
      }
      
      # Store results
      stage_results <- setNames(successful_results, sapply(successful_results, function(x) x$model_id))
      results <- c(results, stage_results)
      
      # Create performance summary
      stage_performance <- data.frame(
         model_id = sapply(successful_results, function(x) x$model_id),
         strategy = sapply(successful_results, function(x) x$strategy),
         weight_scheme = sapply(successful_results, function(x) x$weight_scheme),
         distribution = sapply(successful_results, function(x) x$distribution),
         score = sapply(successful_results, function(x) x$score),
         stage = stage$stage_num,
         stringsAsFactors = FALSE
      )
      stage_performance <- stage_performance[order(-stage_performance$score), ]
      all_performance[[paste0("stage_", stage$stage_num)]] <- stage_performance
      
      cat("Top performers for Stage", stage$stage_num, ":\n")
      top_n <- min(5, nrow(stage_performance))
      for (i in 1:top_n) {
         cat(sprintf("  %d. %s_%s_%s (score: %.4f)\n",
                     i,
                     stage_performance$strategy[i],
                     stage_performance$weight_scheme[i],
                     stage_performance$distribution[i],
                     stage_performance$score[i]))
      }
      
      # Configure next stage
      if (stage$stage_num == 1) {
         top_strategies <- head(aggregate(score ~ strategy, stage_performance, mean)[order(-aggregate(score ~ strategy, stage_performance, mean)$score), "strategy"], 3)
         stages[[2]]$strategies <- top_strategies
         cat(sprintf("\nSelected strategies for Stage 2: %s\n", paste(top_strategies, collapse = ", ")))
      } else if (stage$stage_num == 2) {
         top_combos <- head(stage_performance, 2)
         stages[[3]]$strategies <- unique(top_combos$strategy)
         stages[[3]]$distributions <- unique(top_combos$distribution)
         cat("Selected for Stage 3:\n")
         cat(sprintf("  Strategies: %s\n", paste(stages[[3]]$strategies, collapse = ", ")))
         cat(sprintf("  Distributions: %s\n", paste(stages[[3]]$distributions, collapse = ", ")))
      }
      
      gc()  # Clean up after each stage
   }
   
   
   # # Process each stage
   # for (stage_idx in seq_along(stages)) {
   #    stage <- stages[[stage_idx]]
   #    cat(sprintf("\n=== STAGE %d: %s ===\n", stage$stage_num, stage$name))
   #    
   #    if (is.null(stage$strategies) || is.null(stage$distributions)) {
   #       cat("Skipping stage due to incomplete configuration\n")
   #       next
   #    }
   #    
   #    # Create combinations for parallelization
   #    combos <- expand.grid(
   #       strategy = stage$strategies,
   #       weight = stage$weights,
   #       distribution = stage$distributions,
   #       stringsAsFactors = FALSE
   #    )
   #    
   #    total_models <- length(stage$strategies) * length(stage$weights) * length(stage$distributions)
   #    cat(sprintf("Fitting %d models for Stage %d...\n\n", total_models, stage$stage_num))
   #    
   #    # Track time for each stage
   #    start_time <- Sys.time()
   #    model_results <- NULL
   #    
   #    # Parallel model fitting
   #    # model_list <- future_pmap(combos, fit_one_model, .progress = TRUE, 
   #    #                           .options = furrr_options(seed = TRUE, scheduling = stage_scheduling[stage_idx]))
   #    model_list <- future_map(seq_len(nrow(combos)), function(i) {
   #       fit_gam(
   #          strategy = combos$strategy[i],
   #          weight = combos$weight[i], 
   #          distribution = combos$distribution[i],
   #          data = data,
   #          linear_formula = linear_formula,
   #          linear_predictors = linear_predictors,
   #          weight_schemes = weight_schemes,
   #          gam_strategies = gam_strategies,
   #          distributions = distributions,
   #          salinity_threshold = salinity_threshold,
   #          stage_num = stage$stage_num,
   #          strip = TRUE
   #       )
   #    }, .progress = TRUE, .options = furrr_options(seed = TRUE, scheduling = stage_scheduling[stage_idx]))
   #    
   #    stage_times[stage_idx] <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
   #    
   #    model_list <- compact(model_list)
   #    stage_results <- setNames(lapply(model_list, `[[`, "result"),
   #                              sapply(model_list, `[[`, "model_id"))
   #    results <- c(results, stage_results)
   #    
   #    # Evaluation
   #    cat(sprintf("\n=== STAGE %d ANALYSIS ===\n", stage$stage_num))
   #    if (length(stage_results) == 0) {
   #       cat("ERROR: No models fitted successfully in this stage!\n")
   #       break
   #    }
   #    
   #    stage_performance <- data.frame(
   #       model_id = names(stage_results),
   #       strategy = sapply(stage_results, function(x) x$strategy),
   #       weight_scheme = sapply(stage_results, function(x) x$weight_scheme),
   #       distribution = sapply(stage_results, function(x) x$distribution),
   #       score = sapply(stage_results, function(x) x$score),
   #       stage = stage$stage_num,
   #       stringsAsFactors = FALSE
   #    )
   #    stage_performance <- stage_performance[order(-stage_performance$score), ]
   #    all_performance[[paste0("stage_", stage$stage_num)]] <- stage_performance
   #    
   #    cat("Top performers for Stage", stage$stage_num, ":\n")
   #    top_n <- min(10, nrow(stage_performance))
   #    for (i in 1:top_n) {
   #       cat(sprintf("  %d. %s_%s_%s (score: %.4f)\n",
   #                   i,
   #                   stage_performance$strategy[i],
   #                   stage_performance$weight_scheme[i],
   #                   stage_performance$distribution[i],
   #                   stage_performance$score[i]))
   #    }
   #    
   #    # Next stage configuration
   #    if (stage$stage_num == 1) {
   #       top_strategies <- head(aggregate(score ~ strategy, stage_performance, mean)[order(-aggregate(score ~ strategy, stage_performance, mean)$score), "strategy"], 4)
   #       stages[[2]]$strategies <- top_strategies
   #       cat(sprintf("\nSelected strategies for Stage 2: %s\n", paste(top_strategies, collapse = ", ")))
   #    } else if (stage$stage_num == 2) {
   #       top_combos <- head(stage_performance, 2)
   #       stages[[3]]$strategies <- unique(top_combos$strategy)
   #       stages[[3]]$distributions <- unique(top_combos$distribution)
   #       cat("Selected for Stage 3:\n")
   #       cat(sprintf("  Strategies: %s\n", paste(stages[[3]]$strategies, collapse = ", ")))
   #       cat(sprintf("  Distributions: %s\n", paste(stages[[3]]$distributions, collapse = ", ")))
   #    }
   # }
   
   cat("\n", strrep("=", 80), "\nALL STAGES COMPLETE!\n", strrep("=", 80), "\n")
   cat("\nStage Runtimes (minutes):\n")
   for (i in seq_along(stage_times)) {
      cat(sprintf("  Stage %d (%s): %.2f min\n", i, stages[[i]]$name, stage_times[i]))
   }
   
   # Step 6: Summarize results and select best model
   cat("\nStep 6: Model fitting complete. Summarizing results...\n")
   summary_table <- data.frame(
      model_id = names(results),
      strategy = sapply(results, function(x) x$strategy),
      weights = sapply(results, function(x) x$weight_scheme),
      distribution = sapply(results, function(x) x$distribution),
      score = sapply(results, function(x) x$score),
      stringsAsFactors = FALSE
   )
   summary_table <- summary_table[order(summary_table$score, decreasing = TRUE), ]
   print(head(summary_table, 10))
   
   best_model_id <- summary_table$model_id[1]
   best_result <- results[[best_model_id]]
   
   # Refit the best performing model without stripping (strip)
   cat("Refitting best model with full components...\n")
   
   
   # Refit the best performing model (strip = FALSE)
   # Refit without stripping
   refit_result <- fit_gam(
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
      stage_num = 99,
      strip = FALSE
   )
   
   if (!is.null(refit_result)) {
      best_result$model <- refit_result$result$model
      best_result$formula <- refit_result$result$formula
   } else {
      warning("Refitting failed, using stripped model from optimization")
   }
   
   formula_char <- if ("gam" %in% class(best_result$model)) {
      fc <- as.character(best_result$model$formula)[c(2,1,3)]
      paste(fc[c(2,1,3)], collapse = " ")
   } else {
      f <- deparse(best_result$formula)
      if (length(f) > 1) paste(f, collapse = " ") else f
   }
   predictors <- all.vars(best_result$model$formula)[-1]
   
   # Gather all stage results
   stage_results <- list(
      weight_schemes = names(weight_schemes),
      gam_strategies = names(gam_strategies),
      distributions = names(distributions),
      total_combinations = length(weight_schemes) * length(gam_strategies) * length(distributions),
      successful_fits = summary_table$model_id[!is.na(summary_table$score)],
      performance_by_strategy = aggregate(score ~ strategy, data = summary_table, mean),
      performance_by_weights = aggregate(score ~ weights, data = summary_table, mean),
      performance_by_distribution = aggregate(score ~ distribution, data = summary_table, mean),
      all_results_table = summary_table,
      detailed_results = results
   )
   
   # Gather final result
   final_result <- list(
      model = best_result$model,
      formula = formula_char,
      predictors = predictors,
      evaluation = c(
         best_result[which(names(best_result) == 'overall_rmse') : which(names(best_result) == 'total_observations')],
         list(model_type = "gam", strategy = best_result$strategy,
              weight_scheme = best_result$weight_scheme, distribution = best_result$distribution)
      ),
      score = best_result$score,
      stage_results = stage_results,
      summary = list(
         total_predictors = length(predictors),
         final_score = best_result$score,
         model_type = "gam",
         build_method = "systematic_gam",
         strategy = best_result$strategy,
         weight_scheme = best_result$weight_scheme,
         distribution = best_result$distribution,
         n_models_tested = length(results),
         n_successful_fits = sum(sapply(results, function(x) !is.null(x$model)))
      )
   )
   
   class(final_result) <- "gam_model_builder_result"
   return(final_result)
}
