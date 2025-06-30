parallel_gam_model_builder <- function(data, linear_formula, linear_predictors, response_var, salinity_threshold,
                              use_ar = FALSE, ar_order = 1, use_qgam = FALSE, quantile = 0.5, ncores = 10,
                              time_var = "DateTime", group_var = "Year") {
   
   # Load required packages and set the parallelization strategy
   library(purrr)
   library(furrr)
   library(dplyr)
   library(tibble)
   
   # Store original plan and set up parallel processing
   original_plan <- future::plan()
   on.exit(future::plan(original_plan), add = TRUE)
   
   future::plan(future::multisession, workers = ncores)
   
   cat("======= Generalized Additive Model Approach =======\n")
   cat("Starting from the best linear model to better capture salinity events\n\n")
   
   # Step 1: Create different weighting schemes
   cat("Step 1: Creating weighting schemes for extreme events...\n")
   weight_schemes <- list(
      'none' = NULL,
      "quantile" = func_env$create_weight_schemes(data[[response_var]], "quantile_progressive"),
      #"exponential" = create_weight_schemes(data[[response_var]], "exponential"),
      #"binary" = create_weight_schemes(data[[response_var]], "binary_extreme"),
      'ar_event_sequence' = func_env$create_weight_schemes(data[[response_var]], 'ar_event_sequence', time_var = 'DateTime', data = data),
      'ar_gradient' = func_env$create_weight_schemes(data[[response_var]], 'ar_gradient', time_var = 'DateTime', data = data),
      'ar_buildup' = func_env$create_weight_schemes(data[[response_var]], 'ar_buildup', time_var = 'DateTime', data = data)
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
      "mixed_interactions" = "mixed_interactions"  # Strategic mix of smooth and tensor terms
   )
   
   # Step 3: Define alternative distributions
   cat("Step 3: Defining alternative distributions...\n")
   distributions <- list(
      "gaussian" = gaussian(),                                
      "gamma" = Gamma(link = "log")                         
      #"tweedie" = tw()                                       # Exponential Tweedie family distributions (more flexible)
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
   
   # Optimized parallel fitting function
   fit_model_parallel <- function(combo_row, stage_num, weight_schemes, gam_strategies, distributions) {
      tryCatch({
         combo <- combo_row
         model_id <- paste(combo$strategy, combo$weight_scheme, combo$distribution, sep = "_")
         
         result <- fit_gam(
            data = data,
            linear_formula = linear_formula,
            linear_predictors = linear_predictors,
            strategy = combo$strategy,
            weight = combo$weight_scheme,
            distribution = combo$distribution,
            weight_schemes = weight_schemes,
            gam_strategies = gam_strategies,
            distributions = distributions,
            salinity_threshold = salinity_threshold,
            stage_num = stage_num,
            strip = TRUE,
            use_ar = use_ar,
            ar_order = ar_order,
            use_qgam = use_qgam,
            quantile = quantile,
            time_var = time_var,
            group_var = group_var
         )
         
         if (!is.null(result)) {
            result$result$model_id <- model_id
            return(result$result)
         }
         return(NULL)
      }, error = function(e) {
         # Silent error handling to avoid cluttering parallel output
         return(NULL)
      })
   }
   
   # Step 5: Systematic model fitting
   cat("Step 5: Systematic model fitting...\n\n")
   
   for (stage_idx in seq_along(stages)) {
      stage <- stages[[stage_idx]]
      stage_start <- Sys.time()
      cat(sprintf("\n=== STAGE %d: %s ===\n", stage$stage_num, stage$name))
      
      # Memory monitoring
      mem_usage <- sum(gc()[, 2])
      cat(sprintf("Memory usage before stage %d: %.2f MB\n", stage$stage_num, mem_usage))
      
      # Handle stage progression logic
      if (stage$stage_num == 2) {
         # Select top strategies from stage 1 (top 3 or within 10% of best)
         if (length(all_performance) > 0) {
            stage1_perf <- all_performance[["Strategy Screening"]]
            best_score <- max(stage1_perf$score, na.rm = TRUE)
            top_strategies <- stage1_perf %>%
               filter(score >= best_score * 0.9) %>%
               pull(strategy) %>%
               unique()
            stage$strategies <- top_strategies
            cat(sprintf("Using top strategies from Stage 1: %s\n", paste(top_strategies, collapse = ", ")))
         } else {
            cat("No valid results from Stage 1, skipping Stage 2\n")
            next
         }
      } else if (stage$stage_num == 3) {
         # Select best strategy-distribution combo from stage 2
         if (length(all_performance) >= 2) {
            stage2_perf <- all_performance[["Distribution Testing"]]
            best_combo <- stage2_perf[which.max(stage2_perf$score), ]
            stage$strategies <- best_combo$strategy
            stage$distributions <- best_combo$distribution
            cat(sprintf("Using best combo from Stage 2: %s + %s\n", 
                        best_combo$strategy, best_combo$distribution))
         } else {
            cat("No valid results from Stage 2, skipping Stage 3\n")
            next
         }
      }
      
      # Skip if configuration is incomplete (shouldn't happen for stage 1)
      if(is.null(stage$strategies) || is.null(stage$distributions)) {
         cat("Skipping stage due to incomplete configuration\n")
         next
      }
      
      # Create all combinations for this stage
      combinations <- expand.grid(
         strategy = stage$strategies,
         weight_scheme = stage$weights,
         distribution = stage$distributions,
         stringsAsFactors = FALSE
      )
      
      total_models <- nrow(combinations)
      cat(sprintf("Fitting %d models in parallel for Stage %d...\n", total_models, stage$stage_num))
      
      # Fit all models for current stage
      stage_results <- list()
      model_counter <- 0
   
      # PARALLEL PROCESSING: Use future_pmap for optimal parallel performance
      stage_results <- combinations %>%
         split(seq(nrow(.))) %>%
         future_map(fit_model_parallel, 
                    stage_num = stage$stage_num,
                    weight_schemes = weight_schemes,
                    gam_strategies = gam_strategies,
                    distributions = distributions,
                    .options = furrr_options(seed = TRUE)) %>%
         compact()  # Remove NULL results
      
      # Store results
      for (res in stage_results) {
         if (!is.null(res)) {
            results[[res$model_id]] <- res
         }
      }
      
      # Memory management: keep only top models
      if (length(results) > 50) {
         scores <- sapply(results, `[[`, "score")
         keep_indices <- order(scores, decreasing = TRUE)[1:max_stored_models]
         results <- results[keep_indices]
         gc()  # Force garbage collection
      }
      
      # Calculate stage performance
      if (length(stage_results) > 0) {
         perf <- tibble(
            model_id = sapply(stage_results, `[[`, "model_id"),
            strategy = sapply(stage_results, `[[`, "strategy"),
            weight_scheme = sapply(stage_results, `[[`, "weight_scheme"),
            distribution = sapply(stage_results, `[[`, "distribution"),
            score = sapply(stage_results, `[[`, "score"),
            stage = stage$stage_num
         ) %>% arrange(desc(score))
         
         all_performance[[stage$name]] <- perf
         
         # Stage timing
         stage_times[stage$stage_num] <- as.numeric(difftime(Sys.time(), stage_start, units = "mins"))
         
         # Stage summary
         successful_fits <- sum(!is.na(perf$score))
         top_score <- max(perf$score, na.rm = TRUE)
         
         cat(sprintf("Stage %d complete: %d/%d successful fits in %.2f minutes\n", 
                     stage$stage_num, successful_fits, total_models, stage_times[stage$stage_num]))
         cat(sprintf("Top score: %.4f (%s)\n", top_score, perf$model_id[which.max(perf$score)]))
         cat(sprintf("Processing rate: %.2f models/minute\n", 
                     total_models / stage_times[stage$stage_num]))
      } else {
         cat(sprintf("Stage %d: No successful model fits\n", stage$stage_num))
         stage_times[stage$stage_num] <- as.numeric(difftime(Sys.time(), stage_start, units = "mins"))
      }
      
   }
   
   # Final summary
   cat("\n", rep("=", 80), "\n")
   cat("ALL STAGES COMPLETE!\n")
   cat(rep("=", 80), "\n")
   
   # Overall timing summary
   total_runtime <- sum(stage_times, na.rm = TRUE)
   total_models_tested <- length(results)
   
   cat(sprintf("\nOverall Performance:\n"))
   cat(sprintf("  Total runtime: %.2f minutes\n", total_runtime))
   cat(sprintf("  Total models tested: %d\n", total_models_tested))
   cat(sprintf("  Average processing rate: %.2f models/minute\n", 
               total_models_tested / total_runtime))
   
   cat("\nStage Breakdown:\n")
   for (i in seq_along(stage_times)) {
      if (i <= length(stages) && !is.na(stage_times[i])) {
         stage_models <- if (length(all_performance) >= i) nrow(all_performance[[i]]) else 0
         cat(sprintf("  Stage %d (%s): %d models in %.2f min (%.2f models/min)\n", 
                     i, stages[[i]]$name, stage_models, stage_times[i],
                     stage_models / stage_times[i]))
      }
   }
   
   # Display the best models from each stage
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
   
   # Find overall best model
   if (length(results) > 0) {
      cat("\nStep 6: Identifying best overall model...\n")
      
      all_scores <- sapply(results, function(x) x$score)
      best_model_id <- names(results)[which.max(all_scores)]
      best_result <- results[[best_model_id]]
      
      cat(sprintf("Best configuration: %s (score: %.4f)\n", best_model_id, best_result$score))
      
      # Refit best model with full components
      cat("\nStep 7: Refitting best model with full components for final use...\n")
      
      final_gam_result <- tryCatch({
         fit_gam(
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
            strip = FALSE,
            use_ar = use_ar,
            ar_order = ar_order,
            use_qgam = use_qgam,
            quantile = quantile,
            time_var = time_var,
            group_var = group_var
         )
      }, error = function(e) {
         cat("WARNING: Final refit failed:", e$message, "\n")
         return(NULL)
      })
      
      if (!is.null(final_gam_result)) {
         best_result$model <- final_gam_result$result$model
         best_result$formula <- final_gam_result$result$formula
         cat("Best model successfully refitted with full components.\n")
      } else {
         cat("Using stripped model from optimization phase.\n")
      }
      
      # Extract final model information
      formula_char <- if ("gam" %in% class(best_result$model)) {
         paste(deparse(best_result$model$formula), collapse = " ")
      } else {
         paste(deparse(best_result$formula), collapse = " ")
      }
      
      predictors <- all.vars(best_result$model$formula)[-1]
      
      # Compile final results
      summary_table <- data.frame(
         model_id = names(results),
         strategy = sapply(results, `[[`, "strategy"),
         weights = sapply(results, `[[`, "weight_scheme"),
         distribution = sapply(results, `[[`, "distribution"),
         score = sapply(results, `[[`, "score"),
         stringsAsFactors = FALSE
      ) %>% arrange(desc(score))
      
      cat("\nTop 10 performing models overall:\n")
      print(head(summary_table, 10))
      
      # Create comprehensive results object
      stage_results_summary <- list(
         weight_schemes = names(weight_schemes),
         gam_strategies = names(gam_strategies),
         distributions = names(distributions),
         total_combinations_possible = length(weight_schemes) * length(gam_strategies) * length(distributions),
         total_combinations_tested = nrow(summary_table),
         successful_fits = sum(!is.na(summary_table$score)),
         failed_fits = sum(is.na(summary_table$score)),
         all_results_table = summary_table,
         stage_times = stage_times,
         total_runtime = total_runtime,
         processing_efficiency = total_models_tested / total_runtime,
         stage_performance = all_performance,
         ar_settings = if (use_ar) list(use_ar = use_ar, ar_order = ar_order) else NULL,
         qgam_settings = if (use_qgam) list(use_qgam = use_qgam, quantile = quantile) else NULL
      )
      
      final_result <- list(
         model = best_result$model,
         formula = formula_char,
         predictors = predictors,
         evaluation = c(
            best_result[which(names(best_result) == 'overall_rmse') : which(names(best_result) == 'total_observations')],
            list(
               model_type = "gam",
               strategy = best_result$strategy,
               weight_scheme = best_result$weight_scheme,
               distribution = best_result$distribution,
               quantile = if (use_qgam) best_result$quantile else NA,
               ar_order = if (use_ar) best_result$ar_order else NA
            )
         ),
         score = best_result$score,
         stage_results = stage_results_summary,
         summary = list(
            total_predictors = length(predictors),
            final_score = best_result$score,
            model_type = "gam",
            build_method = "optimized_systematic_gam",
            strategy = best_result$strategy,
            weight_scheme = best_result$weight_scheme,
            distribution = best_result$distribution,
            n_models_tested = total_models_tested,
            n_successful_fits = sum(!is.na(summary_table$score)),
            total_runtime_minutes = total_runtime,
            average_models_per_minute = total_models_tested / total_runtime,
            ar_enabled = use_ar,
            qgam_enabled = use_qgam,
            parallel_cores = ncores
         )
      )
      
      class(final_result) <- "gam_model_builder_result"
      
      #cat(sprintf("\n%s\n", paste(rep("=", 50), collapse = "")))
      cat(sprintf("OPTIMIZATION COMPLETE!\n"))
      cat(sprintf("Tested %d models in %.2f minutes using %d cores\n", 
                  total_models_tested, total_runtime, ncores))
      cat(sprintf("Processing rate: %.2f models/minute\n", total_models_tested / total_runtime))
      cat(sprintf("Best model: %s (score: %.4f)\n", best_model_id, best_result$score))
      #cat(sprintf(rep("=", 50) %s% "\n"))
      
      return(final_result)
      
   } else {
      cat("ERROR: No successful model fits across all stages\n")
      return(NULL)
   }
}