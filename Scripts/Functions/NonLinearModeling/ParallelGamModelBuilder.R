gam_model_builder <- function(data, linear_formula, linear_predictors, response_var, salinity_threshold,
                              use_ar = FALSE, ar_order = 1, use_qgam = FALSE, quantile = 0.5,
                              time_var = "DateTime", group_var = "Year") {
   
   # Load required packages and set the parallelization strategy
   library(purrr)
   library(furrr)
   plan(multisession)
   
   cat("======= Generalized Additive Model Approach =======\n")
   cat("Starting from the best linear model to better capture salinity events\n\n")
   
   # Step 1: Create different weighting schemes
   cat("Step 1: Creating weighting schemes for extreme events...\n")
   weight_schemes <- list(
      'none' = NULL,
      "quantile" = create_weight_schemes(data[[response_var]], "quantile_progressive"),
      #"exponential" = create_weight_schemes(data[[response_var]], "exponential"),
      #"binary" = create_weight_schemes(data[[response_var]], "binary_extreme"),
      'ar_event_sequence' = create_weight_schemes(data[[response_var]], 'ar_event_sequence', time_var = 'DateTime', data = data),
      'ar_gradient' = create_weight_schemes(data[[response_var]], 'ar_gradient', time_var = 'DateTime', data = data),
      'ar_buildup' = create_weight_schemes(data[[response_var]], 'ar_buildup', time_var = 'DateTime', data = data)
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
   
      combinations <- expand.grid(
         strategy = stage$strategies,
         weight_scheme = stage$weight_schemes,
         distribution = stage$distributions,
         stringsAsFactors = FALSE
      )
      
      combs <- split(combinations, seq(nrow(combinations)))
      
      fit_all <- function(combo_row) {
         combo <- combo_row[[1]]
         id <- paste(combo$strategy, combo$weight_scheme, combo$distribution, sep = "_")
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
            result$result$model_id <- id
            return(result$result)
         }
         return(NULL)
      }
      
      fits <- future_pmap(combs, fit_all)
      stage_results <- compact(fits)
      
      for (res in stage_results) {
         results[[res$model_id]] <- res
      }
      
      perf <- tibble::tibble(
         model_id = names(results),
         strategy = sapply(results, `[[`, "strategy"),
         weight_scheme = sapply(results, `[[`, "weight_scheme"),
         distribution = sapply(results, `[[`, "distribution"),
         score = sapply(results, `[[`, "score"),
         stage = stage_num
      )
      perf <- perf[order(perf$score, decreasing = TRUE), ]
      all_performance[[stage$name]] <- perf
      
      top_score <- perf$score[1]
      cat(sprintf("Top model score for Stage %d: %.4f\n", stage_num, top_score))
      stage_times[stage_num] <- as.numeric(difftime(Sys.time(), stage_start, units = "mins"))
   }
   
   # Final summary
   cat("\n", rep("=", 80), "\n")
   cat("ALL STAGES COMPLETE!\n")
   cat(rep("=", 80), "\n")
   
   cat("\nStage Runtimes (minutes):\n")
   for(i in seq_along(stage_times)) {
      if(i <= length(stages)) {
         cat(sprintf("  Stage %d (%s): %.2f min\n", i, stages[[i]]$name, stage_times[i]))
      }
   }
   
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
   
   cat("\nStep 6: Model fitting complete. Summarizing results...\n")
   
   summary_table <- data.frame(
      model_id = names(results),
      strategy = sapply(results, `[[`, "strategy"),
      weights = sapply(results, `[[`, "weight_scheme"),
      distribution = sapply(results, `[[`, "distribution"),
      score = sapply(results, `[[`, "score"),
      stringsAsFactors = FALSE
   )
   summary_table <- summary_table[order(summary_table$score, decreasing = TRUE), ]
   
   cat("\nTop 10 performing models:\n")
   print(head(summary_table, 10))
   
   best_model_id <- summary_table$model_id[1]
   best_result <- results[[best_model_id]]
   
   cat("\nStep 7: Refitting best model with full components for final use...\n")
   cat(sprintf("Best configuration: %s_%s_%s (score: %.4f)\n",
               best_result$strategy, best_result$weight_scheme, best_result$distribution, best_result$score))
   
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
      stage_num = 99,
      strip = FALSE,
      use_ar = use_ar,
      ar_order = ar_order,
      use_qgam = use_qgam,
      quantile = quantile,
      time_var = time_var,
      group_var = group_var
   )
   
   if(!is.null(final_gam_result)) {
      best_result$model <- final_gam_result$result$model
      best_result$formula <- final_gam_result$result$formula
      cat("Best model successfully refitted with full components.\n")
   } else {
      cat("WARNING: Final refit failed, using stripped model from optimization.\n")
   }
   
   formula_char <- if("gam" %in% class(best_result$model)) {
      f <- as.character(best_result$model$formula)[c(2,1,3)]
      paste(f[c(2,1,3)], collapse = " ")
   } else {
      f <- deparse(best_result$formula)
      paste(f, collapse = " ")
   }
   
   predictors <- all.vars(best_result$model$formula)[-1]
   
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
      performance_by_model_type = if("model_type" %in% names(summary_table)) {
         aggregate(score ~ model_type, data = summary_table, FUN = mean, na.rm = TRUE)
      } else NULL,
      all_results_table = summary_table,
      stage_times = stage_times,
      total_runtime = sum(stage_times),
      ar_settings = if(use_ar) list(use_ar = use_ar, ar_order = ar_order) else NULL,
      qgam_settings = if(use_qgam) list(use_qgam = use_qgam, quantile = quantile) else NULL
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
            quantile = if(use_qgam) best_result$quantile else NA,
            ar_order = if(use_ar) best_result$ar_order else NA
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
         average_models_per_minute = length(results) / sum(stage_times),
         ar_enabled = use_ar,
         qgam_enabled = use_qgam
      )
   )
   class(final_result) <- "gam_model_builder_result"
   
   cat(sprintf("\nOptimization complete! Tested %d models in %.2f minutes (%.1f models/min)\n",
               length(results), sum(stage_times), length(results) / sum(stage_times)))
   
   return(final_result)
}

