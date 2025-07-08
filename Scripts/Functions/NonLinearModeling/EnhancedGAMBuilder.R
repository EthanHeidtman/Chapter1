enhanced_parallel_gam_model_builder <- function(data, best_predictors, best_interactions, response_var, 
                                                salinity_threshold, use_ar = FALSE, ar_order = 1, 
                                                use_qgam = FALSE, quantile = 0.5, ncores = 10,
                                                time_var = "DateTime", group_var = "Year",
                                                max_predictors = 5, max_interactions = 2,
                                                max_stored_models = 50) {
   
   # Load required packages and set the parallelization strategy
   library(purrr)
   library(furrr)
   library(dplyr)
   library(tibble)
   
   # Store original plan and set up parallel processing
   original_plan <- future::plan()
   on.exit(future::plan(original_plan), add = TRUE)
   
   future::plan(future::multisession, workers = ncores)
   
   cat("======= Enhanced Generalized Additive Model Builder =======\n")
   cat("Building GAM models from best predictors and interactions\n\n")
   
   # Input validation
   if (length(best_predictors) == 0) {
      stop("best_predictors cannot be empty")
   }
   
   cat(sprintf("Input: %d best predictors, %d best interactions\n", 
               length(best_predictors), length(best_interactions)))
   cat("Best predictors:", paste(best_predictors, collapse = ", "), "\n")
   if (length(best_interactions) > 0) {
      cat("Best interactions:", paste(best_interactions, collapse = ", "), "\n")
   }
   
   # Step 1: Generate predictor combinations
   cat("\nStep 1: Generating predictor combinations...\n")
   
   # Create combinations of predictors (1 to max_predictors)
   predictor_combinations <- list()
   for (i in 1:min(max_predictors, length(best_predictors))) {
      combos <- combn(best_predictors, i, simplify = FALSE)
      predictor_combinations <- c(predictor_combinations, combos)
   }
   
   # Add interaction combinations if available
   interaction_combinations <- list()
   if (length(best_interactions) > 0) {
      for (i in 1:min(max_interactions, length(best_interactions))) {
         int_combos <- combn(best_interactions, i, simplify = FALSE)
         interaction_combinations <- c(interaction_combinations, int_combos)
      }
   }
   
   # Generate all formula combinations
   formula_combinations <- list()
   combo_id <- 1
   
   # Base predictor combinations (no interactions)
   for (pred_combo in predictor_combinations) {
      formula_combinations[[combo_id]] <- list(
         id = combo_id,
         predictors = pred_combo,
         interactions = character(0),
         n_predictors = length(pred_combo),
         n_interactions = 0
      )
      combo_id <- combo_id + 1
   }
   
   # Predictor + interaction combinations
   if (length(interaction_combinations) > 0) {
      for (pred_combo in predictor_combinations) {
         for (int_combo in interaction_combinations) {
            formula_combinations[[combo_id]] <- list(
               id = combo_id,
               predictors = pred_combo,
               interactions = int_combo,
               n_predictors = length(pred_combo),
               n_interactions = length(int_combo)
            )
            combo_id <- combo_id + 1
         }
      }
   }
   
   cat(sprintf("Generated %d formula combinations\n", length(formula_combinations)))
   
   # Step 2: Create weighting schemes
   cat("\nStep 2: Creating weighting schemes for extreme events...\n")
   weight_schemes <- list(
      'none' = NULL,
      "quantile" = func_env$create_weight_schemes(data[[response_var]], "quantile_progressive"),
      'ar_event_sequence' = func_env$create_weight_schemes(data[[response_var]], 'ar_event_sequence', 
                                                           time_var = time_var, data = data),
      'ar_gradient' = func_env$create_weight_schemes(data[[response_var]], 'ar_gradient', 
                                                     time_var = time_var, data = data),
      'ar_buildup' = func_env$create_weight_schemes(data[[response_var]], 'ar_buildup', 
                                                    time_var = time_var, data = data)
   )
   
   # Step 3: Define GAM enhancement strategies
   cat("Step 3: Defining GAM enhancement strategies...\n")
   gam_strategies <- list(
      "linear" = "linear",                         # Linear terms only
      "smooth_all" = "smooth_all",                 # Smooth all continuous predictors
      "smooth_flow" = "smooth_flow",               # Smooth only flow variables
      "smooth_stress" = "smooth_stress",           # Smooth only stress variables
      "smooth_temporal" = "smooth_temporal",       # Smooth only temporal variables
      "tensor_pairs" = "tensor_pairs",             # Tensor products of predictor pairs
      "mixed_smooth_tensor" = "mixed_smooth_tensor", # Mix of smooth and tensor terms
      "hierarchical_smooth" = "hierarchical_smooth" # Hierarchical smoothing approach
   )
   
   # Step 4: Define alternative distributions
   cat("Step 4: Defining alternative distributions...\n")
   distributions <- list(
      "gaussian" = gaussian(),                                
      "gamma" = Gamma(link = "log")
   )
   
   # Step 5: Define testing phases
   cat("Step 5: Defining testing phases...\n")
   stages <- list(
      list(
         stage_num = 1,
         name = "Formula Screening",
         max_formulas = 20,  # Test top 20 formula combinations first
         strategies = c("linear", "smooth_all"),
         weights = "quantile",
         distributions = "gaussian"
      ),
      list(
         stage_num = 2,
         name = "Strategy Optimization",
         max_formulas = 10,  # Use top 10 formulas from stage 1
         strategies = names(gam_strategies),
         weights = "quantile",
         distributions = "gaussian"
      ),
      list(
         stage_num = 3,
         name = "Distribution Testing",
         max_formulas = 5,   # Use top 5 formula-strategy combos
         strategies = NULL,  # Will be filled from stage 2
         weights = "quantile",
         distributions = names(distributions)
      ),
      list(
         stage_num = 4,
         name = "Weight Optimization",
         max_formulas = 3,   # Use top 3 formula-strategy-distribution combos
         strategies = NULL,  # Will be filled from stage 3
         weights = names(weight_schemes),
         distributions = NULL  # Will be filled from stage 3
      )
   )
   
   # Helper function to create formula string
   create_formula_string <- function(formula_combo, strategy, response_var) {
      predictors <- formula_combo$predictors
      interactions <- formula_combo$interactions
      
      # Build formula based on strategy
      if (strategy == "linear") {
         # Linear terms only
         terms <- predictors
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else if (strategy == "smooth_all") {
         # Smooth all continuous predictors
         smooth_terms <- paste0("s(", predictors, ")")
         terms <- smooth_terms
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else if (strategy == "smooth_flow") {
         # Smooth only flow-related variables
         flow_vars <- predictors[grepl("Discharge|Inflow|Flow", predictors)]
         smooth_terms <- paste0("s(", flow_vars, ")")
         linear_terms <- setdiff(predictors, flow_vars)
         terms <- c(smooth_terms, linear_terms)
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else if (strategy == "smooth_stress") {
         # Smooth only stress-related variables
         stress_vars <- predictors[grepl("Stress|Stressed", predictors)]
         smooth_terms <- paste0("s(", stress_vars, ")")
         linear_terms <- setdiff(predictors, stress_vars)
         terms <- c(smooth_terms, linear_terms)
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else if (strategy == "smooth_temporal") {
         # Smooth only temporal variables
         temporal_vars <- predictors[grepl("Day|Season|Year", predictors)]
         smooth_terms <- paste0("s(", temporal_vars, ")")
         linear_terms <- setdiff(predictors, temporal_vars)
         terms <- c(smooth_terms, linear_terms)
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else if (strategy == "tensor_pairs") {
         # Create tensor products of predictor pairs
         if (length(predictors) >= 2) {
            pred_pairs <- combn(predictors, 2, simplify = FALSE)
            tensor_terms <- sapply(pred_pairs, function(pair) {
               paste0("te(", paste(pair, collapse = ", "), ")")
            })
            terms <- tensor_terms[1:min(3, length(tensor_terms))]  # Limit to 3 tensors
            if (length(interactions) > 0) {
               terms <- c(terms, interactions)
            }
         } else {
            terms <- paste0("s(", predictors, ")")
            if (length(interactions) > 0) {
               terms <- c(terms, interactions)
            }
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else if (strategy == "mixed_smooth_tensor") {
         # Mix of smooth and tensor terms
         smooth_terms <- paste0("s(", predictors[1:min(2, length(predictors))], ")")
         if (length(predictors) >= 3) {
            remaining_vars <- predictors[3:length(predictors)]
            if (length(remaining_vars) >= 2) {
               tensor_term <- paste0("te(", paste(remaining_vars[1:2], collapse = ", "), ")")
               terms <- c(smooth_terms, tensor_term)
            } else {
               terms <- c(smooth_terms, remaining_vars)
            }
         } else {
            terms <- smooth_terms
         }
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
         
      } else {
         # Default to smooth_all
         smooth_terms <- paste0("s(", predictors, ")")
         terms <- smooth_terms
         if (length(interactions) > 0) {
            terms <- c(terms, interactions)
         }
         formula_str <- paste(response_var, "~", paste(terms, collapse = " + "))
      }
      
      return(formula_str)
   }
   
   # Initialize results storage
   results <- list()
   all_performance <- list()
   stage_times <- numeric(length(stages))
   
   # Optimized parallel fitting function
   fit_gam_parallel <- function(combo_data, weight_schemes, distributions) {
      tryCatch({
         formula_str <- create_formula_string(combo_data$formula_combo, combo_data$strategy, response_var)
         model_id <- paste(combo_data$formula_combo$id, combo_data$strategy, 
                           combo_data$weight_scheme, combo_data$distribution, sep = "_")
         
         # Convert formula string to formula
         formula_obj <- as.formula(formula_str)
         
         # Get weights
         weights <- if (combo_data$weight_scheme == "none") {
            NULL
         } else {
            weight_schemes[[combo_data$weight_scheme]]
         }
         
         # Get distribution
         family <- distributions[[combo_data$distribution]]
         
         # Fit the GAM model
         if (use_ar) {
            library(mgcv)
            model <- gamm(formula_obj, data = data, weights = weights, family = family,
                          correlation = corAR1(form = ~ 1 | !!sym(group_var)))
            gam_model <- model$gam
         } else if (use_qgam) {
            library(qgam)
            model <- qgam(formula_obj, data = data, weights = weights, qu = quantile)
            gam_model <- model
         } else {
            library(mgcv)
            model <- gam(formula_obj, data = data, weights = weights, family = family)
            gam_model <- model
         }
         
         # Evaluate model performance
         predictions <- predict(gam_model, newdata = data, type = "response")
         
         # Calculate performance metrics
         overall_rmse <- sqrt(mean((data[[response_var]] - predictions)^2, na.rm = TRUE))
         overall_r2 <- cor(data[[response_var]], predictions, use = "complete.obs")^2
         
         # High salinity performance
         high_sal_mask <- data[[response_var]] > salinity_threshold
         if (sum(high_sal_mask, na.rm = TRUE) > 0) {
            high_sal_rmse <- sqrt(mean((data[[response_var]][high_sal_mask] - predictions[high_sal_mask])^2, na.rm = TRUE))
            high_sal_r2 <- cor(data[[response_var]][high_sal_mask], predictions[high_sal_mask], use = "complete.obs")^2
         } else {
            high_sal_rmse <- NA
            high_sal_r2 <- NA
         }
         
         # Calculate composite score
         score <- ifelse(is.na(high_sal_r2), 
                         overall_r2 * 0.7 + (1 - overall_rmse) * 0.3,
                         overall_r2 * 0.4 + high_sal_r2 * 0.4 + (1 - overall_rmse) * 0.2)
         
         # Return results
         return(list(
            model_id = model_id,
            formula_id = combo_data$formula_combo$id,
            strategy = combo_data$strategy,
            weight_scheme = combo_data$weight_scheme,
            distribution = combo_data$distribution,
            n_predictors = combo_data$formula_combo$n_predictors,
            n_interactions = combo_data$formula_combo$n_interactions,
            formula = formula_str,
            model = if (combo_data$stage_num < 4) NULL else gam_model,  # Only store final models
            overall_rmse = overall_rmse,
            overall_r2 = overall_r2,
            high_sal_rmse = high_sal_rmse,
            high_sal_r2 = high_sal_r2,
            score = score
         ))
         
      }, error = function(e) {
         return(NULL)
      })
   }
   
   # Step 6: Systematic model fitting
   cat("\nStep 6: Systematic model fitting...\n\n")
   
   for (stage_idx in seq_along(stages)) {
      stage <- stages[[stage_idx]]
      stage_start <- Sys.time()
      cat(sprintf("\n=== STAGE %d: %s ===\n", stage$stage_num, stage$name))
      
      # Select formulas for this stage
      if (stage$stage_num == 1) {
         # Use all formulas, but limit to max_formulas
         selected_formulas <- formula_combinations[1:min(stage$max_formulas, length(formula_combinations))]
      } else {
         # Select top formulas from previous stage
         prev_stage_name <- stages[[stage_idx - 1]]$name
         if (prev_stage_name %in% names(all_performance)) {
            prev_results <- all_performance[[prev_stage_name]]
            top_formula_ids <- prev_results %>%
               arrange(desc(score)) %>%
               head(stage$max_formulas) %>%
               pull(formula_id) %>%
               unique()
            selected_formulas <- formula_combinations[top_formula_ids]
         } else {
            cat("No results from previous stage, skipping\n")
            next
         }
      }
      
      # Handle stage progression for strategies and distributions
      if (stage$stage_num == 3) {
         # Select best strategy from stage 2
         stage2_results <- all_performance[["Strategy Optimization"]]
         best_strategy <- stage2_results %>%
            arrange(desc(score)) %>%
            head(1) %>%
            pull(strategy)
         stage$strategies <- best_strategy
      } else if (stage$stage_num == 4) {
         # Select best strategy-distribution combo from stage 3
         stage3_results <- all_performance[["Distribution Testing"]]
         best_combo <- stage3_results %>%
            arrange(desc(score)) %>%
            head(1)
         stage$strategies <- best_combo$strategy
         stage$distributions <- best_combo$distribution
      }
      
      # Create combinations for this stage
      combinations <- expand.grid(
         formula_idx = seq_along(selected_formulas),
         strategy = stage$strategies,
         weight_scheme = stage$weights,
         distribution = stage$distributions,
         stringsAsFactors = FALSE
      )
      
      # Prepare data for parallel processing
      combo_data_list <- list()
      for (i in seq_len(nrow(combinations))) {
         combo_data_list[[i]] <- list(
            formula_combo = selected_formulas[[combinations$formula_idx[i]]],
            strategy = combinations$strategy[i],
            weight_scheme = combinations$weight_scheme[i],
            distribution = combinations$distribution[i],
            stage_num = stage$stage_num
         )
      }
      
      total_models <- length(combo_data_list)
      cat(sprintf("Fitting %d models in parallel for Stage %d...\n", total_models, stage$stage_num))
      
      # Parallel model fitting
      stage_results <- combo_data_list %>%
         future_map(fit_gam_parallel, 
                    weight_schemes = weight_schemes,
                    distributions = distributions,
                    .options = furrr_options(seed = TRUE)) %>%
         compact()
      
      # Store results
      for (res in stage_results) {
         if (!is.null(res)) {
            results[[res$model_id]] <- res
         }
      }
      
      # Calculate stage performance
      if (length(stage_results) > 0) {
         perf <- tibble(
            model_id = sapply(stage_results, `[[`, "model_id"),
            formula_id = sapply(stage_results, `[[`, "formula_id"),
            strategy = sapply(stage_results, `[[`, "strategy"),
            weight_scheme = sapply(stage_results, `[[`, "weight_scheme"),
            distribution = sapply(stage_results, `[[`, "distribution"),
            n_predictors = sapply(stage_results, `[[`, "n_predictors"),
            n_interactions = sapply(stage_results, `[[`, "n_interactions"),
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
      }
   }
   
   # Final results compilation
   cat("\n", rep("=", 80), "\n")
   cat("ALL STAGES COMPLETE!\n")
   cat(rep("=", 80), "\n")
   
   if (length(results) > 0) {
      all_scores <- sapply(results, function(x) x$score)
      best_model_id <- names(results)[which.max(all_scores)]
      best_result <- results[[best_model_id]]
      
      cat(sprintf("\nBest configuration: %s (score: %.4f)\n", best_model_id, best_result$score))
      
      # Create summary table
      summary_table <- data.frame(
         model_id = names(results),
         formula_id = sapply(results, `[[`, "formula_id"),
         strategy = sapply(results, `[[`, "strategy"),
         weights = sapply(results, `[[`, "weight_scheme"),
         distribution = sapply(results, `[[`, "distribution"),
         n_predictors = sapply(results, `[[`, "n_predictors"),
         n_interactions = sapply(results, `[[`, "n_interactions"),
         score = sapply(results, `[[`, "score"),
         stringsAsFactors = FALSE
      ) %>% arrange(desc(score))
      
      cat("\nTop 10 performing models:\n")
      print(head(summary_table, 10))
      
      # Return comprehensive results
      return(list(
         best_model = best_result,
         all_results = results,
         summary_table = summary_table,
         stage_performance = all_performance,
         formula_combinations = formula_combinations,
         runtime_minutes = sum(stage_times, na.rm = TRUE),
         models_tested = length(results),
         processing_rate = length(results) / sum(stage_times, na.rm = TRUE)
      ))
   } else {
      cat("ERROR: No successful model fits\n")
      return(NULL)
   }
}