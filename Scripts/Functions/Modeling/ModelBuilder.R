# This function systematically builds the best model for salinity prediction

model_builder <- function(data, salinity_threshold) {
   
   cat("STARTING SYSTEMATIC MODEL BUILDING PROCESS\n")
   cat("==========================================\n")
   
   empty_formula <- 'Salinity ~'
   
   # Stage 0: Test tide predictors
   stage0_tides <- test_predictor_group(base_formula = empty_formula, predictor_config$tide, data, 'tide')
   
   if (!is.na(stage0_tides$best_predictor)) {
      base_formula <- paste(empty_formula, stage0_tides$best_predictor)
   } else {
      base_formula <- empty_formula
   }
   
   cat(sprintf("Base formula for subsequent stages: %s\n", base_formula))
   
   # Stage 1: Test discharge lag predictors
   stage1_discharge_lag <- test_predictor_group(base_formula, predictor_config$discharge_lag, data, "discharge_lag")
   
   # Stage 2: Test discharge rolling predictors
   stage2_discharge_rolling <- test_predictor_group(base_formula, predictor_config$discharge_rolling, data, "discharge_rolling")
   
   # Stage 3: Test lagged inflow predictors
   stage3_inflow_lag <- test_predictor_group(base_formula, predictor_config$inflow_lag, data, "inflow_lag")
   
   # Stage 4: Test rolling inflow predictors
   stage4_inflow_rolling <- test_predictor_group(base_formula, predictor_config$inflow_rolling, data, "inflow_rolling")
   
   # Stage 5: Test latent flow predictors
   stage5_latent_flow <- test_predictor_group(base_formula, predictor_config$latent_flow, data, 'latent_flow')
   
   # Stage 5: Test combinations of best flow predictors
   best_flow_predictors <- c(
      stage0_tides$best_predictor,
      stage1_discharge_lag$best_predictor,
      stage2_discharge_rolling$best_predictor,
      stage3_inflow_lag$best_predictor,
      stage4_inflow_rolling$best_predictor,
      stage5_latent_flow$best_predictor
   )
   
   # Remove any NA values
   best_flow_predictors <- best_flow_predictors[!is.na(best_flow_predictors)]
   
   stage5_combinations <- test_predictor_combinations(base_formula, best_flow_predictors, data)
   
   # Determine best formula after flow combinations
   if (!is.na(stage5_combinations$best_combination)) {
      best_flow_formula <- stage5_combinations$results[[stage5_combinations$best_combination]]$formula
   } else {
      # Fall back to base formula with best individual predictor
      if (length(best_flow_predictors) > 0) {
         best_flow_formula <- paste(base_formula, "+", best_flow_predictors[1])
      } else {
         best_flow_formula <- base_formula
      }
   }
   
   # Stage 6: Add binary stress predictors to best combination
   stage6_stress <- test_predictor_group(
      best_flow_formula,
      predictor_config$stress_binary,
      data,
      "stress_binary"
   )
   
   # Stage 7: Add continuous stress predictors to the best combination
   stage7_stress <- test_predictor_group(
      best_flow_formula,
      predictor_config$stress_continuous,
      data,
      "stress_continuous"
   )
   
   # Stage 8: Test combinations of best stress predictors with best flow combination
   best_stress_predictors <- c(
      stage6_stress$best_predictor,
      stage7_stress$best_predictor
   )
   
   best_stress_predictors <- best_stress_predictors[!is.na(best_stress_predictors)]
   stage8_stress_combinations <- test_predictor_combinations(best_flow_formula, best_stress_predictors, data)
  
   # Determine best formula after stress combinations
   if (!is.na(stage8_stress_combinations$best_combination)) {
      best_with_stress_formula <- stage8_stress_combinations$results[[stage8_stress_combinations$best_combination]]$formula
   } else {
      # Fall back to adding best individual stress predictor
      if (length(best_stress_predictors) > 0) {
         best_with_stress_formula <- paste(best_flow_formula, "+", best_stress_predictors[1])
      } else {
         best_with_stress_formula <- best_flow_formula
      }
   }
   
   # Stage 9: Add temporal predictors
   stage9_temporal <- test_predictor_group(best_with_stress_formula, predictor_config$temporal, data, "temporal")
   
   # Determine best formula after temporal predictors
   if (!is.na(stage9_temporal$best_predictor)) {
      best_main_effects_formula <- stage9_temporal$results[[stage9_temporal$best_predictor]]$formula
   } else {
      best_main_effects_formula <- best_with_stress_formula
   }
   
   # Stage 10: Test interactions
   stage10_interactions <- test_interactions(best_main_effects_formula, predictor_config$interactions, data)
   
   # Determine final best model with proper fallback logic
   final_best_model <- NULL
   final_best_formula <- best_main_effects_formula
   final_best_score <- -Inf
   # Create a list of all candidates with their scores
   candidates <- list()
   
   if (!is.na(stage10_interactions$best_interaction)) {
      candidates[["interactions"]] <- list(
         model = stage10_interactions$models[[stage10_interactions$best_interaction]],
         formula = stage10_interactions$results[[stage10_interactions$best_interaction]]$formula,
         score = stage10_interactions$best_score
      )
   }
   
   if (!is.na(stage9_temporal$best_predictor)) {
      candidates[["temporal"]] <- list(
         model = stage9_temporal$models[[stage9_temporal$best_predictor]],
         formula = stage9_temporal$results[[stage9_temporal$best_predictor]]$formula,
         score = stage9_temporal$best_score
      )
   }
   
   if (!is.na(stage8_stress_combinations$best_combination)) {
      candidates[["stress_combo"]] <- list(
         model = stage8_stress_combinations$models[[stage8_stress_combinations$best_combination]],
         formula = stage8_stress_combinations$results[[stage8_stress_combinations$best_combination]]$formula,
         score = stage8_stress_combinations$best_score
      )
   }
   
   if (!is.na(stage5_combinations$best_combination)) {
      candidates[["flow_combo"]] <- list(
         model = stage5_combinations$models[[stage5_combinations$best_combination]],
         formula = stage5_combinations$results[[stage5_combinations$best_combination]]$formula,
         score = stage5_combinations$best_score
      )
   }
   
   # Find the best candidate
   if (length(candidates) > 0) {
      best_candidate <- names(candidates)[which.max(sapply(candidates, function(x) x$score))]
      final_best_model <- candidates[[best_candidate]]$model
      final_best_formula <- candidates[[best_candidate]]$formula
      final_best_score <- candidates[[best_candidate]]$score
      
      cat(sprintf("Selected final model from stage: %s\n", best_candidate))
   } else {
      cat("Warning: No combinations performed well, using fallback model\n")
   }
   
   # Compile final results
   final_results <- list(
      stage0_tides = stage0_tides,
      stage1_discharge_lag = stage1_discharge_lag,
      stage2_discharge_rolling = stage2_discharge_rolling,
      stage3_inflow_lag = stage3_inflow_lag,
      stage4_inflow_rolling = stage4_inflow_rolling,
      stage5_combinations = stage5_combinations,
      stage6_stress_binary = stage6_stress,
      stage7_stress_continuous = stage7_stress,
      stage8_stress_combinations = stage8_stress_combinations,
      stage9_temporal = stage9_temporal,
      stage10_interactions = stage10_interactions,
      
      # Final best model
      final_best_model = final_best_model,
      final_best_formula = final_best_formula,
      final_best_score = final_best_score
   )
   
   cat("\n=== FINAL RESULTS ===\n")
   cat(sprintf("Best final model formula: %s\n", final_results$final_best_formula))
   cat(sprintf("Best final score: %.3f\n", final_results$final_best_score))
   
   return(final_results)
}