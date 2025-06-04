# This function systematically builds the best model for salinity prediction

model_builder <- function(data, salinity_threshold) {
   
   cat("STARTING SYSTEMATIC MODEL BUILDING PROCESS\n")
   cat("==========================================\n")
   
   # Stage 1: Test discharge lag predictors
   base_formula <- "Salinity ~ Norm_Tide"
   
   stage1_discharge_lag <- test_predictor_group(
      base_formula, 
      predictor_config$discharge_lag, 
      data, 
      "discharge_lag"
   )
   
   # Stage 2: Test discharge rolling predictors
   stage2_discharge_rolling <- test_predictor_group(
      base_formula,
      predictor_config$discharge_rolling,
      data,
      "discharge_rolling"
   )
   
   # Stage 3: Test inflow predictors
   stage3_inflow_lag <- test_predictor_group(
      base_formula,
      predictor_config$inflow_lag,
      data,
      "inflow_lag"
   )
   
   stage4_inflow_rolling <- test_predictor_group(
      base_formula,
      predictor_config$inflow_rolling,
      data,
      "inflow_rolling"
   )
   
   # Stage 5: Test combinations of best flow predictors
   best_flow_predictors <- c(
      stage1_discharge_lag$best_predictor,
      stage2_discharge_rolling$best_predictor,
      stage3_inflow_lag$best_predictor,
      stage4_inflow_rolling$best_predictor
   )
   
   stage5_combinations <- test_predictor_combinations(
      base_formula,
      best_flow_predictors,
      data
   )
   
   # Remove any NA values
   best_flow_predictors <- best_flow_predictors[!is.na(best_flow_predictors)]
   
   # Stage 6: Add stress predictors to best combination
   if (!is.na(stage5_combinations$best_combination)) {
      best_combination_formula <- stage5_combinations$results[[stage5_combinations$best_combination]]$formula
   } else {
      # Fall back to base formula with best individual predictor
      if (length(best_flow_predictors) > 0) {
         best_combination_formula <- paste(base_formula, "+", best_flow_predictors[1])
      } else {
         best_combination_formula <- base_formula
      }
   }
   
   stage6_stress <- test_predictor_group(
      best_combination_formula,
      predictor_config$stress_binary,
      data,
      "stress_binary"
   )
   
   # Stage 7: Add temporal predictors
   if (!is.na(stage6_stress$best_predictor)) {
      best_with_stress_formula <- stage6_stress$results[[stage6_stress$best_predictor]]$formula
   } else {
      best_with_stress_formula <- best_combination_formula
   }
   
   stage7_temporal <- test_predictor_group(
      best_with_stress_formula,
      predictor_config$temporal,
      data,
      "temporal"
   )
   
   # Stage 8: Test interactions
   if (!is.na(stage7_temporal$best_predictor)) {
      best_main_effects_formula <- stage7_temporal$results[[stage7_temporal$best_predictor]]$formula
   } else {
      best_main_effects_formula <- best_with_stress_formula
   }
   
   stage8_interactions <- test_interactions(
      best_main_effects_formula,
      predictor_config$interactions,
      data
   )
   
   # Determine final best model
   final_best_model <- NULL
   final_best_formula <- best_main_effects_formula
   final_best_score <- -Inf
   
   if (!is.na(stage8_interactions$best_interaction)) {
      final_best_model <- stage8_interactions$models[[stage8_interactions$best_interaction]]
      final_best_formula <- stage8_interactions$results[[stage8_interactions$best_interaction]]$formula
      final_best_score <- stage8_interactions$best_score
   } else if (!is.na(stage7_temporal$best_predictor)) {
      final_best_model <- stage7_temporal$models[[stage7_temporal$best_predictor]]
      final_best_formula <- stage7_temporal$results[[stage7_temporal$best_predictor]]$formula
      final_best_score <- stage7_temporal$best_score
   } else if (!is.na(stage6_stress$best_predictor)) {
      final_best_model <- stage6_stress$models[[stage6_stress$best_predictor]]
      final_best_formula <- stage6_stress$results[[stage6_stress$best_predictor]]$formula
      final_best_score <- stage6_stress$best_score
   }
   
   # Compile final results
   final_results <- list(
      stage1_discharge_lag = stage1_discharge_lag,
      stage2_discharge_rolling = stage2_discharge_rolling,
      stage3_inflow_lag = stage3_inflow_lag,
      stage4_inflow_rolling = stage4_inflow_rolling,
      stage5_combinations = stage5_combinations,
      stage6_stress = stage6_stress,
      stage7_temporal = stage7_temporal,
      stage8_interactions = stage8_interactions,
      
      # Final best model
      final_best_model = stage8_interactions$models[[stage8_interactions$best_interaction]],
      final_best_formula = stage8_interactions$results[[stage8_interactions$best_interaction]]$formula,
      final_best_score = stage8_interactions$best_score
   )
   
   cat("\n=== FINAL RESULTS ===\n")
   cat(sprintf("Best final model formula: %s\n", final_results$final_best_formula))
   cat(sprintf("Best final score: %.3f\n", final_results$final_best_score))
   
   return(final_results)
}