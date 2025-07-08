# This function systematically identifies the best predictors for salinity prediction
# Returns best predictors from each category and their interactions for GAM building

linear_predictor_selector <- function(data, salinity_threshold, predictor_config, performance_criteria) {
   
   cat("STARTING SYSTEMATIC PREDICTOR SELECTION PROCESS\n")
   cat("===============================================\n")
   
   # Initialize results storage
   best_predictors <- list()
   stage_results <- list()
   
   # Stage 0: Test tide predictors
   cat("Stage 0: Testing tide predictors...\n")
   stage0_tides <- test_predictor_group('Salinity ~', predictor_config$tide, data, 'tide')
   stage_results$tide <- stage0_tides
   
   if (!is.na(stage0_tides$best_predictor)) {
      best_predictors$tide <- stage0_tides$best_predictor
      cat(sprintf("Best tide predictor: %s (score: %.3f)\n", 
                  stage0_tides$best_predictor, stage0_tides$best_score))
   } else {
      cat("No significant tide predictor found\n")
   }
   
   # Stage 1: Test discharge lag predictors
   cat("\nStage 1: Testing discharge lag predictors...\n")
   stage1_discharge_lag <- test_predictor_group('Salinity ~', predictor_config$discharge_lag, data, "discharge_lag")
   stage_results$discharge_lag <- stage1_discharge_lag
   
   if (!is.na(stage1_discharge_lag$best_predictor)) {
      best_predictors$discharge_lag <- stage1_discharge_lag$best_predictor
      cat(sprintf("Best discharge lag predictor: %s (score: %.3f)\n", 
                  stage1_discharge_lag$best_predictor, stage1_discharge_lag$best_score))
   } else {
      cat("No significant discharge lag predictor found\n")
   }
   
   # Stage 2: Test discharge rolling predictors
   cat("\nStage 2: Testing discharge rolling predictors...\n")
   stage2_discharge_rolling <- test_predictor_group('Salinity ~', predictor_config$discharge_rolling, data, "discharge_rolling")
   stage_results$discharge_rolling <- stage2_discharge_rolling
   
   if (!is.na(stage2_discharge_rolling$best_predictor)) {
      best_predictors$discharge_rolling <- stage2_discharge_rolling$best_predictor
      cat(sprintf("Best discharge rolling predictor: %s (score: %.3f)\n", 
                  stage2_discharge_rolling$best_predictor, stage2_discharge_rolling$best_score))
   } else {
      cat("No significant discharge rolling predictor found\n")
   }
   
   # Stage 3: Test lagged inflow predictors
   cat("\nStage 3: Testing inflow lag predictors...\n")
   stage3_inflow_lag <- test_predictor_group('Salinity ~', predictor_config$inflow_lag, data, "inflow_lag")
   stage_results$inflow_lag <- stage3_inflow_lag
   
   if (!is.na(stage3_inflow_lag$best_predictor)) {
      best_predictors$inflow_lag <- stage3_inflow_lag$best_predictor
      cat(sprintf("Best inflow lag predictor: %s (score: %.3f)\n", 
                  stage3_inflow_lag$best_predictor, stage3_inflow_lag$best_score))
   } else {
      cat("No significant inflow lag predictor found\n")
   }
   
   # Stage 4: Test rolling inflow predictors
   cat("\nStage 4: Testing inflow rolling predictors...\n")
   stage4_inflow_rolling <- test_predictor_group('Salinity ~', predictor_config$inflow_rolling, data, "inflow_rolling")
   stage_results$inflow_rolling <- stage4_inflow_rolling
   
   if (!is.na(stage4_inflow_rolling$best_predictor)) {
      best_predictors$inflow_rolling <- stage4_inflow_rolling$best_predictor
      cat(sprintf("Best inflow rolling predictor: %s (score: %.3f)\n", 
                  stage4_inflow_rolling$best_predictor, stage4_inflow_rolling$best_score))
   } else {
      cat("No significant inflow rolling predictor found\n")
   }
   
   # Stage 5: Test latent flow predictors
   cat("\nStage 5: Testing latent flow predictors...\n")
   stage5_latent_flow <- test_predictor_group('Salinity ~', predictor_config$latent_flow, data, 'latent_flow')
   stage_results$latent_flow <- stage5_latent_flow
   
   if (!is.na(stage5_latent_flow$best_predictor)) {
      best_predictors$latent_flow <- stage5_latent_flow$best_predictor
      cat(sprintf("Best latent flow predictor: %s (score: %.3f)\n", 
                  stage5_latent_flow$best_predictor, stage5_latent_flow$best_score))
   } else {
      cat("No significant latent flow predictor found\n")
   }
   
   # Stage 6: Test binary stress predictors
   cat("\nStage 6: Testing binary stress predictors...\n")
   stage6_stress <- test_predictor_group('Salinity ~', predictor_config$stress_binary, data, "stress_binary")
   stage_results$stress_binary <- stage6_stress
   
   if (!is.na(stage6_stress$best_predictor)) {
      best_predictors$stress_binary <- stage6_stress$best_predictor
      cat(sprintf("Best binary stress predictor: %s (score: %.3f)\n", 
                  stage6_stress$best_predictor, stage6_stress$best_score))
   } else {
      cat("No significant binary stress predictor found\n")
   }
   
   # Stage 7: Test continuous stress predictors
   cat("\nStage 7: Testing continuous stress predictors...\n")
   stage7_stress <- test_predictor_group('Salinity ~', predictor_config$stress_continuous, data, "stress_continuous")
   stage_results$stress_continuous <- stage7_stress
   
   if (!is.na(stage7_stress$best_predictor)) {
      best_predictors$stress_continuous <- stage7_stress$best_predictor
      cat(sprintf("Best continuous stress predictor: %s (score: %.3f)\n", 
                  stage7_stress$best_predictor, stage7_stress$best_score))
   } else {
      cat("No significant continuous stress predictor found\n")
   }
   
   # Stage 8: Test temporal predictors
   cat("\nStage 8: Testing temporal predictors...\n")
   stage8_temporal <- test_predictor_group('Salinity ~', predictor_config$temporal, data, "temporal")
   stage_results$temporal <- stage8_temporal
   
   if (!is.na(stage8_temporal$best_predictor)) {
      best_predictors$temporal <- stage8_temporal$best_predictor
      cat(sprintf("Best temporal predictor: %s (score: %.3f)\n", 
                  stage8_temporal$best_predictor, stage8_temporal$best_score))
   } else {
      cat("No significant temporal predictor found\n")
   }
   
   # Stage 9: Test interactions between all best predictors
   cat("\nStage 9: Testing interactions between best predictors...\n")
   
   # Collect all best predictors for interaction testing
   all_best_predictors <- unlist(best_predictors)
   all_best_predictors <- all_best_predictors[!is.na(all_best_predictors)]
   
   best_interactions <- list()
   stage_results$interactions <- list()
   
   if (length(all_best_predictors) > 1) {
      # Build base formula with all best predictors
      base_formula <- paste("Salinity ~", paste(all_best_predictors, collapse = " + "))
      
      stage9_interactions <- test_interactions(base_formula, all_best_predictors, data)
      stage_results$interactions <- stage9_interactions
      
      if (!is.na(stage9_interactions$best_interaction)) {
         best_interactions$main <- stage9_interactions$best_interaction
         cat(sprintf("Best interaction: %s (score: %.3f)\n", 
                     stage9_interactions$best_interaction, stage9_interactions$best_score))
      } else {
         cat("No significant interactions found\n")
      }
   } else {
      cat("Not enough predictors for interaction testing\n")
   }
   
   # Compile final results
   cat("\n===============================================\n")
   cat("PREDICTOR SELECTION PROCESS COMPLETED\n")
   cat("===============================================\n")
   
   # Summary of selected predictors
   cat("SELECTED PREDICTORS BY CATEGORY:\n")
   for (category in names(best_predictors)) {
      cat(sprintf("  %s: %s\n", category, best_predictors[[category]]))
   }
   
   if (length(best_interactions) > 0) {
      cat("SELECTED INTERACTIONS:\n")
      for (interaction in names(best_interactions)) {
         cat(sprintf("  %s: %s\n", interaction, best_interactions[[interaction]]))
      }
   }
   
   # Create flat vector of all selected predictors for easy GAM use
   selected_predictors <- unlist(best_predictors)
   selected_predictors <- selected_predictors[!is.na(selected_predictors)]
   
   selected_interactions_vec <- unlist(best_interactions)
   selected_interactions_vec <- selected_interactions_vec[!is.na(selected_interactions_vec)]
   
   cat(sprintf("\nTotal selected predictors: %d\n", length(selected_predictors)))
   cat(sprintf("Total selected interactions: %d\n", length(selected_interactions_vec)))
   
   results <- list(
      # Main results for GAM building
      predictors = list(
         by_category = best_predictors,
         interactions = best_interactions,
         all_predictors = selected_predictors,
         all_interactions = selected_interactions_vec,
         combined = c(selected_predictors, selected_interactions_vec)
      ),
      
      # Detailed stage results for analysis
      stage_results = stage_results,
      
      # Summary information
      summary = list(
         total_predictors = length(selected_predictors),
         total_interactions = length(selected_interactions_vec),
         categories_with_predictors = names(best_predictors),
         selection_method = "systematic_linear_testing"
      )
   )
   
   cat("===============================================\n")
   
   return(results)
}