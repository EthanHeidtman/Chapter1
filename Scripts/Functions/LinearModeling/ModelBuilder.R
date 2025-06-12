# This function systematically builds the best model for salinity prediction

linear_model_builder <- function(data, salinity_threshold) {
   
   cat("STARTING SYSTEMATIC MODEL BUILDING PROCESS\n")
   cat("==========================================\n")
   
   # Initialize
   current_formula <- 'Salinity ~'
   current_predictors <- character(0)
   
   # Stage 0: Test tide predictors
   stage0_tides <- test_predictor_group(current_formula, predictor_config$tide, data, 'tide')
   
   if (!is.na(stage0_tides$best_predictor)) {
      current_predictors <- c(current_predictors, stage0_tides$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage0_tides$best_predictor))
   }
   cat(sprintf("Current formula: %s\n", current_formula))
   
   # Stage 1: Test discharge lag predictors
   stage1_discharge_lag <- test_predictor_group(current_formula, predictor_config$discharge_lag, data, "discharge_lag")
   
   if (!is.na(stage1_discharge_lag$best_predictor)) {
      current_predictors <- c(current_predictors, stage1_discharge_lag$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage1_discharge_lag$best_predictor))
   }
   cat(sprintf("Current formula: %s\n", current_formula))
   
   # Stage 2: Test discharge rolling predictors
   stage2_discharge_rolling <- test_predictor_group(current_formula, predictor_config$discharge_rolling, data, "discharge_rolling")
   
   if (!is.na(stage2_discharge_rolling$best_predictor)) {
      current_predictors <- c(current_predictors, stage2_discharge_rolling$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage2_discharge_rolling$best_predictor))
   }
   cat(sprintf("Current formula: %s\n", current_formula))
   
   # Stage 3: Test lagged inflow predictors
   stage3_inflow_lag <- test_predictor_group(current_formula, predictor_config$inflow_lag, data, "inflow_lag")
   
   if (!is.na(stage3_inflow_lag$best_predictor)) {
      current_predictors <- c(current_predictors, stage3_inflow_lag$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage3_inflow_lag$best_predictor))
   }
   cat(sprintf("Current formula: %s\n", current_formula))
   
   # Stage 4: Test rolling inflow predictors
   stage4_inflow_rolling <- test_predictor_group(current_formula, predictor_config$inflow_rolling, data, "inflow_rolling")
   
   if (!is.na(stage4_inflow_rolling$best_predictor)) {
      current_predictors <- c(current_predictors, stage4_inflow_rolling$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage4_inflow_rolling$best_predictor))
   }
   cat(sprintf("Current formula: %s\n", current_formula))
   
   # Stage 5: Test latent flow predictors
   stage5_latent_flow <- test_predictor_group(current_formula, predictor_config$latent_flow, data, 'latent_flow')
   
   if (!is.na(stage5_latent_flow$best_predictor)) {
      current_predictors <- c(current_predictors, stage5_latent_flow$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage5_latent_flow$best_predictor))
   }
   cat(sprintf("Current formula: %s\n", current_formula))
   
   # Stage 6: Add binary stress predictors to best combination
   stage6_stress <- test_predictor_group(current_formula, predictor_config$stress_binary, data, "stress_binary")
   
   if (!is.na(stage6_stress$best_predictor)) {
      current_predictors <- c(current_predictors, stage6_stress$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage6_stress$best_predictor))
   }
   
   # Stage 7: Add continuous stress predictors to the best combination
   stage7_stress <- test_predictor_group(current_formula, predictor_config$stress_continuous, data, "stress_continuous")
   
   if (!is.na(stage7_stress$best_predictor)) {
      current_predictors <- c(current_predictors, stage7_stress$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage7_stress$best_predictor))
   }

   # Stage 8: Add temporal predictors
   stage8_temporal <- test_predictor_group(current_formula, predictor_config$temporal, data, "temporal")
   
   if (!is.na(stage8_temporal$best_predictor)) {
      current_predictors <- c(current_predictors, stage8_temporal$best_predictor)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage8_temporal$best_predictor))
   }
   
   # Stage 9: Test interactions between all of the best predictors
   best_predictors <- c(
      stage0_tides$best_predictor,
      stage1_discharge_lag$best_predictor,
      stage2_discharge_rolling$best_predictor,
      stage3_inflow_lag$best_predictor,
      stage4_inflow_rolling$best_predictor,
      stage5_latent_flow$best_predictor,
      stage6_stress$best_predictor,
      stage7_stress$best_predictor,
      stage8_temporal$best_predictor
   )
   
   # Remove NAs and get unique predictors
   best_predictors <- unique(best_predictors[!is.na(best_predictors)])
   
   stage9_interactions <- test_interactions(current_formula, best_predictors, data)
   
   if (!is.na(stage9_interactions$best_interaction)) {
      current_predictors <- c(current_predictors, stage9_interactions$best_interaction)
      current_formula <- paste("Salinity ~", paste(current_predictors, collapse = " + "))
      cat(sprintf("Added: %s\n", stage9_interactions$best_interaction))
   }
   
   # Store sequential results
   sequential_formula <- current_formula
   sequential_predictors <- current_predictors
   cat(sprintf("\nBest flow formula after sequential addition: %s\n", sequential_formula))
   
   # Stage 10: Test combinations of best flow predictors
   if (!is.na(stage9_interactions$best_interaction)) {
      best_predictors <- c(best_predictors, stage9_interactions$best_interaction)
   }
   
   # Remove any NA values
   best_predictors <- unique(best_predictors[!is.na(best_predictors)])
   
   # Initialize final variables with sequential results
   final_formula <- sequential_formula
   final_predictors <- sequential_predictors
   final_score <- NA
   
   # Test if the full sequential model is better than combined models
   if (length(best_predictors) > 1) {
      stage10_combinations <- test_predictor_combinations(base_formula = "Salinity ~", best_predictors, data)
      
      if (!is.na(stage10_combinations$best_combination)) {
         combo_formula <- stage10_combinations$results[[stage10_combinations$best_combination]]$formula
         combo_score <- stage10_combinations$best_score
         
         # Test current sequential formula score
         # Check for weird interaction names
         clean_sequential_formula <- gsub('_x_', ' * ', sequential_formula)
         
         sequential_model <- lm(as.formula(clean_sequential_formula), data = data)
         sequential_result <- evaluate_model(sequential_model, data, salinity_threshold, "linear")
         sequential_result$model <- sequential_model
         sequential_result$formula <- clean_sequential_formula
         sequential_result$score <- performance_score(sequential_result)
         sequential_score <- sequential_result$score
         
         cat(sprintf("Sequential score: %.3f, Combination score: %.3f\n", sequential_score, combo_score))
         
         # Choose the better model
         if (combo_score > sequential_score) {
            final_formula <- combo_formula
            final_predictors <- trimws(strsplit(stage10_combinations[['best_combination']], "\\+")[[1]])
            final_score <- combo_score
            cat(sprintf("Combination model is better! Final formula: %s\n", final_formula))
         } else {
            final_formula <- clean_sequential_formula
            final_predictors <- sequential_predictors
            final_score <- sequential_score
            cat("Sequential model is better, keeping it.\n")
         }
      } else {
         # No valid combinations found, use sequential model
         final_formula <- gsub('_x_', ' * ', sequential_formula)
         final_predictors <- sequential_predictors
         cat("No better combinations found, using sequential model.\n")
      }
   } else {
      # Not enough predictors for combination testing
      final_formula <- gsub('_x_', ' * ', sequential_formula)
      final_predictors <- sequential_predictors
      cat("Not enough predictors for combination testing, using sequential model.\n")
   }
   
   # Build and evaluate final model
   cat(sprintf("\nFinal formula: %s\n", final_formula))
   final_model <- lm(as.formula(final_formula), data = data)
   final_evaluation <- evaluate_model(final_model, data, salinity_threshold, 'linear')
   
   # Calculate final score if not already done
   if (is.na(final_score)) {
      final_score <- performance_score(final_evaluation)
   }
   
   # Compile final results
   final_results <- list(
      model = final_model,
      formula = final_formula,
      predictors = final_predictors,
      evaluation = final_evaluation,
      score = final_score,
      stage_results = list(
         tide = stage0_tides,
         discharge_lag = stage1_discharge_lag,
         discharge_rolling = stage2_discharge_rolling,
         inflow_lag = stage3_inflow_lag,
         inflow_rolling = stage4_inflow_rolling,
         latent_flow = stage5_latent_flow,
         stress_binary = stage6_stress,
         stress_continuous = stage7_stress,
         temporal = stage8_temporal,
         interactions = stage9_interactions,
         combinations = if(exists("stage10_combinations")) stage10_combinations else NULL
      ),
      summary = list(
         total_predictors = length(final_predictors),
         final_score = final_score,
         model_type = "linear",
         build_method = if(exists("stage10_combinations") && !is.na(stage10_combinations$best_combination) && final_score == stage10_combinations$best_score) "combination" else "sequential"
      )
   )
   
   cat("\n==========================================\n")
   cat("MODEL BUILDING PROCESS COMPLETED\n")
   cat(sprintf("Final score: %.3f\n", final_score))
   cat(sprintf("Total predictors: %d\n", length(final_predictors)))
   cat("==========================================\n")
   
   return(final_results)
}