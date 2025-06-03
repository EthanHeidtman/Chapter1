### SYSTEMATIC MODEL BUILDING FRAMEWORK ###
# This framework automates the model building process while maintaining 
# your visual inspection workflow

# =======================================================================================
# 1. DEFINE MODEL BUILDING CONFIGURATION
# =======================================================================================

# Define predictor categories and their candidates
predictor_config <- list(
   
   # Base predictors (always included)
   base = c("Norm_Tide"),
   
   # Discharge predictors (test systematically)
   discharge_lag = c("Norm_PowLagDischarge1", "Norm_PowLagDischarge3", "Norm_PowLagDischarge6", 
                     "Norm_PowLagDischarge10", "Norm_PowLagDischarge12", "Norm_PowLagDischarge24",
                     "Norm_PowLagDischarge36", "Norm_PowLagDischarge48", "Norm_PowLagDischarge72"),
   
   discharge_rolling = c("Norm_RollingPowDischarge0.5", "Norm_RollingPowDischarge1", 
                         "Norm_RollingPowDischarge2", "Norm_RollingPowDischarge4",
                         "Norm_RollingPowDischarge7", "Norm_RollingPowDischarge10", 
                         "Norm_RollingPowDischarge14"),
   
   # Inflow predictors
   inflow_lag = c("Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72"),
   
   inflow_rolling = c("Norm_RollingPowInflows1", "Norm_RollingPowInflows2", 
                      "Norm_RollingPowInflows7", "Norm_RollingPowInflows10"),
   
   # Latent flow features
   latent_flow = c("Norm_SimpleLatent", "Norm_StressLatent", "Norm_BestLatent"),
   
   # Stress indicators
   stress_binary = c("IsModerateStress", "IsHighStress", "IsFlush", "IsStressed"),
   stress_continuous = c("Norm_StressHours_7day_Marietta", "Norm_StressHours_14day_Marietta", 
                         "Norm_StressHours_30day_Marietta", "Norm_CumulativeStress_7day_Marietta"),
   
   # Seasonal/temporal
   temporal = c("SalinitySeason", "DayOfYear"),
   
   # Pre-defined interaction candidates (based on physical understanding)
   interactions = list(
      "discharge_tide" = c("Norm_PowLagDischarge12", "Norm_Tide"),
      "inflow_tide" = c("Norm_RollingPowInflows2", "Norm_Tide"),
      "discharge_stress" = c("Norm_PowLagDischarge12", "IsHighStress"),
      "discharge_season" = c("Norm_PowLagDischarge12", "SalinitySeason"),
      "tide_season" = c("Norm_Tide", "SalinitySeason")
   )
)

# Define performance criteria and weights
performance_criteria <- list(
   weights = c(
      high_sal_rmse = 0.4,      # Primary concern: high salinity accuracy
      high_sal_r2 = 0.2,        # High salinity explanation
      overall_r2 = 0.2,         # Overall model fit
      overall_rmse = 0.1,       # Overall accuracy
      parsimony = 0.1           # Prefer simpler models
   ),
   
   # Minimum performance thresholds
   thresholds = list(
      min_high_sal_count = 20,    # Need sufficient high salinity events
      min_overall_r2 = 0.3,       # Minimum explanatory power
      max_predictors = 15         # Avoid overfitting
   )
)

# =======================================================================================
# 2. SYSTEMATIC MODEL BUILDING FUNCTIONS
# =======================================================================================

# Function to calculate composite performance score
calculate_performance_score <- function(model_results, weights = performance_criteria$weights) {
   
   # Handle missing values
   if (is.na(model_results$high_salinity_rmse) || model_results$high_salinity_count < performance_criteria$thresholds$min_high_sal_count) {
      return(-Inf)  # Penalize models that can't predict high salinity
   }
   
   # Calculate individual components (normalize to 0-1 scale where higher is better)
   high_sal_rmse_score <- 1 / (1 + model_results$high_salinity_rmse)  # Lower RMSE is better
   high_sal_r2_score <- pmax(0, model_results$high_salinity_r2)       # Higher R2 is better
   overall_r2_score <- pmax(0, model_results$overall_r2)              # Higher R2 is better
   overall_rmse_score <- 1 / (1 + model_results$overall_rmse)         # Lower RMSE is better
   
   # Parsimony score (fewer predictors is better, estimated from model complexity)
   n_predictors <- length(attr(terms(model_results$model), "term.labels"))
   parsimony_score <- exp(-n_predictors / 10)  # Exponential penalty for complexity
   
   # Weighted composite score
   composite_score <- (
      weights["high_sal_rmse"] * high_sal_rmse_score +
         weights["high_sal_r2"] * high_sal_r2_score +
         weights["overall_r2"] * overall_r2_score +
         weights["overall_rmse"] * overall_rmse_score +
         weights["parsimony"] * parsimony_score
   )
   
   return(composite_score)
}

# Function to test predictor group systematically
test_predictor_group <- function(base_formula, predictor_group, data, group_name) {
   
   cat(sprintf("\n=== TESTING %s PREDICTORS ===\n", toupper(group_name)))
   
   models <- list()
   results_list <- list()
   
   # Test each predictor individually
   for (i in seq_along(predictor_group)) {
      predictor <- predictor_group[i]
      
      # Skip if predictor doesn't exist in data
      if (!predictor %in% names(data)) {
         cat(sprintf("Warning: %s not found in data, skipping\n", predictor))
         next
      }
      
      # Build formula
      formula_str <- paste(base_formula, "+", predictor)
      
      # Fit model
      tryCatch({
         model <- lm(as.formula(formula_str), data = data)
         models[[predictor]] <- model
         
         # Evaluate model
         eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
         eval_result$model <- model
         eval_result$formula <- formula_str
         eval_result$score <- calculate_performance_score(eval_result)
         
         results_list[[predictor]] <- eval_result
         
         cat(sprintf("%s: Score = %.3f, High Sal RMSE = %.3f, Overall R2 = %.3f\n", 
                     predictor, eval_result$score, eval_result$high_salinity_rmse, eval_result$overall_r2))
         
      }, error = function(e) {
         cat(sprintf("Error fitting model with %s: %s\n", predictor, e$message))
      })
   }
   
   # Rank results by performance score
   scores <- sapply(results_list, function(x) x$score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   # Return results
   return(list(
      group_name = group_name,
      models = models,
      results = results_list,
      ranked_predictors = names(scores)[ranked_indices],
      best_predictor = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]],
      summary_table = data.frame(
         Predictor = names(scores)[ranked_indices],
         Score = scores[ranked_indices],
         High_Sal_RMSE = sapply(results_list[ranked_indices], function(x) x$high_salinity_rmse),
         Overall_R2 = sapply(results_list[ranked_indices], function(x) x$overall_r2),
         stringsAsFactors = FALSE
      )
   ))
}

# Function to test combinations of best predictors
test_predictor_combinations <- function(base_formula, predictor_list, data, max_combinations = 10) {
   
   cat("\n=== TESTING PREDICTOR COMBINATIONS ===\n")
   
   # Generate combinations (start with pairs, then triplets, etc.)
   models <- list()
   results_list <- list()
   
   # Test pairwise combinations
   for (i in 1:(length(predictor_list)-1)) {
      for (j in (i+1):length(predictor_list)) {
         
         combo_name <- paste(predictor_list[i], predictor_list[j], sep = "_+_")
         formula_str <- paste(base_formula, "+", predictor_list[i], "+", predictor_list[j])
         
         tryCatch({
            model <- lm(as.formula(formula_str), data = data)
            models[[combo_name]] <- model
            
            eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
            eval_result$model <- model
            eval_result$formula <- formula_str
            eval_result$score <- calculate_performance_score(eval_result)
            
            results_list[[combo_name]] <- eval_result
            
            cat(sprintf("%s: Score = %.3f\n", combo_name, eval_result$score))
            
         }, error = function(e) {
            cat(sprintf("Error with combination %s: %s\n", combo_name, e$message))
         })
      }
   }
   
   # Test best triplet combinations (top 5 pairs + next best individual)
   if (length(results_list) > 0) {
      scores <- sapply(results_list, function(x) x$score)
      top_pairs <- names(sort(scores, decreasing = TRUE))[1:min(3, length(scores))]
      
      for (pair_name in top_pairs) {
         for (additional_pred in predictor_list) {
            
            # Check if this predictor is already in the pair
            if (grepl(additional_pred, pair_name, fixed = TRUE)) next
            
            triplet_name <- paste(pair_name, additional_pred, sep = "_+_")
            current_formula <- results_list[[pair_name]]$formula
            formula_str <- paste(current_formula, "+", additional_pred)
            
            tryCatch({
               model <- lm(as.formula(formula_str), data = data)
               models[[triplet_name]] <- model
               
               eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
               eval_result$model <- model
               eval_result$formula <- formula_str
               eval_result$score <- calculate_performance_score(eval_result)
               
               results_list[[triplet_name]] <- eval_result
               
               cat(sprintf("%s: Score = %.3f\n", triplet_name, eval_result$score))
               
            }, error = function(e) {
               cat(sprintf("Error with triplet %s: %s\n", triplet_name, e$message))
            })
         }
      }
   }
   
   # Return ranked results
   scores <- sapply(results_list, function(x) x$score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   return(list(
      models = models,
      results = results_list,
      ranked_combinations = names(scores)[ranked_indices],
      best_combination = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]],
      summary_table = data.frame(
         Combination = names(scores)[ranked_indices],
         Score = scores[ranked_indices],
         High_Sal_RMSE = sapply(results_list[ranked_indices], function(x) x$high_salinity_rmse),
         Overall_R2 = sapply(results_list[ranked_indices], function(x) x$overall_r2),
         stringsAsFactors = FALSE
      )
   ))
}

# Function to test interactions systematically
test_interactions <- function(base_model_formula, interaction_list, data) {
   
   cat("\n=== TESTING INTERACTION TERMS ===\n")
   
   models <- list()
   results_list <- list()
   
   for (interaction_name in names(interaction_list)) {
      predictors <- interaction_list[[interaction_name]]
      
      # Create interaction term
      interaction_term <- paste(predictors, collapse = " * ")
      formula_str <- paste(base_model_formula, "+", interaction_term)
      
      tryCatch({
         model <- lm(as.formula(formula_str), data = data)
         models[[interaction_name]] <- model
         
         eval_result <- evaluate_model(model, data, salinity_threshold, "linear")
         eval_result$model <- model
         eval_result$formula <- formula_str
         eval_result$score <- calculate_performance_score(eval_result)
         
         results_list[[interaction_name]] <- eval_result
         
         cat(sprintf("%s: Score = %.3f\n", interaction_name, eval_result$score))
         
      }, error = function(e) {
         cat(sprintf("Error with interaction %s: %s\n", interaction_name, e$message))
      })
   }
   
   # Return results
   scores <- sapply(results_list, function(x) x$score)
   ranked_indices <- order(scores, decreasing = TRUE)
   
   return(list(
      models = models,
      results = results_list,
      ranked_interactions = names(scores)[ranked_indices],
      best_interaction = names(scores)[ranked_indices[1]],
      best_score = scores[ranked_indices[1]]
   ))
}

# =======================================================================================
# 3. MAIN SYSTEMATIC BUILDING WORKFLOW
# =======================================================================================

systematic_model_building <- function(data, salinity_threshold) {
   
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
   
   # Stage 6: Add stress predictors to best combination
   best_combination_formula <- stage5_combinations$results[[stage5_combinations$best_combination]]$formula
   
   stage6_stress <- test_predictor_group(
      best_combination_formula,
      predictor_config$stress_binary,
      data,
      "stress_binary"
   )
   
   # Stage 7: Add temporal predictors
   best_with_stress_formula <- stage6_stress$results[[stage6_stress$best_predictor]]$formula
   
   stage7_temporal <- test_predictor_group(
      best_with_stress_formula,
      predictor_config$temporal,
      data,
      "temporal"
   )
   
   # Stage 8: Test interactions
   best_main_effects_formula <- stage7_temporal$results[[stage7_temporal$best_predictor]]$formula
   
   stage8_interactions <- test_interactions(
      best_main_effects_formula,
      predictor_config$interactions,
      data
   )
   
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

# =======================================================================================
# 4. VISUALIZATION AND DIAGNOSTICS
# =======================================================================================

# Function to create summary plots for each stage
plot_stage_results <- function(stage_results, stage_name) {
   
   library(ggplot2)
   
   # Performance comparison plot
   p1 <- ggplot(stage_results$summary_table, aes(x = reorder(Predictor, Score), y = Score)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("Performance Scores:", stage_name),
           x = "Predictor", y = "Composite Score") +
      theme_minimal()
   
   # High salinity RMSE comparison
   p2 <- ggplot(stage_results$summary_table, aes(x = reorder(Predictor, -High_Sal_RMSE), y = High_Sal_RMSE)) +
      geom_col(fill = "coral", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("High Salinity RMSE:", stage_name),
           x = "Predictor", y = "High Salinity RMSE") +
      theme_minimal()
   
   return(list(score_plot = p1, rmse_plot = p2))
}

# =======================================================================================
# 5. USAGE EXAMPLE
# =======================================================================================

# Run the systematic model building
results <- systematic_model_building(model_data, salinity_threshold)

# Generate plots for each stage
# stage1_plots <- plot_stage_results(results$stage1_discharge_lag, "Discharge Lag")
# print(stage1_plots$score_plot)
# print(stage1_plots$rmse_plot)