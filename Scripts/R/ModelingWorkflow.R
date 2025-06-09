### SYSTEMATIC MODEL BUILDING FRAMEWORK ###

# =======================================================================================
# 1. DEFINE MODEL BUILDING CONFIGURATION
# =======================================================================================




# =======================================================================================
# 3. MAIN SYSTEMATIC BUILDING WORKFLOW
# =======================================================================================


# =======================================================================================
# 4. VISUALIZATION AND DIAGNOSTICS
# =======================================================================================


# =======================================================================================
# 5. USAGE EXAMPLE
# =======================================================================================

# Run the systematic model building
results <- model_builder(model_data, salinity_threshold)

# Generate plots for each stage
stage1_plots <- plot_stage_results(results$stage5_combinations, "Discharge Lag")
print(stage1_plots$score_plot)
print(stage1_plots$rmse_plot)

test <- analyze_final_model(final_model = results$final_best_model, data = model_data, threshold = salinity_threshold)


# Function to get detailed diagnostics for the final model
analyze_final_model <- function(final_model, data, threshold) {
   
   cat("=== DETAILED DIAGNOSTIC ANALYSIS ===\n")
   
   # 1. Model Summary
   cat("\n1. MODEL SUMMARY:\n")
   print(summary(final_model))
   
   # 2. Get predictions and residuals
   predictions <- predict(final_model, newdata = data)
   residuals <- data$Salinity - predictions
   
   # 3. Detailed performance metrics
   cat("\n2. DETAILED PERFORMANCE METRICS:\n")
   eval_results <- evaluate_model(final_model, data, threshold, "linear")
   
   cat(sprintf("Overall RMSE: %.4f\n", eval_results$overall_rmse))
   cat(sprintf("Overall R²: %.4f\n", eval_results$overall_r2))
   cat(sprintf("High Salinity RMSE: %.4f\n", eval_results$high_salinity_rmse))
   cat(sprintf("High Salinity R²: %.4f\n", eval_results$high_salinity_r2))
   cat(sprintf("High Salinity Count: %d\n", eval_results$high_salinity_count))
   cat(sprintf("Hit Rate: %.3f\n", eval_results$hit_rate))
   cat(sprintf("False Alarm Rate: %.3f\n", eval_results$false_alarm_rate))
   
   # 4. Residual analysis
   cat("\n3. RESIDUAL ANALYSIS:\n")
   residual_stats <- list(
      mean_residual = mean(residuals, na.rm = TRUE),
      median_residual = median(residuals, na.rm = TRUE),
      residual_sd = sd(residuals, na.rm = TRUE),
      residual_range = range(residuals, na.rm = TRUE)
   )
   
   cat(sprintf("Mean Residual: %.4f\n", residual_stats$mean_residual))
   cat(sprintf("Median Residual: %.4f\n", residual_stats$median_residual))
   cat(sprintf("Residual SD: %.4f\n", residual_stats$residual_sd))
   cat(sprintf("Residual Range: [%.4f, %.4f]\n", 
               residual_stats$residual_range[1], residual_stats$residual_range[2]))
   
   # 5. Variable importance (based on t-statistics)
   cat("\n4. VARIABLE IMPORTANCE (|t-statistics|):\n")
   coef_summary <- summary(final_model)$coefficients
   t_stats <- abs(coef_summary[, "t value"])
   importance_order <- order(t_stats, decreasing = TRUE)
   
   for (i in importance_order) {
      cat(sprintf("%-30s: |t| = %.3f, p = %.4f\n", 
                  rownames(coef_summary)[i], t_stats[i], coef_summary[i, "Pr(>|t|)"]))
   }
   
   return(list(
      eval_results = eval_results,
      residual_stats = residual_stats,
      variable_importance = t_stats[importance_order],
      predictions = predictions,
      residuals = residuals
   ))
}

recommend_next_steps <- function(diagnostic_results, threshold) {
   
   cat("\n=== NEXT STEPS RECOMMENDATIONS ===\n")
   
   # 1. Assess current performance
   eval_results <- diagnostic_results$eval_results
   
   cat("\n1. CURRENT PERFORMANCE ASSESSMENT:\n")
   
   # High salinity performance
   if (is.na(eval_results$high_salinity_rmse)) {
      cat("⚠️  CRITICAL: No high salinity events predicted - model may be too conservative\n")
   } else if (eval_results$high_salinity_rmse > 0.2) {
      cat("⚠️  High salinity RMSE > 0.2 - still substantial prediction errors\n")
   } else {
      cat("✅ High salinity RMSE acceptable\n")
   }
   
   # Hit rate assessment
   if (is.na(eval_results$hit_rate) || eval_results$hit_rate < 0.7) {
      cat("⚠️  Hit rate < 70% - missing too many high salinity events\n")
   } else {
      cat("✅ Hit rate acceptable\n")
   }
   
   # False alarm assessment
   if (!is.na(eval_results$false_alarm_rate) && eval_results$false_alarm_rate > 0.3) {
      cat("⚠️  False alarm rate > 30% - too many false positives\n")
   } else {
      cat("✅ False alarm rate acceptable\n")
   }
   
   # 2. Recommend model improvements
   cat("\n2. RECOMMENDED MODEL IMPROVEMENTS:\n")
   
   # Check residual patterns
   if (abs(diagnostic_results$residual_stats$mean_residual) > 0.01) {
      cat("📊 Residuals show bias - consider:\n")
      cat("   - Polynomial terms for key predictors\n")
      cat("   - Different transformation of response variable\n")
   }
   
   cat("\n📈 PROGRESSION TO ADVANCED MODELS:\n")
   cat("Since linear model limitations are evident, proceed to:\n\n")
   
   cat("STEP 3A: Generalized Additive Models (GAM)\n")
   cat("- Smooth terms for discharge and inflow variables\n")
   cat("- Tensor product smooths for interactions\n")
   cat("- Better handling of non-linear relationships\n\n")
   
   cat("STEP 3B: Threshold/Regime Models\n")
   cat("- Separate models for high vs normal stress conditions\n")
   cat("- Use IsHighStress as regime indicator\n")
   cat("- Different parameter sets for each regime\n\n")
   
   cat("STEP 3C: Weighted Models\n")
   cat("- Weight observations by salinity level\n")
   cat("- Emphasize high salinity events in fitting\n")
   cat("- Consider robust regression techniques\n\n")
   
   cat("STEP 4: Hierarchical Bayesian Models\n")
   cat("- Use current model as prior information\n")
   cat("- Latent flow variable implementation\n")
   cat("- Uncertainty quantification\n")
   
   # 3. Data collection recommendations
   cat("\n3. DATA COLLECTION PRIORITIES:\n")
   
   if (eval_results$high_salinity_count < 50) {
      cat("⚠️  Limited high salinity events (n = %d)\n", eval_results$high_salinity_count)
      cat("   - Continue monitoring during low-flow periods\n")
      cat("   - Consider synthetic data generation\n")
   }
   
   cat("📡 Consider additional predictors:\n")
   cat("   - Upstream precipitation (2-7 day lags)\n")
   cat("   - Reservoir storage levels\n")
   cat("   - Atmospheric pressure (affects tidal range)\n")
   cat("   - Water temperature (affects density stratification)\n")
}

convert_to_gam <- function(linear_formula, data, threshold) {
   
   require(mgcv)
   
   cat("\n=== CONVERTING TO GAM MODEL ===\n")
   
   # Parse the linear formula to identify smooth terms
   formula_str <- as.character(linear_formula)[3]  # Get RHS of formula
   
   # Create GAM formula with smooth terms for continuous predictors
   gam_formula_str <- "Salinity ~ s(Norm_Tide, k=6) + 
                      s(Norm_RollingPowDischarge14, k=8) + 
                      s(Norm_RollingPowInflows10, k=8) + 
                      s(DayOfYear, k=12, bs='cc') +
                      IsHighStress +
                      ti(Norm_RollingPowInflows2, Norm_Tide, k=c(6,6))"
   
   # Fit GAM model
   gam_model <- gam(as.formula(gam_formula_str), 
                    data = data, 
                    weights = ifelse(data$Salinity > threshold, 3, 1),  # Weight high salinity events
                    method = "REML")
   
   # Evaluate GAM performance
   gam_results <- evaluate_model(gam_model, data, threshold, "gam")
   
   cat("GAM Model Performance:\n")
   cat(sprintf("Overall R²: %.4f\n", gam_results$overall_r2))
   cat(sprintf("High Salinity RMSE: %.4f\n", gam_results$high_salinity_rmse))
   cat(sprintf("Hit Rate: %.3f\n", gam_results$hit_rate))
   
   return(list(model = gam_model, results = gam_results))
}

# Function to create threshold model based on stress regime
create_threshold_model <- function(data, threshold) {
   
   cat("\n=== CREATING THRESHOLD MODEL ===\n")
   
   # Split data by stress regime
   normal_data <- data[!data$IsHighStress, ]
   stress_data <- data[data$IsHighStress, ]
   
   cat(sprintf("Normal regime observations: %d\n", nrow(normal_data)))
   cat(sprintf("Stress regime observations: %d\n", nrow(stress_data)))
   
   # Fit separate models for each regime
   normal_formula <- "Salinity ~ Norm_Tide + Norm_RollingPowDischarge14 + DayOfYear"
   stress_formula <- "Salinity ~ Norm_Tide + Norm_RollingPowInflows10 + Norm_RollingPowInflows2 * Norm_Tide"
   
   normal_model <- lm(as.formula(normal_formula), data = normal_data)
   stress_model <- lm(as.formula(stress_formula), data = stress_data)
   
   # Combine into threshold model object
   threshold_model <- list(
      normal_regime = normal_model,
      stress_regime = stress_model,
      type = "stress_threshold"
   )
   
   # Evaluate threshold model
   threshold_results <- evaluate_model(threshold_model, data, threshold, "threshold")
   
   cat("Threshold Model Performance:\n")
   cat(sprintf("Overall R²: %.4f\n", threshold_results$overall_r2))
   cat(sprintf("High Salinity RMSE: %.4f\n", threshold_results$high_salinity_rmse))
   cat(sprintf("Hit Rate: %.3f\n", threshold_results$hit_rate))
   
   return(list(model = threshold_model, results = threshold_results))
}

final_model <- results$final_best_model
diagnostic_results <- analyze_final_model(final_model, model_data, salinity_threshold)

gam_model <- convert_to_gam(final_model, model_data, salinity_threshold)
