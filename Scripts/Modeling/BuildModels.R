# =============================================================================
# Script Name:    BuildModels.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-11-25
# Last Updated:   2025-11-25
# Description:    Takes the output of ScreenWithRF.R and creates regularized 
#                 statistical models to further screen predictors and predict
#                 salinity exceedance or raw salinity values.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(rsample)
library(purrr)
library(glmnet) # For regularized regression
library(mgcv)   # For generalized additive models

# Source necessary functions 
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelDataScreened.qs'))
model_data <- model_data %>%
   drop_na

# Create expanding folds for cross validation and make into proper form for tidymodels
folds <- make_expanding_folds(model_data, initial_train_length = 5)

fit_model <- function(data,
                      threshold = NULL,
                      response = 'Salinity',
                      predictors = NULL,
                      model_type = 'logistic',  # 'logistic', 'linear', 'gam'
                      folds = folds,
                      
                      # Elastic net hyperparameters
                      penalty_range = c(0.001, 1),
                      mixture_range = c(0, 1),  # 0=ridge, 1=lasso, 0.5=elastic net
                      standardize = TRUE,
                      
                      # GAM hyperparameters
                      gam_k = 5,  # smoothing parameter (wiggliness)
                      gam_basis = "tp",  # "tp", "cr", "cs"
                      
                      # Tuning control
                      penalty_levels = 20,
                      mixture_levels = 5) {
   
   # Prepare data
   if (model_type %in% c("logistic", "gam_binary") & !is.null(threshold)) {
      data <- data %>%
         mutate(Response = factor(ifelse(.data[[response]] > threshold, "Violation", "Safe"),
                                  levels = c("Safe", "Violation")))
   } else {
      data <- data %>%
         mutate(Response = .data[[response]])
   }
   
   if (is.null(predictors)) {
      predictors <- setdiff(names(data)[10:ncol(data)], response)
   }
   
   data_clean <- data %>%
      select(DateTime, Response, all_of(predictors)) %>%
      drop_na()
   
   cat("Sample size:", nrow(data_clean), "\n")
   cat("Predictors:", length(predictors), "\n\n")
   
   # Handle GAM
   if (model_type %in% c("gam", "gam_binary")) {
      return(fit_gam(data_clean, predictors, model_type, folds, threshold, gam_k, gam_basis))
   }
   
   # Create folds
   cv_folds <- tibble(
      splits = map(folds, ~ make_splits(
         list(analysis = .x$train, assessment = .x$test),
         data = data_clean
      )),
      id = paste0("Fold", seq_along(folds))
   )
   class(cv_folds) <- c("manual_rset", "rset", "tbl_df", "tbl", "data.frame")
   
   # Set up model
   if (model_type == "logistic") {
      model_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
         set_engine("glmnet") %>%
         set_mode("classification")
      metrics <- metric_set(roc_auc, accuracy)
      best_metric <- "roc_auc"
   } else if (model_type == "linear") {
      model_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
         set_engine("glmnet") %>%
         set_mode("regression")
      metrics <- metric_set(rmse, rsq)
      best_metric <- "rmse"
   }
   
   # Recipe - controlled standardization
   if (standardize) {
      rec <- recipe(Response ~ ., data = data_clean %>% select(-DateTime)) %>%
         step_normalize(all_predictors())
   } else {
      rec <- recipe(Response ~ ., data = data_clean %>% select(-DateTime))
   }
   
   # Workflow
   wf <- workflow() %>%
      add_model(model_spec) %>%
      add_recipe(rec)
   
   # Tuning grid - controlled levels
   grid <- grid_regular(
      penalty(range = log10(penalty_range)),
      mixture(range = mixture_range),
      levels = c(penalty_levels, mixture_levels)
   )
   
   cat("Tuning", nrow(grid), "parameter combinations\n")
   cat("Penalty range: [", min(grid$penalty), ",", max(grid$penalty), "]\n")
   cat("Mixture values:", unique(grid$mixture), "\n")
   cat("Standardize:", standardize, "\n\n")
   
   # Tune
   tune_res <- wf %>%
      tune_grid(
         resamples = cv_folds,
         grid = grid,
         metrics = metrics,
         control = control_grid(verbose = TRUE)
      )
   
   # Best model
   best <- select_best(tune_res, metric = best_metric)
   
   cat("\n=== BEST PARAMETERS ===\n")
   cat("Penalty (λ):", best$penalty, "\n")
   cat("Mixture (α):", best$mixture, "\n")
   if (best$mixture == 0) cat("  → Pure Ridge (no variable selection)\n")
   if (best$mixture == 1) cat("  → Pure LASSO (aggressive selection)\n")
   if (best$mixture > 0 & best$mixture < 1) cat("  → Elastic Net (balanced)\n")
   
   # Finalize and fit
   final_wf <- finalize_workflow(wf, best)
   final_fit <- fit(final_wf, data = data_clean %>% select(-DateTime))
   
   # Extract coefficients - FIXED
   glmnet_fit <- extract_fit_engine(final_fit)
   coefs <- coef(glmnet_fit, s = best$penalty)
   coefs_vec <- as.vector(coefs)
   names(coefs_vec) <- rownames(coefs)
   
   coefs_vec <- coefs_vec[-1]  # remove intercept
   selected_idx <- which(coefs_vec != 0)
   selected <- names(coefs_vec)[selected_idx]
   
   cat("\n=== SELECTED VARIABLES ===\n")
   cat("Count:", length(selected), "\n")
   
   if (length(selected) > 0) {
      coef_df <- data.frame(
         Variable = selected,
         Coefficient = coefs_vec[selected_idx]
      ) %>% arrange(desc(abs(Coefficient)))
      print(coef_df)
   } else {
      cat("All coefficients shrunk to zero - try lower penalty range\n")
   }
   
   list(
      tune_results = tune_res,
      best_params = best,
      final_fit = final_fit,
      selected_vars = selected,
      coefficients = coefs_vec[selected_idx],
      model_type = model_type
   )
}

fit_gam <- function(data,
                          response = 'Salinity',
                          predictors = NULL,
                          threshold = NULL,
                          folds = NULL,
                          k_continuous = 10,
                          k_cyclical = 12,
                          do_cv = FALSE) {
   
   # Prepare data
   if (!is.null(threshold)) {
      data <- data %>%
         mutate(Response = as.numeric(.data[[response]] > threshold))
      family_type <- binomial()
      response_type <- "binary"
   } else {
      data <- data %>%
         mutate(Response = .data[[response]])
      family_type <- gaussian()
      response_type <- "continuous"
   }
   
   # Select predictors - EXCLUDE the original response variable
   if (is.null(predictors)) {
      predictors <- setdiff(names(data)[10:ncol(data)], c(response, "Response"))  # FIX: exclude both
   } else {
      predictors <- setdiff(predictors, c(response, "Response"))  # FIX: ensure Response not included
   }
   
   data_clean <- data %>%
      select(DateTime, Response, all_of(predictors)) %>%  # Only Response (the created variable)
      drop_na()
   
   cat("Sample size:", nrow(data_clean), "\n")
   cat("Predictors:", length(predictors), "\n")
   cat("Response variable: Response (derived from", response, ")\n\n")
   
   # Classify predictors by type
   cyclical_vars <- intersect(predictors, c("DayCos", "DaySin", "MonthCos", "MonthSin", "HourCos", "HourSin"))
   continuous_vars <- setdiff(predictors, cyclical_vars)
   
   cat("Continuous predictors:", length(continuous_vars), "\n")
   cat("Cyclical predictors:", length(cyclical_vars), "\n\n")
   
   # Build formula with appropriate splines
   formula_parts <- c()
   
   # Continuous variables - thin plate splines
   if (length(continuous_vars) > 0) {
      smooth_terms <- paste0("s(", continuous_vars, ", k=", k_continuous, ", bs='tp')", 
                             collapse = " + ")
      formula_parts <- c(formula_parts, smooth_terms)
   }
   
   # Cyclical variables - tensor products for Sin/Cos pairs
   if ("DayCos" %in% cyclical_vars && "DaySin" %in% cyclical_vars) {
      formula_parts <- c(formula_parts, 
                         paste0("te(DayCos, DaySin, k=", k_cyclical, ", bs='cc')"))
      cyclical_vars <- setdiff(cyclical_vars, c("DayCos", "DaySin"))
   }
   
   if ("MonthCos" %in% cyclical_vars && "MonthSin" %in% cyclical_vars) {
      formula_parts <- c(formula_parts, 
                         paste0("te(MonthCos, MonthSin, k=", k_cyclical, ", bs='cc')"))
      cyclical_vars <- setdiff(cyclical_vars, c("MonthCos", "MonthSin"))
   }
   
   if ("HourCos" %in% cyclical_vars && "HourSin" %in% cyclical_vars) {
      formula_parts <- c(formula_parts, 
                         paste0("te(HourCos, HourSin, k=", k_cyclical, ", bs='cc')"))
      cyclical_vars <- setdiff(cyclical_vars, c("HourCos", "HourSin"))
   }
   
   # Any remaining cyclical variables as linear
   if (length(cyclical_vars) > 0) {
      formula_parts <- c(formula_parts, paste(cyclical_vars, collapse = " + "))
   }
   
   formula <- as.formula(paste("Response ~", paste(formula_parts, collapse = " + ")))
   
   cat("=== FORMULA ===\n")
   print(formula)
   cat("\n")
   
   # Fit using bam
   cat("Fitting BAM (Big Additive Model)...\n")
   start_time <- Sys.time()
   
   bam_fit <- bam(
      formula,
      data = data_clean %>% select(-DateTime),
      family = family_type,
      method = "fREML",
      select = TRUE,
      discrete = TRUE,
      nthreads = 4  # Will fall back to 1 if openMP not available
   )
   
   end_time <- Sys.time()
   cat("Fitting time:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n\n")
   
   # Summary
   cat("=== MODEL SUMMARY ===\n")
   print(summary(bam_fit))
   
   # Extract significant terms
   s_table <- summary(bam_fit)$s.table
   sig_smooths <- rownames(s_table)[s_table[, "p-value"] < 0.05]
   sig_vars <- gsub("s\\((.*)\\)", "\\1", sig_smooths)
   sig_vars <- gsub("te\\((.*),(.*)\\)", "\\1,\\2", sig_vars)
   
   # Add significant parametric terms
   if (!is.null(summary(bam_fit)$p.table) && nrow(summary(bam_fit)$p.table) > 1) {
      p_table <- summary(bam_fit)$p.table
      sig_parametric <- rownames(p_table)[p_table[, "Pr(>|t|)"] < 0.05]
      sig_parametric <- sig_parametric[sig_parametric != "(Intercept)"]
      if (length(sig_parametric) > 0) {
         sig_vars <- c(sig_vars, sig_parametric)
      }
   }
   
   cat("\n=== SIGNIFICANT PREDICTORS (p < 0.05) ===\n")
   cat("Count:", length(sig_vars), "\n")
   print(sig_vars)
   
   # Optional CV
   cv_perf <- NULL
   if (do_cv && !is.null(folds)) {
      cat("\n=== RUNNING CROSS-VALIDATION ===\n")
      
      cv_perf <- map_dfr(seq_along(folds), function(i) {
         cat("Fold", i, "/", length(folds), "\n")
         
         train_data <- data_clean[folds[[i]]$train, ] %>% select(-DateTime)
         test_data <- data_clean[folds[[i]]$test, ] %>% select(-DateTime)
         
         fold_fit <- bam(formula, data = train_data, family = family_type, 
                         method = "fREML", discrete = TRUE)
         
         if (response_type == "binary") {
            preds <- predict(fold_fit, test_data, type = "response")
            actual_factor <- factor(test_data$Response, levels = c("0", "1"))
            auc <- roc_auc_vec(actual_factor, preds)
            tibble(fold = i, metric = "auc", value = auc)
         } else {
            preds <- predict(fold_fit, test_data)
            rmse_val <- rmse_vec(test_data$Response, preds)
            rsq_val <- rsq_vec(test_data$Response, preds)
            bind_rows(
               tibble(fold = i, metric = "rmse", value = rmse_val),
               tibble(fold = i, metric = "rsq", value = rsq_val)
            )
         }
      })
      
      cat("\n=== CV PERFORMANCE ===\n")
      cv_summary <- cv_perf %>%
         group_by(metric) %>%
         summarise(mean = mean(value), sd = sd(value))
      print(cv_summary)
   }
   
   # In-sample performance
   fitted_vals <- fitted(bam_fit)
   actual_vals <- data_clean$Response
   
   if (response_type == "binary") {
      in_sample_auc <- roc_auc_vec(factor(actual_vals, levels = c("0", "1")), fitted_vals)
      cat("\n=== IN-SAMPLE PERFORMANCE ===\n")
      cat("AUC:", round(in_sample_auc, 4), "\n")
   } else {
      in_sample_rmse <- rmse_vec(actual_vals, fitted_vals)
      in_sample_rsq <- rsq_vec(actual_vals, fitted_vals)
      cat("\n=== IN-SAMPLE PERFORMANCE ===\n")
      cat("RMSE:", round(in_sample_rmse, 4), "\n")
      cat("R-squared:", round(in_sample_rsq, 4), "\n")
   }
   
   # Model diagnostics
   cat("\n=== MODEL DIAGNOSTICS ===\n")
   cat("Running gam.check()...\n")
   gam.check(bam_fit)
   
   list(
      bam_fit = bam_fit,
      selected_vars = sig_vars,
      cv_performance = cv_perf,
      formula = formula,
      response_type = response_type
   )
}



elastic_linear <- fit_model(
   data = model_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10), 
   mixture_range = c(0, 1),
   folds = folds
)

lasso_linear <- fit_model(
   data = model_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(1, 1),  # Pure LASSO
   folds = folds
)

ridge_linear <- fit_model(
   data = model_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(0, 0),  # Pure Ridge
   folds = folds
)

gam <- fit_gam(
   data = model_data,
   response = 'Salinity',
   threshold = NULL,
   predictors = NULL,  
   k_continuous = 25,
   k_cyclical = 15,
   folds = folds,
   do_cv = TRUE  
)


# Write output files
objects <- list(elastic_linear, lasso_linear, ridge_linear, gam)
file_names <- list('ElasticLinearModel', 'LassoLinearModel', 'RidgeLinearModel', 'GamModel')
write_qs_files(objects, 'Outputs/Experiments/Models', file_names)

# Clear global environment
rm(list = ls())


# # 1. View CV results
# collect_metrics(test$tune_results)
# 
# # 2. See best performing hyperparameter combinations
# show_best(test$tune_results, n = 10, metric = "rmse")  # or "roc_auc"
# 
# # 3. Plot CV performance
# autoplot(test$tune_results)
# 
# # 4. See the regularization path (how coefficients changed with penalty)
# autoplot(test$tune_results, metric = "rmse") +
#    labs(title = "Cross-Validation Performance")
# 
# # 5. Get predictions from final model
# predictions <- predict(test$final_fit, new_data = model_data)
# 
# # 6. Extract the underlying glmnet object (for more detailed inspection)
# glmnet_obj <- extract_fit_engine(test$final_fit)
# 
# # Plot the coefficient path from glmnet
# plot(glmnet_obj, xvar = "lambda", label = TRUE)
# abline(v = log(test$best_params$penalty), lty = 2, col = "red")
# 
# # 7. See how many variables at different penalty levels
# plot(glmnet_obj$lambda, glmnet_obj$df, type = "l",
#      xlab = "Lambda", ylab = "Number of Non-zero Coefficients")
# abline(v = test$best_params$penalty, lty = 2, col = "red")


