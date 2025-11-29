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

set.seed(123)

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
                      model_type = 'logistic',  # 'logistic', 'linear', 'rf
                      folds = folds,
                      
                      # Elastic net hyperparameters
                      penalty_range = c(0.001, 1),
                      mixture_range = c(0, 1),  # 0=ridge, 1=lasso, 0.5=elastic net
                      standardize = TRUE,
                      
                      # Random forest hyperparameters
                      mtry_range = NULL, 
                      trees = 500,
                      min_n_range = c(10, 20),
                      
                      
                      # Tuning control
                      penalty_levels = 20,
                      mixture_levels = 5,
                      rf_levels = 10) {
   
   # Prepare data
   if (model_type %in% c("logistic") & !is.null(threshold)) {
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
      
      rec <- if (standardize) {
         recipe(Response ~ ., data = data_clean %>% select(-DateTime)) %>%
            step_normalize(all_predictors())
      } else {
         recipe(Response ~ ., data = data_clean %>% select(-DateTime))
      }
      
      grid <- grid_regular(
         penalty(range = log10(penalty_range)),
         mixture(range = mixture_range),
         levels = c(penalty_levels, mixture_levels)
      )
      
   } else if (model_type == "linear") {
      model_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
         set_engine("glmnet") %>%
         set_mode("regression")
      metrics <- metric_set(rmse, rsq)
      best_metric <- "rmse"
      
      rec <- if (standardize) {
         recipe(Response ~ ., data = data_clean %>% select(-DateTime)) %>%
            step_normalize(all_predictors())
      } else {
         recipe(Response ~ ., data = data_clean %>% select(-DateTime))
      }
      
      grid <- grid_regular(
         penalty(range = log10(penalty_range)),
         mixture(range = mixture_range),
         levels = c(penalty_levels, mixture_levels)
      )
      
   } else if (model_type == "rf") {
      # Set mtry range based on number of predictors
      if (is.null(mtry_range)) {
         mtry_range <- c(floor(sqrt(length(predictors))), length(predictors))
      }
      
      model_spec <- rand_forest(
         mtry = tune(),
         trees = trees,
         min_n = tune()
      ) %>%
         set_engine("ranger", importance = "permutation") %>%
         set_mode("regression")
      
      metrics <- metric_set(rmse, rsq)
      best_metric <- "rmse"
      
      # Random forest doesn't require normalization
      rec <- recipe(Response ~ ., data = data_clean %>% select(-DateTime))
      
      grid <- grid_regular(
         mtry(range = mtry_range),
         min_n(range = min_n_range),
         levels = rf_levels
      )
   }
   
   # Workflow
   wf <- workflow() %>%
      add_model(model_spec) %>%
      add_recipe(rec)
   
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
   if (model_type %in% c("logistic", "linear")) {
      cat("Penalty (λ):", best$penalty, "\n")
      cat("Mixture (α):", best$mixture, "\n")
      if (best$mixture == 0) cat("  → Pure Ridge (no variable selection)\n")
      if (best$mixture == 1) cat("  → Pure LASSO (aggressive selection)\n")
      if (best$mixture > 0 & best$mixture < 1) cat("  → Elastic Net (balanced)\n")
   } else if (model_type == "rf") {
      cat("mtry:", best$mtry, "\n")
      cat("min_n:", best$min_n, "\n")
      cat("trees:", trees, "\n")
   }
   
   # Finalize and fit
   final_wf <- finalize_workflow(wf, best)
   final_fit <- fit(final_wf, data = data_clean %>% select(-DateTime))
   
   # Extract variable importance or coefficients
   if (model_type %in% c("logistic", "linear")) {
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
      
      return(list(
         tune_results = tune_res,
         best_params = best,
         final_fit = final_fit,
         selected_vars = selected,
         coefficients = coefs_vec[selected_idx],
         model_type = model_type
      ))
      
   } else if (model_type == "rf") {
      rf_fit <- extract_fit_engine(final_fit)
      
      # ranger stores importance in the fit object directly
      var_imp <- rf_fit$variable.importance
      
      imp_df <- data.frame(
         Variable = names(var_imp),
         Importance = as.vector(var_imp)
      ) %>% arrange(desc(Importance))
      
      cat("\n=== VARIABLE IMPORTANCE (Top 10) ===\n")
      print(head(imp_df, 10))
      
      return(list(
         tune_results = tune_res,
         best_params = best,
         final_fit = final_fit,
         var_importance = imp_df,
         model_type = model_type
      ))
   }
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

rf <- fit_model(
   data = model_data,
   model_type = 'rf',
   trees = 300,
   rf_levels = 3,
   folds = folds
)



# Write output files
objects <- list(elastic_linear, lasso_linear, ridge_linear, rf)
file_names <- list('ElasticLinearModel', 'LassoLinearModel', 'RidgeLinearModel', 'RFModel')
write_qs_files(objects, 'Outputs/Experiments/Models', file_names)

# Clear global environment
rm(list = ls())
