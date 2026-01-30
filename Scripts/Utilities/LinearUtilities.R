fit_model <- function(data,
                      threshold = NULL,
                      response = 'Salinity',
                      predictors = NULL,
                      model_type = 'logistic',
                      folds = folds,
                      
                      # Threshold evaluation (for regression models)
                      eval_threshold = NULL,  # NEW: threshold for subset evaluation
                      
                      # Elastic net hyperparameters
                      penalty_range = c(0.001, 1),
                      mixture_range = c(0, 1),
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
      predictors <- names(data)[(which(names(data) == 'Salinity') + 1) : (ncol(data) - 1)]
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
      metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)
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
      metrics <- metric_set(rmse, rsq, mae)
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
      
      metrics <- metric_set(rmse, rsq, mae)
      best_metric <- "rmse"
      
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
         control = control_grid(verbose = FALSE)
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
   
   # Finalize workflow
   final_wf <- finalize_workflow(wf, best)
   
   # ===== Generate fold-level predictions and metrics =====
   fold_predictions <- map_dfr(seq_along(cv_folds$splits), function(i) {
      split <- cv_folds$splits[[i]]
      fold_id <- cv_folds$id[i]
      
      # Fit on training fold
      fold_fit <- fit(final_wf, data = analysis(split) %>% select(-DateTime))
      
      # Predict on test fold
      test_data <- assessment(split)
      if (model_type == "logistic") {
         preds <- predict(fold_fit, new_data = test_data, type = "prob") %>%
            bind_cols(predict(fold_fit, new_data = test_data)) %>%
            bind_cols(test_data %>% select(DateTime, Response))
      } else {
         preds <- predict(fold_fit, new_data = test_data) %>%
            bind_cols(test_data %>% select(DateTime, Response))
      }
      
      preds %>% mutate(fold = fold_id)
   })
   
   # Calculate overall metrics
   if (model_type == "logistic") {
      overall_metrics <- fold_predictions %>%
         metrics(truth = Response, .pred_Violation, estimate = .pred_class)
   } else {
      overall_metrics <- fold_predictions %>%
         metrics(truth = Response, estimate = .pred)
   }
   
   # Calculate fold-level metrics for plotting
   if (model_type == "logistic") {
      fold_metrics <- fold_predictions %>%
         group_by(fold) %>%
         metrics(truth = Response, .pred_Violation, estimate = .pred_class) %>%
         ungroup()
   } else {
      fold_metrics <- fold_predictions %>%
         group_by(fold) %>%
         metrics(truth = Response, estimate = .pred) %>%
         ungroup()
   }
   
   # ===== Threshold-based evaluation for regression =====
   threshold_metrics_overall <- NULL
   threshold_metrics_folds <- NULL
   
   if (model_type %in% c("linear", "rf") && !is.null(eval_threshold)) {
      
      # Filter to observations above threshold
      above_threshold <- fold_predictions %>%
         filter(Response > eval_threshold)
      
      if (nrow(above_threshold) > 0) {
         # Overall metrics for high values
         threshold_metrics_overall <- above_threshold %>%
            metrics(truth = Response, estimate = .pred) %>%
            mutate(subset = paste0("above_", eval_threshold))
         
         # Fold-level metrics for high values
         threshold_metrics_folds <- above_threshold %>%
            group_by(fold) %>%
            metrics(truth = Response, estimate = .pred) %>%
            ungroup() %>%
            mutate(subset = paste0("above_", eval_threshold))
         
         cat("\n=== METRICS FOR VALUES >", eval_threshold, "===\n")
         cat("N observations:", nrow(above_threshold), "\n")
         print(threshold_metrics_overall)
      } else {
         warning("No observations above threshold ", eval_threshold)
      }
   }
   
   # Fit final model on all data
   final_fit <- fit(final_wf, data = data_clean %>% select(-DateTime))
   
   # Extract prepped recipe from fitted workflow
   prepped_rec <- extract_recipe(final_fit, estimated = TRUE)
   
   # Get formula - now that recipe is prepped
   model_formula <- formula(prepped_rec)
   
   # Extract final model details
   model_details <- list(
      model = final_fit,
      formula = model_formula,
      response_var = response,
      n_predictors = length(predictors),
      predictor_names = predictors,
      sample_size = nrow(data_clean),
      model_type = model_type,
      standardized = standardize
   )
   
   # Add model-specific details
   if (model_type %in% c("logistic", "linear")) {
      glmnet_fit <- extract_fit_engine(final_fit)
      
      # Get lambda sequence and degrees of freedom
      lambda_seq <- glmnet_fit$lambda
      df_seq <- glmnet_fit$df
      
      model_details$penalty_lambda <- best$penalty
      model_details$mixture_alpha <- best$mixture
      model_details$n_lambda_tried <- length(lambda_seq)
      model_details$lambda_range <- range(lambda_seq)
      model_details$df_at_best <- df_seq[which.min(abs(lambda_seq - best$penalty))]
      
      # Classification-specific
      if (model_type == "logistic") {
         model_details$classification_threshold <- threshold
         model_details$response_levels <- levels(data_clean$Response)
      }
      
   } else if (model_type == "rf") {
      rf_fit <- extract_fit_engine(final_fit)
      
      model_details$mtry <- best$mtry
      model_details$min_n <- best$min_n
      model_details$num_trees <- trees
      model_details$oob_error <- if (!is.null(rf_fit$prediction.error)) {
         rf_fit$prediction.error
      } else {
         NA
      }
      model_details$oob_rsq <- if (!is.null(rf_fit$r.squared)) {
         rf_fit$r.squared
      } else {
         NA
      }
   }
   
   # Add evaluation threshold if used
   if (!is.null(eval_threshold)) {
      model_details$eval_threshold <- eval_threshold
   }
   
   # Print model summary
   cat("\n=== FINAL MODEL DETAILS ===\n")
   cat("Formula:", deparse(model_formula), "\n")
   cat("Response:", response, "\n")
   cat("N predictors:", length(predictors), "\n")
   cat("Sample size:", nrow(data_clean), "\n")
   cat("Model type:", model_type, "\n")
   if (model_type %in% c("logistic", "linear")) {
      cat("Penalty (λ):", best$penalty, "\n")
      cat("Mixture (α):", best$mixture, "\n")
      cat("Active variables:", model_details$df_at_best, "\n")
   } else if (model_type == "rf") {
      cat("mtry:", best$mtry, "\n")
      cat("min_n:", best$min_n, "\n")
      cat("trees:", trees, "\n")
      if (!is.na(model_details$oob_rsq)) {
         cat("OOB R²:", round(model_details$oob_rsq, 4), "\n")
      }
   }
   
   # Print overall metrics
   cat("\n=== OVERALL METRICS (ALL FOLDS) ===\n")
   print(overall_metrics)
   
   # Extract variable importance or coefficients
   if (model_type %in% c("logistic", "linear")) {
      glmnet_fit <- extract_fit_engine(final_fit)
      coefs <- coef(glmnet_fit, s = best$penalty)
      coefs_vec <- as.vector(coefs)
      names(coefs_vec) <- rownames(coefs)
      
      coefs_vec <- coefs_vec[-1]
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
         model_type = model_type,
         
         # Model details
         model_details = model_details,
         
         # NEW: Metrics for plotting
         fold_predictions = fold_predictions,
         overall_metrics = overall_metrics,
         fold_metrics = fold_metrics,
         threshold_metrics_overall = threshold_metrics_overall,
         threshold_metrics_folds = threshold_metrics_folds
      ))
      
   } else if (model_type == "rf") {
      rf_fit <- extract_fit_engine(final_fit)
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
         model_type = model_type,
         
         # Model details
         model_details = model_details,
         
         # NEW: Metrics for plotting
         fold_predictions = fold_predictions,
         overall_metrics = overall_metrics,
         fold_metrics = fold_metrics,
         threshold_metrics_overall = threshold_metrics_overall,
         threshold_metrics_folds = threshold_metrics_folds
      ))
   }
}
