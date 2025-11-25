calculate_test_importance <- function(rf_model, test_data, response_col, predictor_cols) {
   
   # Get baseline predictions and MSE
   baseline_preds <- predict(rf_model, data = test_data)$predictions
   baseline_mse <- mean((test_data[[response_col]] - baseline_preds)^2)
   
   # Calculate importance for each variable
   importance_values <- numeric(length(predictor_cols))
   names(importance_values) <- predictor_cols
   
   for (var in predictor_cols) {
      # Permute this variable
      test_permuted <- test_data
      test_permuted[[var]] <- sample(test_permuted[[var]])
      
      # Get predictions with permuted variable
      permuted_preds <- predict(rf_model, data = test_permuted)$predictions
      permuted_mse <- mean((test_data[[response_col]] - permuted_preds)^2)
      
      # Importance = increase in MSE
      importance_values[var] <- permuted_mse - baseline_mse
   }
   
   return(importance_values)
}

run_rf_cv <- function(data, folds, response_col, predictor_cols, 
                      ntree = 500, mtry = NULL) {
   
   # Allow column indices
   if (is.numeric(response_col)) response_col <- names(data)[response_col]
   if (is.numeric(predictor_cols)) predictor_cols <- names(data)[predictor_cols]
   
   results <- list()
   
   for (i in seq_along(folds)) {
      cat("Running fold", i, "of", length(folds), "...\n")
      
      train_idx <- folds[[i]]$train
      test_idx  <- folds[[i]]$test
      
      train_data <- data[train_idx, ]
      test_data  <- data[test_idx, ]
      
      # Remove rows with NA in response
      train_data <- train_data[!is.na(train_data[[response_col]]), ]
      test_data  <- test_data[!is.na(test_data[[response_col]]), ]
      
      # Skip if no valid data
      if (nrow(test_data) == 0 | nrow(train_data) == 0) {
         warning(paste("Fold", i, "has no valid train/test data — skipping"))
         next
      }
      
      # Monthly median imputation for predictors
      for (col in predictor_cols) {
         train_data[[col]] <- ifelse(
            is.na(train_data[[col]]),
            ave(train_data[[col]], train_data$Month, FUN = function(x) median(x, na.rm = TRUE)),
            train_data[[col]]
         )
         test_data[[col]] <- ifelse(
            is.na(test_data[[col]]),
            ave(train_data[[col]], train_data$Month, FUN = function(x) median(x, na.rm = TRUE))[match(test_data$Month, train_data$Month)],
            test_data[[col]]
         )
      }
      
      # Formula
      rf_formula <- as.formula(
         paste(response_col, "~", paste(predictor_cols, collapse = " + "))
      )
      
      # Train ranger with OOB importance
      cat("  Training model...\n")
      rf_model <- ranger(
         formula = rf_formula,
         data = train_data,
         num.trees = ntree,
         mtry = mtry,
         importance = "permutation",  # OOB importance
         num.threads = 6,
         write.forest = TRUE
      )
      
      # Predict on test set
      preds <- predict(rf_model, data = test_data)$predictions
      obs <- test_data[[response_col]]
      
      # Metrics
      rmse_val <- rmse(obs, preds)
      mae_val  <- mae(obs, preds)
      
      # OOB Variable importance (from training)
      oob_imp <- data.frame(
         Variable = names(rf_model$variable.importance),
         IncMSE_OOB = rf_model$variable.importance,
         Fold = i,
         row.names = NULL
      )
      
      # Test set importance
      cat("  Calculating test set importance...\n")
      test_imp_values <- calculate_test_importance(rf_model, test_data, 
                                                   response_col, predictor_cols)
      
      test_imp <- data.frame(
         Variable = names(test_imp_values),
         IncMSE_Test = test_imp_values,
         Fold = i,
         row.names = NULL
      )
      
      # Combine both importance measures
      combined_imp <- merge(oob_imp, test_imp, by = c("Variable", "Fold"))
      
      # Store results
      results[[i]] <- list(
         fold = i,
         train_years = folds[[i]]$train_years,
         test_years = folds[[i]]$test_years,
         metrics = data.frame(
            Fold = i,
            Train_Years = paste(folds[[i]]$train_years, collapse = "-"),
            Test_Years = paste(folds[[i]]$test_years, collapse = "-"),
            RMSE = rmse_val,
            MAE = mae_val
         ),
         importance = combined_imp,
         importance_oob = oob_imp,
         importance_test = test_imp
      )
   }
   
   # Combine results
   metrics_all <- do.call(rbind, lapply(results, `[[`, "metrics"))
   importance_all <- do.call(rbind, lapply(results, `[[`, "importance"))
   importance_oob_all <- do.call(rbind, lapply(results, `[[`, "importance_oob"))
   importance_test_all <- do.call(rbind, lapply(results, `[[`, "importance_test"))
   
   return(list(
      folds = results,
      metrics = metrics_all,
      importance = importance_all,  # Combined (both OOB and Test)
      importance_oob = importance_oob_all,  # Just OOB
      importance_test = importance_test_all  # Just Test
   ))
}

# Convenience function to compare OOB vs Test importance
compare_importance_types <- function(importance_df, fold_num = NULL) {
   
   if (!is.null(fold_num)) {
      importance_df <- importance_df %>% filter(Fold == fold_num)
   }
   
   # Calculate correlations by fold
   cor_by_fold <- importance_df %>%
      group_by(Fold) %>%
      summarise(
         Correlation = cor(IncMSE_OOB, IncMSE_Test, use = "complete.obs"),
         Spearman = cor(IncMSE_OOB, IncMSE_Test, method = "spearman", use = "complete.obs"),
         .groups = "drop"
      )
   
   cat("\n=== OOB vs Test Importance Correlation ===\n")
   print(cor_by_fold)
   
   # Overall statistics
   cat("\nOverall Mean Correlation:", mean(cor_by_fold$Correlation, na.rm = TRUE), "\n")
   cat("Overall Mean Spearman:", mean(cor_by_fold$Spearman, na.rm = TRUE), "\n")
   
   return(cor_by_fold)
}
