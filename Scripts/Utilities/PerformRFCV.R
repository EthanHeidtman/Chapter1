
build_group_map <- function(predictor_cols) {
   list(
      LagSalinity        = grep("LagSalinity",                    predictor_cols, value = TRUE),
      Tide               = grep("TideRange|TideMean",             predictor_cols, value = TRUE),
      Wind               = grep("RollingWind",                    predictor_cols, value = TRUE),
      SustainedDischarge = grep("RollingDischarge|RollingAnomaly",predictor_cols, value = TRUE),
      FlushingDischarge  = grep("MaxDischarge|ExceedFlux",        predictor_cols, value = TRUE)
   )
}

assign_group <- function(var, group_map) {
   for (grp in names(group_map)) {
      if (var %in% group_map[[grp]]) return(grp)
   }
   if (var == 'h') return('Horizon')
   return('other')
}

screen_predictors_per_group <- function(rf_result, predictor_cols, group_map, n_screen = 10) {
   
   pooled_test_imp <- rf_result$importance_test %>%
      group_by(Variable) %>%
      summarise(TestMag = mean(IncMSE_Test, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Group = sapply(Variable, assign_group, group_map = group_map))
   
   survivors <- pooled_test_imp %>%
      group_by(Group) %>%
      slice_max(TestMag, n = n_screen, with_ties = FALSE) %>%
      ungroup()
   
   singleton_groups <- pooled_test_imp %>%
      group_by(Group) %>%
      filter(n() == 1) %>%
      ungroup()
   
   union(survivors$Variable, singleton_groups$Variable)
}

get_oob_predictions <- function(rf_model, data, oob_mask, num_threads) {
   pred_all <- predict(rf_model, data = data, predict.all = TRUE,
                       num.threads = num_threads)$predictions
   masked <- pred_all
   masked[!oob_mask] <- NA
   list(pred = rowMeans(masked, na.rm = TRUE), n_oob_trees = rowSums(oob_mask))
}

compute_h_importance <- function(rf_result, predictor_cols, group_map,
                                 n_repeats = 1, num_threads = 1, vars_per_chunk = 2) {
   
   h_importance_list <- list()
   
   for (i in seq_along(rf_result$folds)) {
      
      fold_result <- rf_result$folds[[i]]
      if (is.null(fold_result)) next
      
      rf_model   <- fold_result$model
      train_data <- fold_result$train_data
      if (is.null(rf_model) || is.null(train_data)) next
      
      inbag_mat <- do.call(cbind, rf_model$inbag.counts)
      oob_mask  <- inbag_mat == 0
      n_all     <- nrow(train_data)
      
      base_result   <- get_oob_predictions(rf_model, train_data, oob_mask, num_threads)
      valid         <- base_result$n_oob_trees >= 5
      obs_all       <- train_data[['Salinity_h']]
      h_all         <- train_data[['h']]
      
      base_rmse_by_h <- tapply(seq_len(n_all)[valid], h_all[valid], function(rows) {
         sqrt(mean((obs_all[rows] - base_result$pred[rows])^2))
      })
      
      rm(base_result); gc(FALSE)
      
      var_chunks    <- split(predictor_cols, ceiling(seq_along(predictor_cols) / vars_per_chunk))
      fold_imp_list <- vector("list", length(var_chunks))
      
      for (ch in seq_along(var_chunks)) {
         
         chunk_vars <- var_chunks[[ch]]
         n_blocks   <- length(chunk_vars) * n_repeats
         block_list <- vector("list", n_blocks)
         block_meta <- vector("list", n_blocks)
         idx <- 1
         
         for (var in chunk_vars) {
            for (r in seq_len(n_repeats)) {
               perm              <- train_data
               perm[[var]]       <- sample(perm[[var]])
               block_list[[idx]] <- perm
               block_meta[[idx]] <- data.frame(Variable = var, Repeat = r, BlockID = idx)
               idx <- idx + 1
            }
         }
         
         stacked_perm <- data.table::rbindlist(block_list) %>% as.data.frame()
         meta         <- data.table::rbindlist(block_meta) %>% as.data.frame()
         rm(block_list, block_meta)
         
         oob_mask_stacked  <- do.call(rbind, replicate(n_blocks, oob_mask, simplify = FALSE))
         perm_result       <- get_oob_predictions(rf_model, stacked_perm, oob_mask_stacked, num_threads)
         
         rm(stacked_perm, oob_mask_stacked); gc(FALSE)
         
         obs_rep      <- rep(obs_all, times = n_blocks)
         h_rep        <- rep(h_all,   times = n_blocks)
         valid_rep    <- rep(valid,   times = n_blocks)
         block_id_rep <- rep(meta$BlockID, each = n_all)
         
         fold_imp_list[[ch]] <- data.frame(
            BlockID = block_id_rep, h = h_rep, obs = obs_rep,
            pred = perm_result$pred, valid = valid_rep
         ) %>%
            filter(valid) %>%
            group_by(BlockID, h) %>%
            summarise(PermRMSE = sqrt(mean((obs - pred)^2)), .groups = 'drop') %>%
            left_join(meta, by = 'BlockID') %>%
            mutate(BaseRMSE = base_rmse_by_h[as.character(h)],
                   IncRMSE  = PermRMSE - BaseRMSE)
      }
      
      h_importance_list[[i]] <- do.call(rbind, fold_imp_list) %>%
         group_by(Variable, h) %>%
         summarise(Importance = mean(IncRMSE, na.rm = TRUE), .groups = 'drop') %>%
         mutate(Fold = i)
   }
   
   if (length(h_importance_list) == 0) return(NULL)
   
   do.call(rbind, h_importance_list) %>%
      group_by(Variable, h) %>%
      summarise(
         MeanImportance = mean(Importance, na.rm = TRUE),
         SDImportance   = sd(Importance,   na.rm = TRUE),
         .groups = 'drop'
      ) %>%
      mutate(Group = sapply(Variable, assign_group, group_map = group_map)) %>%
      arrange(Group, Variable, h)
}


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
         write.forest = TRUE,
         keep.inbag = TRUE
      )
      
      # Predict on test set
      preds <- predict(rf_model, data = test_data)$predictions
      obs <- test_data[[response_col]]
      
      # Metrics
      rmse_val <- Metrics::rmse(obs, preds)
      mae_val  <- Metrics::mae(obs, preds)
      
      # OOB Variable importance (from training)
      oob_imp <- data.frame(
         Variable    = names(rf_model$variable.importance),
         IncMSE_OOB  = rf_model$variable.importance,
         BaselineMSE = rf_model$prediction.error,   # OOB MSE of the full fitted model, needed to convert IncMSE -> RMSE increase downstream
         Fold        = i,
         row.names   = NULL
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
         model = rf_model,
         fold = i,
         train_years = folds[[i]]$train_years,
         test_years = folds[[i]]$test_years,
         train_data = train_data, 
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
