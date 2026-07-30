build_group_map <- function(predictor_cols) {
   list(
      LagSalinity        = grep("LagSalinity",                     predictor_cols, value = TRUE),
      Tide               = grep("TideRange|TideMean",              predictor_cols, value = TRUE),
      Wind               = grep("RollingWind",                     predictor_cols, value = TRUE),
      SustainedDischarge = grep("RollingDischarge|RollingAnomaly", predictor_cols, value = TRUE),
      FlushingDischarge  = grep("MaxDischarge|ExceedFlux",         predictor_cols, value = TRUE)
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

# Vectorized, NA-free OOB prediction calculation
get_oob_predictions <- function(rf_model, data, oob_mask, num_threads = 1) {
   pred_all <- predict(rf_model, data = data, predict.all = TRUE, num.threads = num_threads)$predictions
   n_oob <- rowSums(oob_mask)
   pred_sum <- rowSums(pred_all * oob_mask)
   list(pred = pred_sum / n_oob, n_oob_trees = n_oob)
}

compute_h_importance <- function(rf_result, predictor_cols, group_map,
                                 n_repeats = 1, num_threads = 1, vars_per_chunk = 2) {
   
   h_importance_list <- vector("list", length(rf_result$folds))
   
   for (i in seq_along(rf_result$folds)) {
      fold_result <- rf_result$folds[[i]]
      if (is.null(fold_result)) next
      
      rf_model   <- fold_result$model
      train_data <- fold_result$train_data
      if (is.null(rf_model) || is.null(train_data)) next
      
      inbag_mat <- do.call(cbind, rf_model$inbag.counts)
      oob_mask  <- (inbag_mat == 0)
      n_all     <- nrow(train_data)
      
      base_result <- get_oob_predictions(rf_model, train_data, oob_mask, num_threads)
      valid       <- base_result$n_oob_trees >= 5
      valid_idx   <- which(valid)
      
      obs_all     <- train_data[['Salinity_h']]
      h_all       <- train_data[['h']]
      
      obs_v       <- obs_all[valid_idx]
      h_v         <- h_all[valid_idx]
      
      base_rmse_by_h <- tapply((obs_v - base_result$pred[valid_idx])^2, h_v, function(x) sqrt(mean(x)))
      rm(base_result)
      
      var_chunks    <- split(predictor_cols, ceiling(seq_along(predictor_cols) / vars_per_chunk))
      fold_imp_list <- vector("list", length(var_chunks))
      
      for (ch in seq_along(var_chunks)) {
         chunk_vars <- var_chunks[[ch]]
         n_blocks   <- length(chunk_vars) * n_repeats
         block_list <- vector("list", n_blocks)
         
         meta_df <- data.frame(
            BlockID  = seq_len(n_blocks),
            Variable = rep(chunk_vars, each = n_repeats),
            Repeat   = rep(seq_len(n_repeats), times = length(chunk_vars)),
            stringsAsFactors = FALSE
         )
         
         idx <- 1
         for (var in chunk_vars) {
            for (r in seq_len(n_repeats)) {
               perm        <- train_data
               perm[[var]] <- sample(perm[[var]])
               block_list[[idx]] <- perm
               idx <- idx + 1
            }
         }
         
         stacked_perm <- data.table::rbindlist(block_list)
         rm(block_list)
         
         perm_pred_all <- predict(rf_model, data = stacked_perm, predict.all = TRUE, num.threads = num_threads)$predictions
         rm(stacked_perm)
         
         block_results <- vector("list", n_blocks)
         
         for (b in seq_len(n_blocks)) {
            row_start <- (b - 1) * n_all
            pred_mat_b <- perm_pred_all[(row_start + 1):(row_start + n_all), , drop = FALSE]
            
            pred_b_oob <- rowSums(pred_mat_b * oob_mask) / rowSums(oob_mask)
            pred_b_v   <- pred_b_oob[valid_idx]
            
            sq_err <- (obs_v - pred_b_v)^2
            perm_rmse <- sqrt(tapply(sq_err, h_v, mean))
            
            inc_rmse <- perm_rmse - base_rmse_by_h[names(perm_rmse)]
            
            block_results[[b]] <- data.frame(
               Variable = meta_df$Variable[b],
               h        = as.numeric(names(inc_rmse)),
               IncRMSE  = as.vector(inc_rmse),
               stringsAsFactors = FALSE
            )
         }
         
         fold_imp_list[[ch]] <- do.call(rbind, block_results)
      }
      
      h_importance_list[[i]] <- do.call(rbind, fold_imp_list) %>%
         group_by(Variable, h) %>%
         summarise(Importance = mean(IncRMSE, na.rm = TRUE), .groups = 'drop') %>%
         mutate(Fold = i)
   }
   
   valid_h_imp <- compact(h_importance_list)
   if (length(valid_h_imp) == 0) return(NULL)
   
   do.call(rbind, valid_h_imp) %>%
      group_by(Variable, h) %>%
      summarise(
         MeanImportance = mean(Importance, na.rm = TRUE),
         SDImportance   = sd(Importance,   na.rm = TRUE),
         .groups = 'drop'
      ) %>%
      mutate(Group = sapply(Variable, assign_group, group_map = group_map)) %>%
      arrange(Group, Variable, h)
}

calculate_test_importance <- function(rf_model, test_data, response_col, predictor_cols, num_threads = 1) {
   baseline_preds <- predict(rf_model, data = test_data, num.threads = num_threads)$predictions
   baseline_mse   <- mean((test_data[[response_col]] - baseline_preds)^2)
   
   importance_values <- numeric(length(predictor_cols))
   names(importance_values) <- predictor_cols
   
   for (var in predictor_cols) {
      test_permuted <- test_data
      test_permuted[[var]] <- sample(test_permuted[[var]])
      
      permuted_preds <- predict(rf_model, data = test_permuted, num.threads = num_threads)$predictions
      permuted_mse   <- mean((test_data[[response_col]] - permuted_preds)^2)
      
      importance_values[var] <- permuted_mse - baseline_mse
   }
   
   return(importance_values)
}

run_rf_cv <- function(data, folds, response_col, predictor_cols, 
                      ntree = 300, mtry = NULL, num_threads = 1, calc_test_imp = TRUE) {
   
   if (is.numeric(response_col)) response_col <- names(data)[response_col]
   if (is.numeric(predictor_cols)) predictor_cols <- names(data)[predictor_cols]
   
   results <- list()
   
   for (i in seq_along(folds)) {
      train_idx <- folds[[i]]$train
      test_idx  <- folds[[i]]$test
      
      train_data <- data[train_idx, ]
      test_data  <- data[test_idx, ]
      
      train_data <- train_data[!is.na(train_data[[response_col]]), ]
      test_data  <- test_data[!is.na(test_data[[response_col]]), ]
      
      if (nrow(test_data) == 0 || nrow(train_data) == 0) {
         warning(paste("Fold", i, "has no valid train/test data — skipping"))
         next
      }
      
      # Fast vectorized monthly median imputation
      train_medians <- train_data %>%
         group_by(Month) %>%
         summarise(across(all_of(predictor_cols), \(x) median(x, na.rm = TRUE)), .groups = "drop")
      
      for (col in predictor_cols) {
         na_train <- which(is.na(train_data[[col]]))
         if (length(na_train) > 0) {
            med_map <- setNames(train_medians[[col]], train_medians$Month)
            train_data[[col]][na_train] <- med_map[as.character(train_data$Month[na_train])]
         }
         
         na_test <- which(is.na(test_data[[col]]))
         if (length(na_test) > 0) {
            med_map <- setNames(train_medians[[col]], train_medians$Month)
            test_data[[col]][na_test] <- med_map[as.character(test_data$Month[na_test])]
         }
      }
      
      rf_formula <- as.formula(paste(response_col, "~", paste(predictor_cols, collapse = " + ")))
      
      rf_model <- ranger(
         formula      = rf_formula,
         data         = train_data,
         num.trees    = ntree,
         mtry         = mtry,
         importance   = "permutation",
         num.threads  = num_threads,
         write.forest = TRUE,
         keep.inbag   = TRUE
      )
      
      preds <- predict(rf_model, data = test_data, num.threads = num_threads)$predictions
      obs   <- test_data[[response_col]]
      
      rmse_val <- Metrics::rmse(obs, preds)
      mae_val  <- Metrics::mae(obs, preds)
      
      oob_imp <- data.frame(
         Variable    = names(rf_model$variable.importance),
         IncMSE_OOB  = rf_model$variable.importance,
         BaselineMSE = rf_model$prediction.error,
         Fold        = i,
         row.names   = NULL
      )
      
      if (calc_test_imp) {
         test_imp_values <- calculate_test_importance(rf_model, test_data, response_col, predictor_cols, num_threads = num_threads)
         test_imp <- data.frame(
            Variable    = names(test_imp_values),
            IncMSE_Test = test_imp_values,
            Fold        = i,
            row.names   = NULL
         )
         combined_imp <- merge(oob_imp, test_imp, by = c("Variable", "Fold"))
      } else {
         test_imp <- NULL
         combined_imp <- oob_imp
      }
      
      results[[i]] <- list(
         model           = rf_model,
         fold            = i,
         train_years     = folds[[i]]$train_years,
         test_years      = folds[[i]]$test_years,
         train_data      = train_data, 
         metrics         = data.frame(
            Fold        = i,
            Train_Years = paste(folds[[i]]$train_years, collapse = "-"),
            Test_Years  = paste(folds[[i]]$test_years, collapse = "-"),
            RMSE        = rmse_val,
            MAE         = mae_val
         ),
         importance      = combined_imp,
         importance_oob  = oob_imp,
         importance_test = test_imp
      )
   }
   
   metrics_all         <- do.call(rbind, lapply(results, `[[`, "metrics"))
   importance_all      <- do.call(rbind, lapply(results, `[[`, "importance"))
   importance_oob_all  <- do.call(rbind, lapply(results, `[[`, "importance_oob"))
   importance_test_all <- if (calc_test_imp) do.call(rbind, lapply(results, `[[`, "importance_test")) else NULL
   
   return(list(
      folds           = results,
      metrics         = metrics_all,
      importance      = importance_all,
      importance_oob  = importance_oob_all,
      importance_test = importance_test_all
   ))
}

compare_importance_types <- function(importance_df, fold_num = NULL) {
   if (!is.null(fold_num)) {
      importance_df <- importance_df %>% filter(Fold == fold_num)
   }
   
   cor_by_fold <- importance_df %>%
      group_by(Fold) %>%
      summarise(
         Correlation = cor(IncMSE_OOB, IncMSE_Test, use = "complete.obs"),
         Spearman    = cor(IncMSE_OOB, IncMSE_Test, method = "spearman", use = "complete.obs"),
         .groups     = "drop"
      )
   
   cat("\n=== OOB vs Test Importance Correlation ===\n")
   print(cor_by_fold)
   
   cat("\nOverall Mean Correlation:", mean(cor_by_fold$Correlation, na.rm = TRUE), "\n")
   cat("Overall Mean Spearman:", mean(cor_by_fold$Spearman, na.rm = TRUE), "\n")
   
   return(cor_by_fold)
}