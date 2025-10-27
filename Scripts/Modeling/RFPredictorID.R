# =============================================================================
# Script Name:    RFPredictorID.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Uses a simple random forest to screen and identify the 3-5 
#                 most important predictors for salinity prediction. Produces
#                 basic importance plots and summary detailing the results. Then
#                 saves the identified predictors and data for the distribution
#                 fitting in the next phase.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ranger)
library(Metrics)
library(patchwork)

# Source necessary functions and utilities
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
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
model_data <- model_data %>%
   dplyr::select(1 : 5, 'Salinity', 'Discharge' : 81) %>%
   arrange(DateTime) %>%
   #relocate(DayOfYear, .after = Salinity) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   group_by(Date)


model_data_4h <- model_data %>%
   mutate(
      Hour4 = floor(hour(DateTime) / 4) * 4,
      Date4h = as.POSIXct(paste(Date, Hour4), format="%Y-%m-%d %H", tz = "UTC")
   ) %>%
   group_by(Date4h) %>%
   summarise(
      DateTime = first(DateTime),
      across(Salinity:RollingInflows90, ~ mean(.x, na.rm = TRUE))
   ) %>%
   ungroup() %>%
   mutate(Year = year(DateTime), 
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

model_data_12h <- model_data %>%
   mutate(
      Hour12 = floor(hour(DateTime) / 12) * 12,
      Date12h = as.POSIXct(paste(Date, Hour12), format="%Y-%m-%d %H", tz = "UTC")
   ) %>%
   group_by(Date12h) %>%
   summarise(
      DateTime = first(DateTime),
      across(Salinity:RollingInflows90, ~ mean(.x, na.rm = TRUE))
   ) %>%
   ungroup() %>%
   mutate(Year = year(DateTime), 
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

model_data_daily <- model_data %>%
   group_by(Date = as.Date(DateTime)) %>%
   summarise(
      DateTime = first(DateTime),
      across(Salinity:RollingInflows90, ~ mean(.x, na.rm = TRUE))
   ) %>%
   ungroup() %>%
   mutate(Year = year(DateTime), 
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))


# # Impute missing data by monthly median
# start_col <- which(names(model_data) == "Salinity")
# cols_to_impute <- names(model_data)[start_col:ncol(model_data)]
# model_data <- model_data %>%
#    group_by(Month) %>%
#    mutate(across(all_of(cols_to_impute),
#                  ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
#    ungroup()

# Make expanding fold CV scheme for RF implementation
folds_hourly <- make_expanding_folds(model_data)
folds_4h <- make_expanding_folds(model_data_4h)
folds_12h <- make_expanding_folds(model_data_12h)
folds_daily <- make_expanding_folds(model_data_daily)

set.seed(123) # set the random seed
ntree = 500  # number of trees to make
mtry = 15     # number of predictors to sample from at each node

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

# RF results
# rf_hourly <- run_rf_cv(data = model_data, folds = folds, response_col = 'Salinity', predictor_cols = 8:79, ntree = ntree, mtry = mtry)
rf_4h <- run_rf_cv(data = model_data_4h, folds = folds_4h, response_col = 'Salinity', predictor_cols = 7:78, ntree = ntree, mtry = mtry)
rf_12h <- run_rf_cv(data = model_data_12h, folds = folds_12h, response_col = 'Salinity', predictor_cols = 7:78, ntree = ntree, mtry = mtry)
rf_daily <- run_rf_cv(data = model_data_daily, folds = folds_daily, response_col = 'Salinity', predictor_cols = 7:78, ntree = ntree, mtry = mtry)


plot_error_metrics <- function(metrics_df) {
   # Reshape data for plotting
   metrics_long <- metrics_df %>%
      select(Fold, Test_Years, RMSE, MAE) %>%
      pivot_longer(cols = c(RMSE, MAE), 
                   names_to = "Metric", 
                   values_to = "Value")
   
   # Create the plot
   p <- ggplot(metrics_long, aes(x = Fold, y = Value, color = Metric, group = Metric)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = c("RMSE" = "#E74C3C", "MAE" = "#3498DB")) +
      labs(title = "Model Performance Across Expanding Window Folds",
           subtitle = "RMSE and MAE on Test Sets",
           x = "Fold (Test Year)",
           y = "Error Value",
           color = "Metric") +
      scale_x_continuous(breaks = metrics_df$Fold,
                         labels = metrics_df$Test_Years) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom")
   
   return(p)
}

plot_error_metrics(rf_daily$metrics)

plot_rmse_mae_separate <- function(metrics_df) {
   # RMSE plot
   p_rmse <- ggplot(metrics_df, aes(x = Fold, y = RMSE)) +
      geom_line(color = "#E74C3C", linewidth = 1) +
      geom_point(color = "#E74C3C", size = 3) +
      geom_hline(yintercept = mean(metrics_df$RMSE), 
                 linetype = "dashed", color = "gray50", alpha = 0.7) +
      labs(title = "Root Mean Squared Error by Fold",
           x = "Fold (Test Year)",
           y = "RMSE") +
      scale_x_continuous(breaks = metrics_df$Fold,
                         labels = metrics_df$Test_Years) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   # MAE plot
   p_mae <- ggplot(metrics_df, aes(x = Fold, y = MAE)) +
      geom_line(color = "#3498DB", linewidth = 1) +
      geom_point(color = "#3498DB", size = 3) +
      geom_hline(yintercept = mean(metrics_df$MAE), 
                 linetype = "dashed", color = "gray50", alpha = 0.7) +
      labs(title = "Mean Absolute Error by Fold",
           x = "Fold (Test Year)",
           y = "MAE") +
      scale_x_continuous(breaks = metrics_df$Fold,
                         labels = metrics_df$Test_Years) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   # Combine plots
   combined <- p_rmse / p_mae +
      plot_annotation(title = "Error Metrics Across Expanding Window CV",
                      theme = theme(plot.title = element_text(face = "bold", size = 14)))
   
   return(combined)
}

plot_rmse_mae_separate(rf_daily$metrics)

plot_mean_importance <- function(importance_df, top_n = 20) {
   # Calculate mean importance across folds
   mean_imp <- importance_df %>%
      group_by(Variable) %>%
      summarise(Mean_Importance = mean(IncMSE_OOB, na.rm = TRUE),
                SD_Importance = sd(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_Importance)) %>%
      slice_head(n = top_n)
   
   # Create plot
   p <- ggplot(mean_imp, aes(x = reorder(Variable, Mean_Importance), 
                             y = Mean_Importance)) +
      geom_col(fill = "#2ECC71", alpha = 0.8) +
      geom_errorbar(aes(ymin = Mean_Importance - SD_Importance,
                        ymax = Mean_Importance + SD_Importance),
                    width = 0.3, alpha = 0.6) +
      coord_flip() +
      labs(title = paste("Top", top_n, "Mean Variable Importance"),
           subtitle = "Error bars show ±1 SD across folds",
           x = "Variable",
           y = "Mean Importance") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14))
   
   return(p)
}

plot_mean_importance(rf_daily$importance, top_n = 30)

plot_importance_heatmap <- function(importance_df, top_n = 15) {
   # Get top variables by mean importance
   top_vars <- importance_df %>%
      group_by(Variable) %>%
      summarise(Mean_Imp = mean(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_Imp)) %>%
      slice_head(n = top_n) %>%
      pull(Variable)
   
   # Filter and prepare data
   imp_filtered <- importance_df %>%
      filter(Variable %in% top_vars) %>%
      mutate(Variable = factor(Variable, levels = rev(top_vars)))
   
   # Create heatmap
   p <- ggplot(imp_filtered, aes(x = Fold, y = Variable, fill = IncMSE_OOB)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = "#3498DB", mid = "#F39C12", high = "#E74C3C",
                           midpoint = median(imp_filtered$IncMSE_OOB, na.rm = TRUE)) +
      labs(title = "Variable Importance Evolution Across Folds",
           subtitle = paste("Top", top_n, "variables by mean IncMSE"),
           x = "Fold",
           y = "Variable",
           fill = "IncMSE_OOB") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5))
   
   return(p)
}

plot_importance_heatmap(rf_daily$importance)

plot_importance_trajectories <- function(importance_df, top_n = 10) {
   # Get top variables
   top_vars <- importance_df %>%
      group_by(Variable) %>%
      summarise(Mean_Imp = mean(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_Imp)) %>%
      slice_head(n = top_n) %>%
      pull(Variable)
   
   # Filter data
   imp_filtered <- importance_df %>%
      filter(Variable %in% top_vars)
   
   # Create line plot
   p <- ggplot(imp_filtered, aes(x = Fold, y = IncMSE_OOB, 
                                 color = Variable, group = Variable)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.6) +
      labs(title = "Variable Importance Trajectories",
           subtitle = paste("Top", top_n, "variables by mean IncMSE"),
           x = "Fold",
           y = "IncMSE",
           color = "Variable") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "right")
   
   return(p)
}

plot_importance_trajectories(rf_daily$importance, top_n = 20)

plot_fold_comparison <- function(importance_df, metrics_df, 
                                 test_years, top_n = 20) {
   # Get fold numbers for specified years
   folds <- metrics_df %>%
      filter(Test_Years %in% test_years) %>%
      pull(Fold)
   
   if(length(folds) == 0) {
      stop("No folds found for specified test years")
   }
   
   # Filter importance data
   imp_filtered <- importance_df %>%
      filter(Fold %in% folds)
   
   # Get top N variables across these folds
   top_vars <- imp_filtered %>%
      group_by(Variable) %>%
      summarise(Mean_Imp = mean(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_Imp)) %>%
      slice_head(n = top_n) %>%
      pull(Variable)
   
   # Prepare data for plotting
   plot_data <- imp_filtered %>%
      filter(Variable %in% top_vars) %>%
      left_join(metrics_df %>% select(Fold, Test_Years, RMSE, MAE), 
                by = "Fold") %>%
      mutate(Variable = factor(Variable, levels = rev(top_vars)),
             Fold_Label = paste0("Fold ", Fold, "\n(", Test_Years, ")"))
   
   # Create comparison plot
   p <- ggplot(plot_data, aes(x = IncMSE_OOB, y = Variable, fill = Fold_Label)) +
      geom_col(position = "dodge", alpha = 0.8) +
      labs(title = "Variable Importance Comparison Across Selected Folds",
           subtitle = paste("Test Years:", paste(test_years, collapse = ", ")),
           x = "IncMSE",
           y = "Variable",
           fill = "Fold") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom")
   
   return(p)
}

plot_fold_comparison(rf_daily$importance, rf_daily$metrics, c(2016, 2018, 2020))

plot_single_fold_detail <- function(importance_df, metrics_df, 
                                    test_year, top_n = 25) {
   # Get fold number
   fold_info <- metrics_df %>%
      filter(Test_Years == test_year)
   
   if(nrow(fold_info) == 0) {
      stop(paste("No fold found for test year:", test_year))
   }
   
   fold_num <- fold_info$Fold
   
   # Get importance for this fold
   fold_imp <- importance_df %>%
      filter(Fold == fold_num) %>%
      arrange(desc(IncMSE_OOB)) %>%
      slice_head(n = top_n) %>%
      mutate(Variable = factor(Variable, levels = rev(Variable)))
   
   # Create detailed plot
   p <- ggplot(fold_imp, aes(x = IncMSE_OOB, y = Variable)) +
      geom_col(aes(fill = IncMSE_OOB), alpha = 0.8) +
      scale_fill_gradient2(low = "#3498DB", mid = "#F39C12", high = "#E74C3C",
                           midpoint = median(fold_imp$IncMSE_OOB)) +
      labs(title = paste("Variable Importance for Test Year", test_year),
           subtitle = paste0("Fold ", fold_num, " | Training: ", 
                             fold_info$Train_Years,
                             "\nRMSE: ", round(fold_info$RMSE, 4), 
                             " | MAE: ", round(fold_info$MAE, 4)),
           x = "IncMSE (Increase in MSE)",
           y = "Variable") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10),
            legend.position = "none")
   
   return(p)
}

plot_single_fold_detail(rf_daily$importance, rf_daily$metrics, 2016)

plot_anomalous_folds <- function(importance_df, metrics_df, 
                                 metric = "RMSE", n_folds = 3, 
                                 top_n = 15) {
   # Identify best and worst performing folds
   if(metric == "RMSE") {
      best_folds <- metrics_df %>% 
         arrange(RMSE) %>% 
         slice_head(n = n_folds) %>% 
         mutate(Performance = "Best")
      worst_folds <- metrics_df %>% 
         arrange(desc(RMSE)) %>% 
         slice_head(n = n_folds) %>% 
         mutate(Performance = "Worst")
   } else {
      best_folds <- metrics_df %>% 
         arrange(MAE) %>% 
         slice_head(n = n_folds) %>% 
         mutate(Performance = "Best")
      worst_folds <- metrics_df %>% 
         arrange(desc(MAE)) %>% 
         slice_head(n = n_folds) %>% 
         mutate(Performance = "Worst")
   }
   
   selected_folds <- bind_rows(best_folds, worst_folds)
   
   # Get importance data
   imp_data <- importance_df %>%
      filter(Fold %in% selected_folds$Fold) %>%
      left_join(selected_folds %>% select(Fold, Test_Years, Performance, 
                                          RMSE, MAE), 
                by = "Fold")
   
   # Get top variables
   top_vars <- imp_data %>%
      group_by(Variable) %>%
      summarise(Mean_Imp = mean(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_Imp)) %>%
      slice_head(n = top_n) %>%
      pull(Variable)
   
   # Prepare plot data
   plot_data <- imp_data %>%
      filter(Variable %in% top_vars) %>%
      mutate(Variable = factor(Variable, levels = rev(top_vars)),
             Fold_Label = paste0(Test_Years, " (", 
                                 round(get(metric), 4), ")"),
             Performance = factor(Performance, levels = c("Best", "Worst")))
   
   # Create faceted plot
   p <- ggplot(plot_data, aes(x = IncMSE_OOB, y = Variable, fill = Performance)) +
      geom_col(alpha = 0.8) +
      facet_wrap(~Fold_Label, ncol = 2) +
      scale_fill_manual(values = c("Best" = "#2ECC71", "Worst" = "#E74C3C")) +
      labs(title = paste("Variable Importance: Best vs Worst Performing Folds"),
           subtitle = paste("Based on", metric),
           x = "IncMSE",
           y = "Variable") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom",
            strip.text = element_text(face = "bold"))
   
   return(p)
}

plot_anomalous_folds(rf_daily$importance, rf_daily$metrics)

compare_fold_to_average <- function(importance_df, test_year, top_n = 20) {
   # Calculate mean importance across all folds
   mean_imp <- importance_df %>%
      group_by(Variable) %>%
      summarise(Mean_IncMSE = mean(IncMSE_OOB, na.rm = TRUE))
   
   # Get specific fold data
   fold_num <- unique(importance_df$Fold[importance_df$Fold == 
                                            which(grepl(test_year, unique(importance_df$Fold)))])
   
   # If fold lookup by year doesn't work, allow direct fold number
   if(length(fold_num) == 0) {
      # Assume test_year might be a fold number
      fold_num <- as.numeric(test_year)
   }
   
   fold_imp <- importance_df %>%
      filter(Fold == fold_num) %>%
      select(Variable, Fold_IncMSE = IncMSE_OOB)
   
   # Combine and calculate difference
   comparison <- mean_imp %>%
      inner_join(fold_imp, by = "Variable") %>%
      mutate(Difference = Fold_IncMSE - Mean_IncMSE,
             Deviation = Difference / Mean_IncMSE * 100) %>%
      arrange(desc(abs(Difference))) %>%
      slice_head(n = top_n) %>%
      mutate(Variable = factor(Variable, levels = rev(Variable)))
   
   # Create diverging bar chart
   p <- ggplot(comparison, aes(x = Deviation, y = Variable, 
                               fill = Deviation > 0)) +
      geom_col(alpha = 0.8) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
      scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
                        labels = c("Below Average", "Above Average")) +
      labs(title = paste("Fold", fold_num, "Importance vs Average"),
           subtitle = "Variables with largest deviations from mean importance",
           x = "% Deviation from Mean IncMSE",
           y = "Variable",
           fill = "Performance") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom")
   
   return(p)
}

compare_fold_to_average(rf_daily$importance, test_year = 2016)

plot_variable_group_trajectories <- function(importance_df, 
                                             pattern, 
                                             pattern_name = "Variables") {
   # Filter variables matching pattern (case-insensitive)
   matched_vars <- importance_df %>%
      filter(grepl(pattern, Variable, ignore.case = TRUE)) %>%
      pull(Variable) %>%
      unique()
   
   if(length(matched_vars) == 0) {
      stop(paste("No variables found matching pattern:", pattern))
   }
   
   # Filter data
   plot_data <- importance_df %>%
      filter(Variable %in% matched_vars)
   
   # Create line plot
   p <- ggplot(plot_data, aes(x = Fold, y = IncMSE_OOB, 
                              color = Variable, group = Variable)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.7) +
      labs(title = paste(pattern_name, "Importance Across Folds"),
           subtitle = paste("Pattern:", pattern, "|", length(matched_vars), "variables"),
           x = "Fold",
           y = "IncMSE",
           color = "Variable") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "right")
   
   return(p)
}

plot_variable_group_trajectories(rf_daily$importance, pattern = 'U|V', pattern_name = 'Wind Variables')

plot_variable_group_heatmap <- function(importance_df, 
                                        pattern, 
                                        pattern_name = "Variables") {
   # Filter variables matching pattern
   matched_vars <- importance_df %>%
      filter(grepl(pattern, Variable, ignore.case = TRUE)) %>%
      pull(Variable) %>%
      unique()
   
   if(length(matched_vars) == 0) {
      stop(paste("No variables found matching pattern:", pattern))
   }
   
   # Prepare data
   plot_data <- importance_df %>%
      filter(Variable %in% matched_vars) %>%
      mutate(Variable = factor(Variable, levels = rev(sort(matched_vars))))
   
   # Create heatmap
   p <- ggplot(plot_data, aes(x = Fold, y = Variable, fill = IncMSE_OOB)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = "#3498DB", mid = "#F39C12", high = "#E74C3C",
                           midpoint = median(plot_data$IncMSE_OOB, na.rm = TRUE)) +
      labs(title = paste(pattern_name, "Importance Evolution"),
           subtitle = paste("Pattern:", pattern),
           x = "Fold",
           y = "Variable",
           fill = "IncMSE") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            axis.text.y = element_text(size = 10))
   
   return(p)
}

plot_variable_group_heatmap(rf_daily$importance, pattern = 'U|V', pattern_name = 'Wind Variables')

compare_variable_groups <- function(importance_df, 
                                    patterns, 
                                    group_names = NULL) {
   # Set default group names if not provided
   if(is.null(group_names)) {
      group_names <- patterns
   }
   
   # Create list to store data for each group
   group_data <- list()
   
   for(i in seq_along(patterns)) {
      matched_vars <- importance_df %>%
         filter(grepl(patterns[i], Variable, ignore.case = TRUE)) %>%
         pull(Variable) %>%
         unique()
      
      if(length(matched_vars) > 0) {
         group_data[[i]] <- importance_df %>%
            filter(Variable %in% matched_vars) %>%
            mutate(Group = group_names[i])
      }
   }
   
   # Combine all groups
   plot_data <- bind_rows(group_data)
   
   if(nrow(plot_data) == 0) {
      stop("No variables found for any of the patterns")
   }
   
   # Calculate mean importance by group and fold
   summary_data <- plot_data %>%
      group_by(Group, Fold) %>%
      summarise(Mean_IncMSE = mean(IncMSE_OOB, na.rm = TRUE),
                SD_IncMSE = sd(IncMSE_OOB, na.rm = TRUE),
                N_vars = n_distinct(Variable),
                .groups = "drop")
   
   # Create plot
   p <- ggplot(summary_data, aes(x = Fold, y = Mean_IncMSE, 
                                 color = Group, group = Group)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.7) +
      geom_ribbon(aes(ymin = Mean_IncMSE - SD_IncMSE,
                      ymax = Mean_IncMSE + SD_IncMSE,
                      fill = Group),
                  alpha = 0.2, color = NA) +
      labs(title = "Variable Group Importance Comparison",
           subtitle = "Mean IncMSE ± SD across folds",
           x = "Fold",
           y = "Mean IncMSE",
           color = "Variable Group",
           fill = "Variable Group") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom")
   
   return(p)
}


plot_variable_rank_stability <- function(importance_df, 
                                         pattern, 
                                         pattern_name = "Variables") {
   # Filter variables matching pattern
   matched_vars <- importance_df %>%
      filter(grepl(pattern, Variable, ignore.case = TRUE)) %>%
      pull(Variable) %>%
      unique()
   
   if(length(matched_vars) == 0) {
      stop(paste("No variables found matching pattern:", pattern))
   }
   
   # Calculate ranks within each fold
   rank_data <- importance_df %>%
      group_by(Fold) %>%
      mutate(Rank = rank(-IncMSE_OOB, ties.method = "first")) %>%
      ungroup() %>%
      filter(Variable %in% matched_vars)
   
   # Create rank evolution plot
   p <- ggplot(rank_data, aes(x = Fold, y = Rank, 
                              color = Variable, group = Variable)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.7) +
      scale_y_reverse() +  # Lower rank (higher importance) at top
      labs(title = paste(pattern_name, "Rank Stability"),
           subtitle = paste("Lower rank = higher importance | Pattern:", pattern),
           x = "Fold",
           y = "Importance Rank",
           color = "Variable") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "right")
   
   return(p)
}

plot_variable_rank_stability(rf_daily$importance, pattern = 'U|V', pattern_name = 'Wind Variables')

summarize_variable_group <- function(importance_df, pattern) {
   # Filter variables matching pattern
   matched_vars <- importance_df %>%
      filter(grepl(pattern, Variable, ignore.case = TRUE)) %>%
      pull(Variable) %>%
      unique()
   
   if(length(matched_vars) == 0) {
      cat("No variables found matching pattern:", pattern, "\n")
      return(NULL)
   }
   
   # Calculate summary statistics
   summary <- importance_df %>%
      filter(Variable %in% matched_vars) %>%
      group_by(Variable) %>%
      summarise(
         Mean_IncMSE = mean(IncMSE_OOB, na.rm = TRUE),
         SD_IncMSE = sd(IncMSE_OOB, na.rm = TRUE),
         Min_IncMSE = min(IncMSE_OOB, na.rm = TRUE),
         Max_IncMSE = max(IncMSE_OOB, na.rm = TRUE),
         CV = SD_IncMSE / Mean_IncMSE,  # Coefficient of variation
         .groups = "drop"
      ) %>%
      arrange(desc(Mean_IncMSE))
   
   cat("\n=== Variable Group Summary ===\n")
   cat("Pattern:", pattern, "\n")
   cat("Number of variables:", length(matched_vars), "\n")
   cat("Variables:", paste(matched_vars, collapse = ", "), "\n\n")
   
   #print(summary)
   
   return(summary)
}

wind_vars <- summarize_variable_group(rf_daily$importance, pattern = 'U|V')

plot_oob_vs_test_importance <- function(importance_df, top_n = 20) {
   # Get top variables by mean OOB importance
   top_vars <- importance_df %>%
      group_by(Variable) %>%
      summarise(Mean_OOB = mean(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_OOB)) %>%
      slice_head(n = top_n) %>%
      pull(Variable)
   
   # Calculate means for both types
   comparison <- importance_df %>%
      filter(Variable %in% top_vars) %>%
      group_by(Variable) %>%
      summarise(
         Mean_OOB = mean(IncMSE_OOB, na.rm = TRUE),
         Mean_Test = mean(IncMSE_Test, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(Mean_OOB, Mean_Test),
                   names_to = "Type",
                   values_to = "Importance") %>%
      mutate(Type = gsub("Mean_", "", Type))
   
   # Create comparison plot
   p <- ggplot(comparison, aes(x = reorder(Variable, Importance), 
                               y = Importance, fill = Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("OOB" = "#3498DB", "Test" = "#E74C3C")) +
      coord_flip() +
      labs(title = "OOB vs Test Set Variable Importance",
           subtitle = paste("Top", top_n, "variables by mean OOB importance"),
           x = "Variable",
           y = "Mean IncMSE",
           fill = "Importance Type") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom")
   
   return(p)
}

plot_oob_vs_test_importance(rf_daily$importance)

plot_importance_correlation <- function(importance_df, fold_num = NULL) {
   
   plot_data <- importance_df
   if (!is.null(fold_num)) {
      plot_data <- plot_data %>% filter(Fold == fold_num)
      title_suffix <- paste("(Fold", fold_num, ")")
   } else {
      title_suffix <- "(All Folds)"
   }
   
   # Calculate correlation
   cor_val <- cor(plot_data$IncMSE_OOB, plot_data$IncMSE_Test, 
                  use = "complete.obs")
   
   # Create scatter plot
   p <- ggplot(plot_data, aes(x = IncMSE_OOB, y = IncMSE_Test)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = "#E74C3C") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
                  color = "gray50", alpha = 0.7) +
      labs(title = paste("OOB vs Test Importance Correlation", title_suffix),
           subtitle = paste("Pearson r =", round(cor_val, 3)),
           x = "OOB Importance (Training)",
           y = "Test Importance (Held-out)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 14))
   
   return(p)
}

plot_importance_correlation(rf_daily$importance)

plot_importance_disagreement <- function(importance_df, top_n = 15) {
   # Calculate ranks for each type
   ranked <- importance_df %>%
      group_by(Fold) %>%
      mutate(
         Rank_OOB = rank(-IncMSE_OOB, ties.method = "first"),
         Rank_Test = rank(-IncMSE_Test, ties.method = "first"),
         Rank_Diff = abs(Rank_OOB - Rank_Test)
      ) %>%
      ungroup()
   
   # Get variables with largest rank disagreements on average
   disagreement <- ranked %>%
      group_by(Variable) %>%
      summarise(Mean_Rank_Diff = mean(Rank_Diff, na.rm = TRUE)) %>%
      arrange(desc(Mean_Rank_Diff)) %>%
      slice_head(n = top_n)
   
   # Get detailed data for these variables
   plot_data <- ranked %>%
      filter(Variable %in% disagreement$Variable) %>%
      select(Variable, Fold, Rank_OOB, Rank_Test) %>%
      pivot_longer(cols = c(Rank_OOB, Rank_Test),
                   names_to = "Type",
                   values_to = "Rank") %>%
      mutate(Type = gsub("Rank_", "", Type),
             Variable = factor(Variable, levels = disagreement$Variable))
   
   # Create plot
   p <- ggplot(plot_data, aes(x = Fold, y = Rank, color = Type, group = Type)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.7) +
      facet_wrap(~Variable, scales = "free_y") +
      scale_y_reverse() +
      scale_color_manual(values = c("OOB" = "#3498DB", "Test" = "#E74C3C")) +
      labs(title = "Variables with Largest OOB vs Test Rank Disagreements",
           subtitle = paste("Top", top_n, "variables by mean rank difference"),
           x = "Fold",
           y = "Importance Rank (lower = more important)",
           color = "Type") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom",
            strip.text = element_text(face = "bold", size = 9))
   
   return(p)
}

plot_importance_disagreement(rf_daily$importance)










outputs <- list(screening_results)
file_names <- c('RF_Predictor_Screening')
write_qs_files(outputs, 'Outputs/Experiments/Phase1_RF', file_names, 
               preset = 'archive', format = 'json')

# Save clean data with selected predictors for Phase 2
outputs <- list(model_data)
file_names <- c('CleanFinalModelData')
write_qs_files(outputs, 'Data/Tidied/Final', file_names, 
               preset = 'archive', format = 'csv')


# Loop over rf_results and save each plot
for (i in seq_along(rf_results)) {
   run <- rf_results[[i]]
   
   # Construct file name using seed
   file_name <- paste0("Outputs/Plots/Phase1_RF/", "rf_importance_seed_", run$seed, ".png")
   
   # Save the plot
   ggsave(
      filename = file_name,
      plot = run$plot,
      width = 10,
      height = 6,
      dpi = 600,
      device = ragg::agg_png
   )
   gc()
}

