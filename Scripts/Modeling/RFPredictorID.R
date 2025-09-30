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

N_TREES <- 1000

# Define seeds to test
seed_list <- c(10, 20, 40, 50, 60)

# Store results
rf_results <- list()

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
model_data <- model_data %>%
   dplyr::select(1 : 5, 'Salinity', 'Discharge' : 70) %>%
   arrange(DateTime) %>%
   relocate(DayOfYear, .after = Salinity) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   group_by(Date)
model_data <- model_data %>%
   summarise(across(-1, ~ mean(.x, na.rm = TRUE))) %>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

# Remove columns with >30% missing data
missing_pct <- colSums(is.na(model_data)) / nrow(model_data) * 100
high_missing <- names(missing_pct[missing_pct > 30])
exclude_cols <- c("DateTime", "Year", "Month", "Day", "Salinity")
high_missing <- setdiff(high_missing, exclude_cols)
if(length(high_missing) > 0) {
   cat(sprintf("Removing %d predictors with >30%% missing data\n", length(high_missing)))
   model_data <- model_data[, !names(model_data) %in% high_missing]
}

# For remaining missing values, use simple median imputation
numeric_cols <- sapply(model_data, is.numeric)
for(col in names(model_data)[numeric_cols]) {
   if(any(is.na(model_data[[col]]))) {
      median_val <- median(model_data[[col]], na.rm = TRUE)
      model_data[[col]][is.na(model_data[[col]])] <- median_val
   }
}

# Remove any remaining rows with missing Salinity
model_data <- model_data[!is.na(model_data$Salinity), ]
cat(sprintf("After cleaning: %d observations, %d predictors available\n", 
            nrow(model_data), sum(!names(model_data) %in% exclude_cols) - 1))


# Simple Train/Test Split for RF Screening ----
# Use chronological split - train on first 70%, test on last 30%
split_point <- floor(0.7 * nrow(model_data))
train_data <- model_data[1:split_point, ]
test_data <- model_data[(split_point + 1):nrow(model_data), ]

# Identify predictor columns
all_predictors <- setdiff(names(model_data), c("DateTime", "Year", "Month", "Day", "Salinity"))

predictors <- all_predictors

# Create formula
formula_str <- paste("Salinity ~", paste(predictors, collapse = " + "))
rf_formula <- as.formula(formula_str)

# Simple Importance Plot ----
plot_importance <- function(importance_df, top_n = 15) {
   top_vars <- head(importance_df, top_n)
   
   ggplot(top_vars, aes(x = reorder(Variable, Importance_scaled), y = Importance_scaled)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(
         title = paste("Top", top_n, "Variable Importance (scaled)"),
         subtitle = sprintf("RF with %d trees", N_TREES),
         x = "Variables", 
         y = "Scaled Importance (0-1)"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 12),
         plot.subtitle = element_text(size = 10)
      )
}


for (seed in seed_list) {
   cat(sprintf("\n===== Running RF with seed %d =====\n", seed))
   set.seed(seed)
   
   # Fit Random Forest
   rf_model <- ranger(
      formula = rf_formula,
      data = train_data,
      num.trees = N_TREES,
      importance = "permutation",
      seed = seed
   )
   
   # Test performance
   test_pred <- predict(rf_model, test_data)$predictions
   test_rmse <- sqrt(mean((test_pred - test_data$Salinity)^2))
   test_r2 <- cor(test_pred, test_data$Salinity)^2
   
   # Variable importance
   importance_scores <- rf_model$variable.importance
   importance_df <- data.frame(
      Variable = names(importance_scores),
      Importance = as.numeric(importance_scores),
      stringsAsFactors = FALSE
   ) %>%
      arrange(desc(Importance)) %>%
      mutate(
         Rank = row_number(),
         Importance_scaled = Importance / max(Importance)  # <-- scale 0-1
      )
   
   # Plot
   importance_plot <- plot_importance(importance_df, top_n = 30)
   
   # Store everything in results list
   rf_results[[as.character(seed)]] <- list(
      seed = seed,
      model = rf_model,
      test_rmse = test_rmse,
      test_r2 = test_r2,
      importance_df = importance_df,
      plot = importance_plot
   )
}


# rf_results[["10"]]$plot
# rf_results[['20']]$plot
# rf_results[['40']]$plot
# rf_results[['50']]$plot
# rf_results[['60']]$plot

# final_predictors <- c('DayOfYear', 'Norm_InflowDeficit', 'Norm_PowDischarge')
# 
# # Create clean dataset with selected predictors
# required_cols <- c('DateTime', 'Year', 'Month', 'Day', 'Salinity')
# clean_data <- model_data[, c(required_cols, final_predictors), drop = FALSE]

chosen_run <- rf_results[[1]]

screening_results <- list(
   selected_predictors = chosen_run$final_predictors,
   full_importance_ranking = chosen_run$importance_df,
   model_performance = list(
      oob_r_squared = chosen_run$rf_model$r.squared,
      test_rmse = chosen_run$test_rmse,
      test_r_squared = chosen_run$test_r2,
      n_trees = chosen_run$config$n_trees,
      n_predictors_screened = length(chosen_run$predictors),
      n_selected = chosen_run$config$top_n_selected
   ),
   data_summary = list(
      n_observations = nrow(model_data),
      train_size = nrow(train_data),
      test_size = nrow(test_data),
      original_predictors = length(chosen_run$predictors) + length(high_missing),
      removed_high_missing = length(high_missing),
      final_candidate_predictors = length(chosen_run$predictors)
   ),
   configuration = list(
      seed = chosen_run$config$seed,
      n_trees = chosen_run$config$n_trees,
      top_n_selected = chosen_run$config$top_n_selected,
      missing_threshold = chosen_run$config$missing_threshold,
      train_split = chosen_run$config$train_split
   )
)

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

