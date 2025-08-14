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
dirs <- c("Scripts/Functions/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# RF Configuration
SEED <- 42
N_TREES <- 1000
TOP_N_PREDICTORS <- 5

set.seed(SEED)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
model_data <- model_data %>%
   dplyr::select(1 : 5, 'Salinity', 85 : 159) %>%
   arrange(DateTime) %>%
   relocate(DayOfYear, .after = Salinity)

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

# Fit Random Forest
rf_model <- ranger(
   formula = rf_formula,
   data = train_data,
   num.trees = N_TREES,
   importance = "permutation",
   seed = SEED
)

cat(sprintf("RF training complete. Out-of-bag R²: %.4f\n", rf_model$r.squared))

# Test set performance (basic check)
test_pred <- predict(rf_model, test_data)$predictions
test_rmse <- sqrt(mean((test_pred - test_data$Salinity)^2))
test_r2 <- cor(test_pred, test_data$Salinity)^2

cat(sprintf("Test set RMSE: %.4f, R²: %.4f\n", test_rmse, test_r2))

importance_scores <- rf_model$variable.importance
importance_df <- data.frame(
   Variable = names(importance_scores),
   Importance = as.numeric(importance_scores),
   stringsAsFactors = FALSE
) %>%
   arrange(desc(Importance)) %>%
   mutate(
      Rank = row_number()
      )
   

# Display top predictors with risk assessment
cat(sprintf("\n=== TOP %d PREDICTORS WITH ROLLING WINDOW COMPATIBILITY ===\n", TOP_N_PREDICTORS * 2))

# Simple Importance Plot ----
plot_importance <- function(importance_df, top_n = 15) {
   top_vars <- head(importance_df, top_n)
   
   ggplot(top_vars, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("Top", top_n, "Variable Importance (Permutation)"),
           subtitle = sprintf("RF with %d trees, OOB R² = %.3f", N_TREES, rf_model$r.squared),
           x = "Variables", 
           y = "Permutation Importance") +
      theme_bw() +
      theme(plot.title = element_text(size = 12),
            plot.subtitle = element_text(size = 10))
}

importance_plot <- plot_importance(importance_df, top_n = 30)
print(importance_plot)

final_predictors <- c('DayOfYear', 'Norm_InflowDeficit', 'Norm_PowDischarge')

# Create clean dataset with selected predictors
required_cols <- c('DateTime', 'Year', 'Month', 'Day', 'Salinity')
clean_data <- model_data[, c(required_cols, final_predictors), drop = FALSE]

screening_results <- list(
   selected_predictors = final_predictors,
   full_importance_ranking = importance_df,
   model_performance = list(
      oob_r_squared = rf_model$r.squared,
      test_rmse = test_rmse,
      test_r_squared = test_r2,
      n_trees = N_TREES,
      n_predictors_screened = length(predictors),
      n_selected = TOP_N_PREDICTORS
   ),
   data_summary = list(
      n_observations = nrow(model_data),
      train_size = nrow(train_data),
      test_size = nrow(test_data),
      original_predictors = length(predictors) + length(high_missing),
      removed_high_missing = length(high_missing),
      final_candidate_predictors = length(predictors)
   ),
   configuration = list(
      seed = SEED,
      n_trees = N_TREES,
      top_n_selected = TOP_N_PREDICTORS,
      missing_threshold = 30,
      train_split = 0.7
   )
)


outputs <- list(screening_results)
file_names <- c('RF_Predictor_Screening')
write_qs_files(outputs, 'Outputs/Experiments/Phase1_RF', file_names, 
               preset = 'archive', format = 'json')

# Save clean data with selected predictors for Phase 2
outputs <- list(clean_data)
file_names <- c('CleanFinalModelData')
write_qs_files(outputs, 'Data/Tidied/Final', file_names, 
               preset = 'archive', format = 'csv')

# Save importance plot
ggsave("Outputs/Plots/Phase1_RF/RFVariableImportance.png", 
       importance_plot, width = 10, height = 6, dpi = 600)

