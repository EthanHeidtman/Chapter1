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

set.seed(123)

# Source necessary functions 
source('Scripts/Utilities/LinearUtilities.R')
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
hourly_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalHourlyDataScreened.qs'))
daily_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalDailyDataScreened.qs'))

hourly_data <- hourly_data %>% 
   drop_na() %>% mutate_if(is.numeric, round, digits = 3) %>%
   dplyr::select(-contains('Inflows'))
daily_data <- daily_data %>% 
   drop_na() %>% mutate_if(is.numeric, round, digits = 3) %>%
   dplyr::select(-contains('Inflows')) %>%
   rename(DateTime = Date)


# Create expanding folds for cross validation and make into proper form for tidymodels
folds_hourly <- make_expanding_folds(hourly_data, initial_train_length = 5)
folds_daily <- make_expanding_folds(daily_data, initial_train_length = 6)

# Elastic Net 
elastic_hourly <- fit_model(
   data = hourly_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10), 
   mixture_range = c(0, 1),
   folds = folds_hourly,
   eval_threshold = 0.16,
   standardize = FALSE
)

# Lasso Regression
lasso_hourly <- fit_model(
   data = hourly_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(1, 1),  # Pure LASSO
   folds = folds_hourly,
   eval_threshold = 0.16,
   standardize = FALSE
)

# Ridge Regression
ridge_hourly <- fit_model(
   data = hourly_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(0, 0),  # Pure Ridge
   folds = folds_hourly,
   eval_threshold = 0.16,
   standardize = FALSE
)

# Elastic Net 
elastic_daily <- fit_model(
   data = daily_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10), 
   mixture_range = c(0, 1),
   folds = folds_daily,
   eval_threshold = 0.16,
   standardize = FALSE
)

# Lasso Regression
lasso_daily <- fit_model(
   data = daily_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(1, 1),  # Pure LASSO
   folds = folds_daily,
   eval_threshold = 0.16,
   standardize = FALSE
)

# Ridge Regression
ridge_daily <- fit_model(
   data = daily_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(0, 0),  # Pure Ridge
   folds = folds_daily,
   eval_threshold = 0.16,
   standardize = FALSE
)


# Write output files
objects <- list(elastic_hourly, lasso_hourly, ridge_hourly, elastic_daily, lasso_daily, ridge_daily)
file_names <- list('ElasticHourly', 'LassoHourly', 'RidgeHourly', 'ElasticDaily', 'LassoDaily', 'RidgeDaily')
write_qs_files(objects, 'Outputs/Experiments/Models/Linear', file_names)

# Clear global environment
rm(list = ls())
