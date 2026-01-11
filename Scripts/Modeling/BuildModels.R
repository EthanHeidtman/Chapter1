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
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelDataScreened.qs'))
model_data <- model_data %>%
   drop_na %>%
   dplyr::select(-contains('Inflows'))
   

# Create expanding folds for cross validation and make into proper form for tidymodels
folds <- make_expanding_folds(model_data, initial_train_length = 5)

# Elastic Net 
elastic_linear <- fit_model(
   data = model_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10), 
   mixture_range = c(0, 1),
   folds = folds
)

# Lasso Regression
lasso_linear <- fit_model(
   data = model_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(1, 1),  # Pure LASSO
   folds = folds
)

# Ridge Regression
ridge_linear <- fit_model(
   data = model_data,
   model_type = 'linear',
   penalty_range = c(0.001, 10),
   mixture_range = c(0, 0),  # Pure Ridge
   folds = folds
)

# Random Forest
rf <- fit_model(
   data = model_data,
   model_type = 'rf',
   trees = 300,
   rf_levels = 3,
   folds = folds
)


# Write output files
objects <- list(elastic_linear, lasso_linear, ridge_linear, rf)
file_names <- list('Elastic', 'Lasso', 'Ridge', 'RF')
write_qs_files(objects, 'Outputs/Experiments/Models', file_names)

# Clear global environment
rm(list = ls())
