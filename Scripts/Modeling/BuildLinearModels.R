# =============================================================================
# Script Name:    BuildBaseModels.R
# Project:        Chapter1
# Author:         Ethan Heidtman
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

# Define lead times that were run
lead_times <- c(0, 1, 6, 12, 24, 48, 72, 168, 336, 504)

# Initialize lists to store results
screened_data <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/FinalHourlyDataScreened_lag', k, '.qs')
   )
}

# Loop through each lead time
for(k in 1 : length(lead_times)) {
   
   cat("\n=== Building Linear Models for lead time k =", lead_times[k], "hours ===\n")
   
   # Get data
   hourly_data_k <- screened_data[[k]]
   
   # Clean data 
   hourly_data_k <- hourly_data_k %>%
      drop_na()

   # Make expanding fold CV scheme for linear model
   folds_hourly <- make_expanding_folds(hourly_data_k, initial_train_length = 6)
   
   # Fit elastic linear regression model
   elastic <- fit_model(
      data = hourly_data_k,
      model_type = 'linear',
      penalty_range = c(0.001, 10), 
      mixture_range = c(0, 1),
      folds = folds_hourly,
      eval_threshold = 0.16,
      standardize = FALSE
   )
   
   # Fit lasso regression model
   lasso <- fit_model(
      data = hourly_data_k,
      model_type = 'linear',
      penalty_range = c(0.001, 10),
      mixture_range = c(1, 1),  # Pure LASSO
      folds = folds_hourly,
      eval_threshold = 0.16,
      standardize = FALSE
   )
   
   # Ridge Regression
   ridge <- fit_model(
      data = hourly_data_k,
      model_type = 'linear',
      penalty_range = c(0.001, 10),
      mixture_range = c(0, 0),  # Pure Ridge
      folds = folds_hourly,
      eval_threshold = 0.16,
      standardize = FALSE
   )
   
   # Write model outputs with k in filename
   write_qs_files(
      list(elastic, lasso, ridge), 
      'Outputs/Experiments/Models/Linear/', 
      list(paste0('Elastic_', lead_times[k]), paste0('Lasso_', lead_times[k]), paste0('Ridge_', lead_times[k]))
   )
   
   cat("Completed lead time k =", lead_times[k], "hours\n")
}

# Clear global environment
rm(list = ls())


# # Write output files
# objects <- list(elastic_hourly, lasso_hourly, ridge_hourly, elastic_daily, lasso_daily, ridge_daily)
# file_names <- list('ElasticHourly', 'LassoHourly', 'RidgeHourly', 'ElasticDaily', 'LassoDaily', 'RidgeDaily')
# write_qs_files(objects, 'Outputs/Experiments/Models/Linear', file_names)


