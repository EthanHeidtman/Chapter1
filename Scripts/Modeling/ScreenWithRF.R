# =============================================================================
# Script Name:    ScreenWithRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Uses a simple random forest to screen a dataset of ~90 predictors
#                 and identify the top variables in each grouping of variable
#                 types. Selects the best and saves a screened version of the data.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ranger)    # For quick random forest implementation

# Source necessary functions 
source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/ShiftPredictors.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Utilities/PerformRFCV.R')

# Read in model data
hourly_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
hourly_data <- hourly_data %>%
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   filter(Date > '2007-03-29') %>%
   dplyr::select(-contains('Inflows'))


# Define lead times to test (in hours)
lead_times <- c(0, 1, 6, 12, 24, 48, 72, 168, 336, 504)

# Random Forest hyperparameters
set.seed(123) 
ntree = 300   # number of trees to create
mtry = 10     # number of predictors to sample at each node (~ sqrt(predictors))

# Loop through each lead time
for(k in lead_times) {
   
   cat("\n=== Processing lead time k =", k, "hours ===\n")
   
   # Shift predictors by k
   if (k == 0) {
      hourly_data_k <- hourly_data # for k = 0, do not shift because it is already at k = 0
   } else {
      hourly_data_k <- shift_predictors_by_k(hourly_data, k = k) # 
   }
   
   
   # Group predictors into clusters
   salinity_cluster <- hourly_data_k %>% dplyr::select(c(contains('Salinity')))
   discharge_cluster <- hourly_data_k %>% dplyr::select(c('Salinity', contains('Discharge')))
   tide_cluster <- hourly_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster <- hourly_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   time_cluster <- hourly_data_k %>% dplyr::select(c('Salinity', contains(c('Sin', 'Cos'))))
   
   # Make expanding fold CV scheme for RF implementation
   folds_hourly <- make_expanding_folds(hourly_data_k, initial_train_length = 6)
   
   # Run the RF across expanding window scheme
   rf_hourly <- run_rf_cv(
      data = hourly_data_k, 
      folds = folds_hourly, 
      response_col = 'Salinity', 
      predictor_cols = 9 : ncol(hourly_data_k), 
      ntree = ntree, 
      mtry = mtry
   )
   
   # Define the list of groups
   group_list <- list(
      salinity = salinity_cluster,
      discharge = discharge_cluster,
      tide = tide_cluster,
      wind = wind_cluster,
      time = time_cluster
   )
   
   # Collect the top variables for each group
   top_vars_hourly <- get_top_vars_by_group(
      importance_df = rf_hourly$importance,
      group_dfs = group_list,
      n_top = list(salinity = 3, discharge = 3, tide = 3, wind = 3, time = 3),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   if ("Variable" %in% names(top_vars_hourly[[1]])) {
      # If show_importance = TRUE (dataframes with Variable and avg_imp)
      selected_vars_hourly <- unlist(lapply(top_vars_hourly, function(x) x$Variable), use.names = FALSE)
   } else {
      # If show_importance = FALSE (just character vectors)
      selected_vars_hourly <- unlist(top_vars_hourly, use.names = FALSE)
   }
   
   # Collect only the screened model variables
   hourly_data_screened <- hourly_data_k %>%
      dplyr::select(c(1 : 8), all_of(selected_vars_hourly))
   
   # Write screened data with k in filename
   write_qs_files(
      list(hourly_data_screened), 
      'Data/Tidied/Final', 
      list(paste0('FinalHourlyDataScreened_lag', k))
   )
   
   # Write RF results with k in filename
   write_qs_files(
      list(rf_hourly), 
      'Outputs/Experiments/Models/RF', 
      list(paste0('RFHourlyScreening_lag', k))
   )
   
   cat("Completed lead time k =", k, "hours\n")
}

# Clear global environment
rm(list = ls())

