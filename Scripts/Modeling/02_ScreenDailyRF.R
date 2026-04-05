# =============================================================================
# Script Name:    02_ScreenDailyRF.R
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
daily_data <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/FinalModelData.qs'))
daily_data <- daily_data %>%
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   relocate(Tide, .after = Salinity) %>%
   filter(Date > '2007-03-29') %>%
   dplyr::select(-contains('Inflows'))

# Define lead times to test (in days)
lead_times <- seq(0, 30, 1)

# Random Forest hyperparameters
set.seed(123) 
ntree = 300   # number of trees to create
mtry = 10     # number of predictors to sample at each node (~ sqrt(predictors))

# Loop through each lead time
for(k in lead_times) {
   
   cat("\n=== Processing lead time k =", k, "days ===\n")
   
   # Shift predictors by k
   if (k == 0) {
      daily_data_k <- daily_data # for k = 0, do not shift because it is already at k = 0
   } else {
      daily_data_k <- shift_predictors_by_k(daily_data, k = k) # 
   }
   
   
   # Group predictors into clusters
   salinity_cluster <- daily_data_k %>% dplyr::select(c(contains('Salinity')))
   rolling_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('RollingDischarge', 'LagDischarge'))))
   flushing_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('Flux', 'Flush'))))
   tide_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   
   # Make expanding fold CV scheme for RF implementation
   folds_daily <- make_expanding_folds(daily_data_k, initial_train_length = 6)
   
   # Run the RF across expanding window scheme
   rf_daily <- run_rf_cv(
      data = daily_data_k, 
      folds = folds_daily, 
      response_col = 'Salinity', 
      predictor_cols = 9 : ncol(daily_data_k), 
      ntree = ntree, 
      mtry = mtry
   )
   
   # Define the list of groups
   group_list <- list(
      salinity = salinity_cluster,
      rolling_discharge = rolling_discharge_cluster,
      flushing_discharge = flushing_discharge_cluster,
      tide = tide_cluster,
      wind = wind_cluster
   )
   
   # Collect the top variables for each group
   top_vars_daily <- get_top_vars_by_group(
      importance_df = rf_daily$importance,
      group_dfs = group_list,
      n_top = list(salinity = 4, 
                   rolling_discharge = 4, 
                   flushing_discharge = 4, 
                   tide = 4, 
                   wind = 4),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   if ("Variable" %in% names(top_vars_daily[[1]])) {
      # If show_importance = TRUE (dataframes with Variable and avg_imp)
      selected_vars_daily <- unlist(lapply(top_vars_daily, function(x) x$Variable), use.names = FALSE)
   } else {
      # If show_importance = FALSE (just character vectors)
      selected_vars_daily <- unlist(top_vars_daily, use.names = FALSE)
   }
   
   # Collect only the screened model variables
   daily_data_screened <- daily_data_k %>%
      dplyr::select(c(1 : 'Salinity'), all_of(selected_vars_daily))
   
   # Write screened data with k in filename
   write_qs_files(
      list(daily_data_screened), 
      'Data/Tidied/Final/Daily', 
      list(paste0('FinalDataScreened_lag', k))
   )
   
   # Write RF results with k in filename
   write_qs_files(
      list(rf_daily), 
      'Outputs/Experiments/Models/DailyRF', 
      list(paste0('RFDailyScreening_lag', k))
   )
   
   cat("Completed lead time k =", k, "days\n")
}

# Clear global environment
rm(list = ls())

