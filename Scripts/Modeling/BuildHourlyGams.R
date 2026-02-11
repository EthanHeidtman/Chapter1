# =============================================================================
# Script Name:    BuildHourlyGAMs.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(tidyverse)
library(tidymodels)
library(mgcv)
library(dplyr)
library(purrr)

# Source necessary functions
source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Utilities/FitGAM.R')

set.seed(123)

# Define lead times that were run
lead_times <- c(0, 1, 2, 4, 6, 8, 10, 12, 24, 36, 48, 72, 168, 336, 504)

# Initialize lists to store results
screened_data <- list()
rf_results <- list()
top_vars_by_k <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/FinalHourlyDataScreened_lag', k, '.qs')
   )
   
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/RF/RFHourlyScreening_lag', k, '.qs')
   )
}


# Loop through each lead time
for(k in 1 : length(lead_times)) {
   
   cat("\n=== Building GAM Model for lead time k =", lead_times[k], "hours ===\n")
   
   # Get data
   hourly_data_k <- screened_data[[k]]
   
   # Define groups for this specific k
   salinity_cluster_k <- hourly_data_k %>% dplyr::select(contains('Salinity'))
   discharge_cluster_k <- hourly_data_k %>% dplyr::select(c('Salinity', contains('Discharge')))
   tide_cluster_k <- hourly_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster_k <- hourly_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   
   group_list_k <- list(
      salinity = salinity_cluster_k,
      discharge = discharge_cluster_k,
      tide = tide_cluster_k,
      wind = wind_cluster_k
   )
   
   # Get top variables using your existing function
   top_vars_by_k[[k]] <- get_top_vars_by_group(
      importance_df = rf_results[[k]]$importance,
      group_dfs = group_list_k,
      n_top = list(salinity = 1, discharge = 1, tide = 1, wind = 1),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   # Top variables
   top_vars <- unname(vapply(top_vars_by_k[[k]], function(x) x$Variable, character(1)))
   
   # Clean data 
   hourly_data_k <- hourly_data_k %>%
      drop_na() %>%
      dplyr::select(c(1 : 8, top_vars, contains('Day'))) %>%
      { 
         # If there is a V wind variable → North (-) vs South (+)
         if (any(grepl("V", top_vars))) {
            
            wind_var <- top_vars[grepl("V", top_vars)][1]
            
            mutate(., WindDir = factor(
               ifelse(.data[[wind_var]] < 0, "North", "South")
            ))
            
            # Else if there is a U wind variable → East (-) vs West (+)
         } else if (any(grepl("U", top_vars))) {
            
            wind_var <- top_vars[grepl("U", top_vars)][1]
            
            mutate(., WindDir = factor(
               ifelse(.data[[wind_var]] < 0, "East", "West")
            ))
            
         } else {
            .
         }
      }
   
   # Make expanding fold CV scheme for linear model
   folds_hourly <- make_expanding_folds(hourly_data_k, initial_train_length = 6)
   
   # Make predictor set
   predictors_k <- names(hourly_data_k)[9 : ncol(hourly_data_k)]
   
   # Fit GAM
   gam_k <- fit_gam(
      data = hourly_data_k,
      predictors = predictors_k,
      folds = folds_hourly, 
      family_type = 'gaussian',
      transform_response = 'none',
      link = NULL,
      high_salinity_threshold = 0.16, # 75th percentile
      k_lagged_range = c(1, 1),
      k_flow_range = c(1, 50),
      k_temporal_range = c(1, 20),
      k_physical_range = c(1, 20),
      gam_levels  = 6,
      nthreads = 4
   )
   
   # Write model outputs with k in filename
   write_qs_files(
      list(gam_k), 
      'Outputs/Experiments/Models/HourlyGAM/', 
      list(paste0('Gam_', lead_times[k]))
   )
   
   cat("Completed lead time k =", lead_times[k], "hours\n")
}


# Clear global environment
rm(list = ls())
