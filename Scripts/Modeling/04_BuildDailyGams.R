# =============================================================================
# Script Name:    04_BuildDailyGAMs.R
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
lead_times <- seq(0, 30, 1)

# Initialize lists to store results
screened_data <- list()
rf_results <- list()
top_vars_by_k <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
}


# Loop through each lead time
for(k in 1 : length(lead_times)) {
   
   cat("\n=== Building GAM Model for lead time k =", lead_times[k], "days ===\n")
   
   # Get data
   daily_data_k <- screened_data[[k]]
   
   # Define groups for this specific k
   salinity_cluster <- daily_data_k %>% dplyr::select(c(contains('Salinity')))
   rolling_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('RollingDischarge', 'LagDischarge'))))
   flushing_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('Flux', 'Flush'))))
   tide_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   
   group_list_k <- list(
      salinity = salinity_cluster,
      rolling_discharge = rolling_discharge_cluster,
      flushing_discharge = flushing_discharge_cluster,
      tide = tide_cluster,
      wind = wind_cluster
   )
   
   # Get top variables using your existing function
   top_vars_by_k[[k]] <- get_top_vars_by_group(
      importance_df = rf_results[[k]]$importance,
      group_dfs = group_list_k,
      n_top = list(salinity = 1, 
                   rolling_discharge = 1, 
                   flushing_discharge = 1, 
                   tide = 1, 
                   wind = 1),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   # Top variables
   top_vars <- unname(vapply(top_vars_by_k[[k]], function(x) x$Variable, character(1)))
   
   # Clean data and create a factor for WindDirection 
   daily_data_k <- daily_data_k %>%
      drop_na() %>%
      dplyr::select(c(1 : "Salinity", top_vars)) %>%
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
   folds_daily <- make_expanding_folds(daily_data_k, initial_train_length = 6)
   
   # Make predictor set (the ones after Salinity)
   ncol_k <- ncol(daily_data_k)
   ncol_sal <- as.numeric(which(names(daily_data_k) == 'Salinity'))
   predictors_k <- names(daily_data_k)[(ncol_sal + 1) : ncol_k]
   
   # Fit GAM
   gam_k <- fit_gam(
      data = daily_data_k,
      predictors = predictors_k,
      folds = folds_daily,
      family_type = 'gaussian',
      transform_response = 'none',
      link = NULL,
      high_salinity_threshold = 0.16,
      k_lagged_range = c(1, 1),
      k_rolling_flow_range = if (k == 1) c(1, 1) else c(1, 50),
      k_flushing_flow_range = if (k == 1) c(1, 1) else c(1, 25),
      k_physical_range = if (k == 1) c(1, 1) else c(1, 20),
      gam_levels = if (k == 1) 1 else 10,
      nthreads = 4
   )
   
   # Write model outputs with k in filename
   write_qs_files(
      list(gam_k), 
      'Outputs/Experiments/Models/DailyGAM/', 
      list(paste0('Gam_', lead_times[k]))
   )
   
   cat("Completed lead time k =", lead_times[k], "days\n")
}


# Clear global environment
rm(list = ls())

