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
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   filter(Date > '2007-03-29') %>%
   dplyr::select(-contains('Inflows'))

hourly_data <- model_data
daily_data <- model_data %>%
   group_by(Date) %>%
   summarise(Year      = first(Year),
             Month     = first(Month),
             Day       = first(Day),
             DayOfYear = first(DayOfYear),
             across(
                where(is.numeric),
                ~ mean(.x, na.rm = TRUE),
                .names = "{.col}"
             ),
       .groups = "drop"
   ) %>%
   select(-HourSin, -HourCos, -MonthSin, -MonthCos)

   
# Group predictors into clusters
salinity_cluster <- model_data %>% dplyr::select(c(contains('Salinity')))
discharge_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Discharge')))
tide_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Tide')))
wind_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
time_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('Sin', 'Cos'))))

# Make expanding fold CV scheme for RF implementation
folds_hourly <- make_expanding_folds(hourly_data, initial_train_length = 6)
folds_daily <- make_expanding_folds(daily_data, date_col = 'Date', initial_train_length = 6)

# Random Forest hyperparameters
set.seed(123) 
ntree = 500   # number of trees to create
mtry = 10     # number of predictors to sample at each node (~ sqrt(predictors))

# Run the RF across expanding window scheme
rf_hourly <- run_rf_cv(data = hourly_data, folds = folds_hourly, response_col = 'Salinity', predictor_cols = 9 : ncol(hourly_data), ntree = ntree, mtry = mtry)
rf_daily <- run_rf_cv(data = daily_data, folds = folds_daily, response_col = 'Salinity', predictor_cols = 8 : ncol(daily_data), ntree = ntree, mtry = mtry)

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
   n_top = list(salinity = 2, discharge = 2, tide = 2, wind = 2, time = 2),
   importance_col = "IncMSE_OOB",
   show_importance = TRUE
)

top_vars_daily <- get_top_vars_by_group(
   importance_df = rf_daily$importance,
   group_dfs = group_list,
   n_top = list(salinity = 2, discharge = 2, tide = 2, wind = 2, time = 2),
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

if ("Variable" %in% names(top_vars_daily[[1]])) {
   # If show_importance = TRUE (dataframes with Variable and avg_imp)
   selected_vars_daily <- unlist(lapply(top_vars_daily, function(x) x$Variable), use.names = FALSE)
} else {
   # If show_importance = FALSE (just character vectors)
   selected_vars_daily <- unlist(top_vars_daily, use.names = FALSE)
}

# Collect only the screened model variables
hourly_data_screened <- hourly_data %>%
   dplyr::select(c(1 : 8), all_of(selected_vars_hourly))
daily_data_screened <- daily_data %>%
   dplyr::select(c(1 : 7), all_of(selected_vars_daily))

# Write output file
objects <- list(hourly_data_screened, daily_data_screened)
file_name <- list('FinalHourlyDataScreened', 'FinalDailyDataScreened')
write_qs_files(objects, 'Data/Tidied/Final', file_name)

# Write output file
objects <- list(rf_hourly, rf_daily)
file_name <- list('RFHourlyScreening', 'RFDailyScreening')
write_qs_files(objects, 'Outputs/Experiments/Models/RF', file_name)

# Clear global environment
rm(list = ls())

