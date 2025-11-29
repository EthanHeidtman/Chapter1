# =============================================================================
# Script Name:    ScreenWithRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-11-25
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
   filter(Date > '2007-03-29') 

# Group predictors into clusters
inflow_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Inflows')))
discharge_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Discharge')))
tide_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Tide')))
wind_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
time_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('Sin', 'Cos'))))

# Make expanding fold CV scheme for RF implementation
folds_hourly <- make_expanding_folds(model_data, initial_train_length = 6)

# Random Forest hyperparameters
set.seed(123) 
ntree = 500   # number of trees to create
mtry = 10     # number of predictors to sample at each node (~ sqrt(predictors))

# Run the RF across expanding window scheme
rf_hourly <- run_rf_cv(data = model_data, folds = folds_hourly, response_col = 'Salinity', predictor_cols = 9 : 92, ntree = ntree, mtry = mtry)

# Function to collect the top variables from each group
get_top_vars_by_group <- function(importance_df, group_dfs, n_top = 2, 
                                   importance_col = "IncMSE_OOB",
                                   show_importance = TRUE) {
   
   # Average importance across folds
   avg_importance <- importance_df %>%
      group_by(Variable) %>%
      summarise(avg_imp = mean(.data[[importance_col]], na.rm = TRUE)) %>%
      arrange(desc(avg_imp))
   
   # Function to get top n vars from a single group
   get_top_from_group <- function(group_df, n) {
      group_vars <- setdiff(colnames(group_df), "Salinity")
      
      group_importance <- avg_importance %>%
         filter(Variable %in% group_vars) %>%
         slice_head(n = n)
      
      if (show_importance) {
         return(group_importance)
      } else {
         return(group_importance$Variable)
      }
   }
   
   # Handle different input types for n_top
   if (is.list(n_top)) {
      # User provided specific n for each group
      top_vars <- mapply(get_top_from_group, 
                         group_dfs, 
                         n_top[names(group_dfs)],
                         SIMPLIFY = FALSE)
   } else {
      # Use same n for all groups
      top_vars <- lapply(group_dfs, get_top_from_group, n = n_top)
   }
   
   names(top_vars) <- names(group_dfs)
   return(top_vars)
}

# Define the list of groups
group_list <- list(
   inflow = inflow_cluster,
   discharge = discharge_cluster,
   tide = tide_cluster,
   wind = wind_cluster,
   time = time_cluster
)

# Collect the top variables for each group
top_vars <- get_top_vars_by_group(
   importance_df = rf_hourly$importance,
   group_dfs = group_list,
   n_top = list(inflow = 2, discharge = 2, tide = 2, wind = 2, time = 2),
   importance_col = "IncMSE_OOB",
   show_importance = TRUE
)

if ("Variable" %in% names(top_vars[[1]])) {
   # If show_importance = TRUE (dataframes with Variable and avg_imp)
   selected_vars <- unlist(lapply(top_vars, function(x) x$Variable), use.names = FALSE)
} else {
   # If show_importance = FALSE (just character vectors)
   selected_vars <- unlist(top_vars, use.names = FALSE)
}

# Collect only the screened model variables
model_data_screened <- model_data %>%
   dplyr::select(c(1 : 8), all_of(selected_vars))

# Write output file
objects <- list(model_data_screened)
file_name <- list('FinalModelDataScreened')
write_qs_files(objects, 'Data/Tidied/Final', file_name)

# Write output file
objects <- list(rf_hourly)
file_name <- list('RFScreening')
write_qs_files(objects, 'Outputs/Experiments/Models', file_name)

# Clear global environment
rm(list = ls())

