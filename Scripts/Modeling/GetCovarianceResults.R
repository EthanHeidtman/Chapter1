# =============================================================================
# Script Name:    GetCovarianceResults.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Gathers the outputs from the covariance experiment runs created
#                 by RunCovarianceExperiments.R. 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)
source('Scripts/Functions/Modeling/ExperimentHelpers.R')

DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
OUTPUT_PATH = 'Outputs/Experiments/CovarianceModeling'

data <- read.csv(DATA_PATH)
data <- data %>%
   mutate(
      DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   arrange(DateTime) %>%
   distinct(DateTime, .keep_all = TRUE) %>%
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))



# Get distribution screening results
dist_results <- load_covariance_results('DistributionScreening')

# Unnest predictions for plotting and analysis
dist_predictions <- unnest_covariance_results(dist_results, experiment_col = 'experiment_name')

dist_predictions <- dist_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>%
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

all_data <- left_join(dist_predictions, data, by = "DateTime")

