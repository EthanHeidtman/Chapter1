# =============================================================================
# Script Name:    RunCovarianceExperiments.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Sources the Rolling Covariance model from python. Then defines
#                 and organizes three sequential experiments to systematically 
#                 test distributions, threshold sensitivity, and the method used
#                 for parameter regression. Saves outputs to Outputs/Experiments/
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(reticulate)
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
library(future)
library(furrr)
#source_python('Scripts/Modeling/RollingCovarianceModel.py') 
source('Scripts/Functions/Modeling/ExperimentHelpers.R')

# Define parallelization setup
n_workers <- 8
plan(multisession, workers = n_workers)  

# Define File locations
DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
OUTPUT_PATH = 'Outputs/Experiments/CovarianceModeling'

# Create dir if missing
if (!dir.exists(OUTPUT_PATH)) {
   dir.create(OUTPUT_PATH, recursive = TRUE)
}

# Read in base data
data <- read.csv(DATA_PATH)
data <- data %>%
   mutate(
      DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   arrange(DateTime) %>%
   distinct(DateTime, .keep_all = TRUE)

# Define the Base Configuration
base_config <- list(
   data_csv = DATA_PATH,
   salinity_col = "Salinity",
   distribution_family = NULL,
   experiment_type = NULL,
   salinity_threshold = 1.0,
   window_length = 14
)


# Distributions to test:
dist_list <- c("burr", "gengamma", "gamma", "lognormal", "gpd", "loglogistic")

# Run distribution experiments in parallel
distribution_results <- future_map(
   dist_list[2 : 6],
   ~ run_one_experiment(
      experiment_name = .x,
      experiment_type = "DistributionScreening",
      base_config = base_config
   ),
   .progress = TRUE,
   .options = furrr_options(seed = TRUE)
)


# Run rolling window size experiments in parallel
window_sizes <- c(7, 10, 14, 30)

window_results <- future_map(
   as.character(window_sizes),
   ~ run_one_experiment(
      experiment_name = .x,  # here .x is the window length as string
      experiment_type = "WindowSizeScreening",
      base_config = modifyList(base_config, list(window_length = as.numeric(.x), distribution_family = 'gpd'))
   ),
   .progress = TRUE,
   .options = furrr_options(seed = TRUE)
)



# exp1_config <- combine_config(base_config, exp1_grid)
# config <- r_to_py(exp1_config)
# py$config <- config[0]

