# =============================================================================
# Script Name:    RollingCovarianceExperiments.py
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
source_python('Scripts/Modeling/CopulaPotModel.py') # Contains the copula model
source('Scripts/Functions/Modeling/ExperimentHelpers.R')

# Define File locations
DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
#PREDICTOR_PATH = 'Outputs/Experiments/LinearModeling/LinearPredictors.json'
OUTPUT_PATH = 'Outputs/Experiments/'

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
   predictors_json = PREDICTOR_PATH,
   salinity_col = "Salinity",
   copula_type = 'gaussian',
   base_threshold = 0.2,
   target_threshold = 0.8,
   group_window_days = 7,
   param_smoothing = FALSE,
   min_exceedances_per_group = 10,
   random_state = as.integer(42)
)


# Experiment 1: Distribution screening
exp1_grid <- list(
   tail_distribution = c("burr", "gengamma", "gamma", "lognormal", 'gpd', 'loglogistic'),
   copula_type = 'gaussian', 
   group_window_days = 7
)

exp1_config <- combine_config(base_config, exp1_grid)
config <- r_to_py(exp1_config)
py$config <- config[0]

exp1 <- run_experiment('DistributionScreening', base_config, exp1_grid)







