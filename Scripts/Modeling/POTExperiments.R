# =============================================================================
# Script Name:    POTExperiments.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-07
# Last Updated:   2025-07-07
# Description:    Sources the CopulaPotModel from python. Then defines
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
PREDICTOR_PATH = 'Outputs/Experiments/LinearModeling/LinearPredictors.json'
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
   base_threshold = 0.2,
   target_threshold = 1.0,
   group_window_days = 7,
   param_smoothing = TRUE,
   min_exceedances_per_group = 10,
   random_state = as.integer(42)
)


# --- Stage 1: Distribution Screening ---
stage1_grid <- expand.grid(
   copula_type = 'gaussian', # 'student_t'
   tail_distribution = c("gpd", "lognormal", "gengamma", "burr", 'loglogistic', 'gamma'),
   stringsAsFactors = FALSE
)

# --- Stage 2: Threshold Sensitivity ---
stage2_grid <- expand.grid(
   base_threshold = c(0.1, 0.2, 0.3),
   target_threshold = c(0.6, 0.8, 1.0),
   stringsAsFactors = FALSE
)


stage1_results <- run_all_experiments(stage1_grid, base_config, "distribution_screening")







