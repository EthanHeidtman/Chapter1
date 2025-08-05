# =============================================================================
# Script Name:    TwoStageExperiments.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Sources the two-stage model approach form python. Then defines
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
source_python('Scripts/Modeling/TwoStageModel.py') # Contains the two stage model
source('Scripts/Functions/Modeling/ExperimentHelpers.R')
source('Scripts/Modeling/Experiment1Plots.R')
# source('Scripts/Modeling/Experiment2Plots.R')
# source('Scripts/Modeling/Experiment3Plots.R')

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
   target_threshold = 0.8,
   group_window_days = 7,
   param_regression_method = "rf",  # default, can be overridden
   min_exceedances_per_group = 10,
   rolling_window_approach = TRUE,
   param_smoothing = TRUE,
   random_state = as.integer(42)
)

# --- Stage 1: Distribution Screening ---
stage1_grid <- expand.grid(
   tail_distribution = c("gpd", "lognormal", "gengamma", "burr", 'loglogistic', 'gamma'),
   stringsAsFactors = FALSE
)

# --- Stage 2: Threshold Sensitivity ---
stage2_grid <- expand.grid(
   base_threshold = c(0.1, 0.2, 0.3),
   target_threshold = c(0.6, 0.8, 1.0),
   stringsAsFactors = FALSE
)

# --- Stage 3: Regression Method Comparison ---
stage3_grid <- expand.grid(
   param_regression_method = c("rf", "gbr"),
   tail_distribution = c("lognormal"),
   stringsAsFactors = FALSE
)

# --- Run each stage sequentially ---
cat("Starting Stage 1: Distribution Screening...\n")
stage1_results <- run_all_experiments(stage1_grid, base_config, "distribution_screening")

stage1_metrics <- map_dfr(stage1_results, extract_metrics) # Get all reported metrics
stage1_data <- bind_all_hybrid_predictions(data, stage1_results) # Gather all time series outputs

dashboard <- create_simple_dashboard(stage1_data, stage1_metrics, threshold = 0.8)

cat("Starting Stage 2: Threshold Sensitivity...\n")
stage2_results <- run_all_experiments(stage2_grid, base_config, "threshold_sensitivity")

cat("Starting Stage 3: Regressor Comparison...\n")
stage3_results <- run_all_experiments(stage3_grid, base_config, "regressor_comparison")



