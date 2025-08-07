# =============================================================================
# Script Name:    FindBestLinearPredictors.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-06-01
# Last Updated:   2025-07-16
# Description:    Loads engineered hourly model data, systematically identifies
#                 the best predictor of salinity from each group using linear
#                 modeling. Then saves a smaller version of the model data and 
#                 the linear predictor results to a .json file for next steps 
#                 in Python.
# =============================================================================

# Source necessary functions
dirs <- c("Scripts/Functions/LinearModeling", "Scripts/Functions/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Load necessary packages
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))

# Define salinity threshold
salinity_threshold = 0.2 # measured in practical salt units (PSU), which is equivalent to parts per thousand

# Define predictor categories and their candidates
predictor_config <- list(

   # Tide predictors
   tide = c('IsFloodTide', 'IsEbbTide', 'IsSlackTide', 'Norm_Tide', 'Norm_LagTide1', 'Norm_LagTide2', 'Norm_LagTide4',
            'Norm_TideVelocity', 'Norm_TideAcceleration', 'Norm_TideRange6', 'Norm_TideRange12', 'Norm_TideRange24'),

   # Discharge predictors
   discharge_lag = c("Norm_PowLagDischarge1", "Norm_PowLagDischarge3", "Norm_PowLagDischarge6",
                     "Norm_PowLagDischarge10", "Norm_PowLagDischarge12", "Norm_PowLagDischarge24",
                     "Norm_PowLagDischarge36", "Norm_PowLagDischarge48", "Norm_PowLagDischarge72", 'Norm_PowLagDischarge96'),

   discharge_rolling = c("Norm_RollingPowDischarge0.5", "Norm_RollingPowDischarge1",
                         "Norm_RollingPowDischarge2", "Norm_RollingPowDischarge4",
                         "Norm_RollingPowDischarge7", "Norm_RollingPowDischarge10",
                         "Norm_RollingPowDischarge14"),

   # Inflow predictors
   inflow_lag = c("Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72",
                  'Norm_PowInflows', "Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72", 'Norm_PowLagInflows96'),

   inflow_rolling = c("Norm_RollingPowInflows1", "Norm_RollingPowInflows2",
                      "Norm_RollingPowInflows7", "Norm_RollingPowInflows10"),

   stress_binary = c('IsLowInflow', 'IsVeryLowInflow', 'IsFlushingFlow'),
   stress_continuous = c('ConsecutiveLowInflowHours', 'ConsecutiveVeryLowInflowHours', 'LowInflowHours7', 
                         'LowInflowHours14', 'LowInflowHours30', 'HoursSinceFlush', 'DaysSinceFlush',
                         'StressFrequency7', 'StressFrequency14', 'StressFrequency30', 'Norm_CumulativeInflowDeficit3',
                         'Norm_CumulativeInflowDeficit7', 'Norm_CumulativeInflowDeficit30', 'Norm_MaxConsecutiveStress7',
                         'Norm_MaxConsecutiveStress14', 'Norm_MaxConsecutiveStress30', 'SSI7', 'SSI14', 'SSI30'),

   # Seasonal/temporal
   temporal = c("Season", "DayOfYear")

)

performance_criteria <- list(
   weights = c(
      # High salinity event metrics (70% of total weight)
      #high_sal_detection = 0.25,     # Confusion matrix metrics (hit rate, etc.)
      high_sal_accuracy = 0.35,      # Error metrics (RMSE, MAE, bias) for high sal events
      high_sal_reliability = 0.35,   # Does the model detect events and can I trust the predictions?
      
      # Model characteristics (30% of total weight)
      overall_performance = 0.25,    # Overall error metrics
      complexity = 0.05              # Parsimony and complexity combined
   )
)

# Save model building output as a text file
sink("Outputs/Experiments/LinearModeling/LinearPredictorSelectionLog.txt")

linear_predictor_results <- linear_predictor_selector(model_data, salinity_threshold, predictor_config, performance_criteria)

# Stop redirecting output and return to console
sink()

# Strip stage results before writing (huge, take time to save and not really needed)
linear_predictor_results$stage_results <- NULL
environment(linear_predictor_results) <- new.env()

# Create minimal data object - only necessary columns to save space
required_cols <- unique(c('DateTime', 'Year', 'Month', 'Day', 'Salinity', linear_predictor_results$predictors$all_predictors))
required_cols <- required_cols[required_cols %in% names(model_data)]
clean_data <- model_data[, required_cols, drop = FALSE]

# Write linear predictor output file
outputs <- list(linear_predictor_results)
file_names <- c('LinearPredictors')
write_qs_files(outputs, 'Outputs/Experiments/LinearModeling', file_names, preset = 'archive', format = 'json')

# Write final cleaned model data output file
outputs <- list(clean_data)
file_names <- c('CleanFinalModelData')
write_qs_files(outputs, 'Data/Tidied/Final', file_names, preset = 'archive', format = 'csv')

# Clear environment
rm(list = ls())
