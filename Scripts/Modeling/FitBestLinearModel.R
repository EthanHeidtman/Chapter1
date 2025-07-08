################################################################################
# Written by Ethan Heidtman, April 2025

# This script uses modeled tidal data, observed discharge data for the Conowingo 
# Dam, and observed/modeled salinity data at Havre de Grace to first develop a 
# predictive relationship for salinity near the Havre de Grace Drinking water
# intake. This script, the first step, designs a best linear model given a set of 
# pre-engineered predictor variables and interactions


############################ LOAD FUNCTIONS, PACKAGES, AND DATA ############################

# Source necessary functions
#func_env <- new.env()
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
salinity_threshold = 0.3 # measured in practical salt units (PSU), which is equivalent to parts per thousand

######################### SIMPLE LINEAR MODEL DEVELOPMENT ############################

# Define predictor categories and their candidates
predictor_config <- list(

   # Tide predictors (will always include the best one in subsequent models)
   # tide = c("Norm_Tide", "Norm_TideRate", 'Norm_LagTide1', 'Norm_LagTide2', 'Norm_LagTide4',
   #          'Norm_TideRange6', 'Norm_TideRange12', 'Norm_TideRange24', 'Norm_LowFlowTideRange',
   #          'Norm_WeightedTideRange12', 'ConsecFloodHours', 'TidePhase'),
   tide = c('IsFloodTide', 'IsEbbTide', 'IsSlackTide', 'Norm_Tide', 'Norm_LagTide1', 'Norm_LagTide2', 'Norm_LagTide4',
            'Norm_TideVelocity', 'Norm_TideAcceleration', 'Norm_TideRange6', 'Norm_TideRange12', 'Norm_TideRange24'),

   # Discharge predictors (test systematically)
   discharge_lag = c("Norm_PowLagDischarge1", "Norm_PowLagDischarge3", "Norm_PowLagDischarge6",
                     "Norm_PowLagDischarge10", "Norm_PowLagDischarge12", "Norm_PowLagDischarge24",
                     "Norm_PowLagDischarge36", "Norm_PowLagDischarge48", "Norm_PowLagDischarge72"),

   discharge_rolling = c("Norm_RollingPowDischarge0.5", "Norm_RollingPowDischarge1",
                         "Norm_RollingPowDischarge2", "Norm_RollingPowDischarge4",
                         "Norm_RollingPowDischarge7", "Norm_RollingPowDischarge10",
                         "Norm_RollingPowDischarge14"),

   # Inflow predictors
   inflow_lag = c("Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72",
                  'Norm_PowInflows', "Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72"),

   inflow_rolling = c("Norm_RollingPowInflows1", "Norm_RollingPowInflows2",
                      "Norm_RollingPowInflows7", "Norm_RollingPowInflows10"),

   # Stress indicators
   # stress_binary = c("IsModerateStress", "IsHighStress", "IsFlush", "IsStressed"),
   # stress_continuous = c("Norm_StressHours_7day_Marietta", "Norm_StressHours_14day_Marietta",
   #                       "Norm_StressHours_30day_Marietta", "Norm_StressHours_7day_Conowingo",
   #                       "Norm_StressHours_14day_Conowingo", "Norm_StressHours_30day_Conowingo",
   #                       "Norm_CumulativeStress_7day_Marietta", "Norm_CumulativeStress_14day_Marietta",
   #                       "Norm_CumulativeStress_30day_Marietta", "DaysSinceHighFlow"),
   stress_binary = c('IsLowInflow', 'IsVeryLowInflow', 'IsFlushingFlow'),
   stress_continuous = c('ConsecutiveLowInflowHours', 'ConsecutiveVeryLowInflowHours', 'LowInflowHours7', 
                         'LowInflowHours14', 'LowInflowHours30', 'HoursSinceFlush', 'DaysSinceFlush',
                         'StressFrequency7', 'StressFrequency14', 'StressFrequency30', 'Norm_CumulativeInflowDeficit3',
                         'Norm_CumulativeInflowDeficit7', 'Norm_CumulativeInflowDeficit30', 'Norm_MaxConsecutiveStress7',
                         'Norm_MaxConsecutiveStress14', 'Norm_MaxConsecutiveStress30'),

   # Seasonal/temporal
   temporal = c("Season", "DayOfYear")

)

# Define performance criteria with updated weights
# performance_criteria <- list(
#    weights = c(
#       # High salinity event metrics (65% of total weight)
#       "high_sal_detection" = 0.30,      # Detection capability 
#       "high_sal_accuracy" = 0.25,       # Accuracy of high salinity predictions
#       "high_sal_reliability" = 0.10,    # Reliability (false alarm control)
#       
#       # Overall model performance (30% of total weight)
#       "overall_performance" = 0.25,     # General model fit
#       "model_stability" = 0.05,         # Consistent performance across conditions
#       
#       # Model characteristics (5% of total weight)
#       "parsimony" = 0.05                # Model complexity penalty
#    )
# )
performance_criteria <- list(
   weights = c(
      # High salinity event metrics (70% of total weight)
      high_sal_detection = 0.25,     # Confusion matrix metrics (hit rate, etc.)
      high_sal_accuracy = 0.25,      # Error metrics (RMSE, MAE, bias) for high sal events
      high_sal_reliability = 0.20,   # False alarms and precision
      
      # Model characteristics (30% of total weight)
      overall_performance = 0.25,    # Overall error metrics
      complexity = 0.05              # Parsimony and complexity combined
   )
)

# Save model building output as a text file
#sink("Outputs/Experiments/LinearModeling/LinearModelBuilderLog.txt")
sink("Outputs/Experiments/LinearModeling/LinearPredictorSelectionLog.txt")

#linear_model_results <- linear_model_builder(model_data, salinity_threshold, predictor_config, performance_criteria)
linear_predictor_results <- linear_predictor_selector(model_data, salinity_threshold, predictor_config, performance_criteria)

# Stop redirecting output and return to console
sink()

# Strip stage results before writing (huge, take time to save and not really needed)
linear_predictor_results$stage_results <- NULL
environment(linear_predictor_results) <- new.env()

# Write output files
outputs <- list(linear_predictor_results)
#file_names <- c('LinearModelResults')
file_names <- c('LinearPredictors')
#write_qs_files(outputs, 'Outputs/Experiments/LinearModeling', file_names, preset = 'archive')
write_qs_files(outputs, 'Outputs/Experiments/LinearModeling', file_names, preset = 'archive')

# Clear environment
rm(list = ls())
