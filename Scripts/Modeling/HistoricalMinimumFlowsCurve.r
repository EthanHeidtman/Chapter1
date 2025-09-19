# =============================================================================
# Script Name:    LogisticRegression.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)
library(zoo)
library(viridis)
source('Scripts/Utilities/ExperimentHelpers.R')
source('Scripts/Utilities/SavePlots.R')
source('Scripts/Plots/ModelScreeningPlots.R')
source('Scripts/Plots/MultiPanelModelPlot.R')
source('Scripts/Plots/EvalLogisticPerformance.R')
source('Scripts/Plots/AllThresholdExceedancePlots.r')
source('Scripts/Plots/FacetLogisticMatrixPlot.r')
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

DATA_PATH = 'Outputs/Experiments/Phase2_LogisticRegression/all_results.rds'
OUTPUT_PATH = 'Outputs/Experiments/Phase2_LogisticRegression'
PLOT_PATH = 'Outputs/Plots/Phase2_LogisticRegression'
FERC_PATH = '/Data/Tidied/Processed/FERCFlowRequirement.csv'

# Read in all model outputs
lr <- readRDS(DATA_PATH)

# Gather the models for each of the 9 logistic model runs for predictors RollingPowInflows and PowDischarge
final_models <- lapply(
   Filter(function(run) grepl("RollingPowInflows_PowDischarge", run$folder_name, fixed = TRUE), lr),
   `[[`, "model"
)

# Gather the outputs 9 logistic model runs for predictors RollingPowInflows and PowDischarge
final_data <- Filter(
   function(run) grepl("RollingPowInflows_PowDischarge", run$folder_name, fixed = TRUE),
   lr
)

# Combine all data into one large dataframe (long format)
final_data <- map_dfr(final_data, function(run) {
   run$data %>%
      mutate(threshold = unname(run$threshold_value[1]))
})

final_data <- final_data %>%
   mutate(threshold = round(threshold, digits = 2)) # round thresholds to 2 digits

p1 <- all_threshold_exceedance_probs(final_data)
ggsave(paste0(PLOT_PATH, '/AllThresholdsAllYears.png'), p1, width = 14, height = 10, dpi = 600)


p2 <- all_threshold_window_plot(final_data)
ggsave(paste0(PLOT_PATH, '/AllThresholds2016.png'), p2, width = 14, height = 10, dpi = 600)


create_raw_daily_curves <- function(final_data, risk_tolerance = 0.10) {
   
   cat("Method 1: Raw daily minimums (no smoothing)\n")
   
   daily_minimums <- final_data %>%
      filter(!is.na(exceedance_probability) & !is.na(Discharge)) %>%
      group_by(threshold, DayOfYear) %>%
      summarise(
         min_safe_flow = if(sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         typical_safe_flow = if(sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) >= 3) {
            median(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
         n_total_obs = n(),
         .groups = "drop"
      ) %>%
      mutate(
         date = as.Date(paste("2024", DayOfYear), format = "%Y %j"),
         month = month(date, label = TRUE),
         method = "Raw Daily"
      )
   
   return(daily_minimums)
}

create_monthly_step_curves <- function(final_data, risk_tolerance = 0.10) {
   
   cat("Method 3: Monthly step function\n")
   
   monthly_minimums <- final_data %>%
      filter(!is.na(exceedance_probability) & !is.na(Discharge)) %>%
      mutate(month = month(Date, label = TRUE)) %>%
      group_by(threshold, month) %>%
      summarise(
         min_safe_flow = if(sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         typical_safe_flow = if(sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) >= 5) {
            median(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
         .groups = "drop"
      )
   
   # Expand to daily values (step function)
   daily_step <- expand_grid(
      threshold = unique(monthly_minimums$threshold),
      DayOfYear = 1:366
   ) %>%
      mutate(
         date = as.Date(paste("2024", DayOfYear), format = "%Y %j"),
         month = month(date, label = TRUE)
      ) %>%
      left_join(monthly_minimums, by = c("threshold", "month")) %>%
      mutate(method = "Monthly Steps")
   
   return(daily_step)
}

daily_curve <- create_raw_daily_curves(final_data, risk_tolerance = 0.10)
ggplot(daily_curve, aes(x = DayOfYear, y = min_safe_flow, color = factor(threshold))) + 
   geom_line(na.rm = TRUE) + 
   scale_y_continuous(trans = 'log10') + 
   labs(x = ' Day of Year',
        y = 'Minimum Safe Flow (cubic m/s)',
        title = 'Minimum Safe Flows by Salinity Exceedance Threshold') + 
   theme_bw()

monthly_curve <- create_monthly_step_curves(final_data, risk_tolerance = 0.1)
ggplot(monthly_curve, aes(x = DayOfYear, y = min_safe_flow, color = factor(threshold))) + 
   geom_line(na.rm = TRUE) + 
   scale_y_continuous(trans = 'log10') + 
   labs(x = ' Day of Year',
        y = 'Minimum Safe Flow (cubic m/s)',
        title = 'Minimum Safe Flows by Salinity Exceedance Threshold') + 
   theme_bw()



   
