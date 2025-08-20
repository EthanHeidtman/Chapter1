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
library(ggplot2)
library(viridis)
library(gridExtra)
library(scales)
library(patchwork)
library(ggh4x)
source('Scripts/Functions/Modeling/ExperimentHelpers.R')

DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
FERC_PATH = 'Data/Tidied/Processed/FERCFlowRequirement.csv'
OUTPUT_PATH = 'Outputs/Experiments/CovarianceModeling'

# Read in final cleaned model data
data <- read.csv(DATA_PATH)
data <- data %>%
   mutate(
      DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   arrange(DateTime) %>%
   distinct(DateTime, .keep_all = TRUE) %>%
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# Read in FERC minimum flow requirement 
ferc <- read.csv(FERC_PATH)
ferc <- ferc %>%
   dplyr::select(-1) %>%
   rename(DayOfYear = Day)

# Get distribution screening results
dist_results <- load_covariance_results('DistributionScreening')
dist_predictions <- unnest_covariance_results(dist_results, experiment_col = 'experiment_name')
dist_predictions <- dist_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>% 
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
   drop_na(exceedance_probability)

dist_data <- left_join(dist_predictions, data, by = "DateTime")
dist_data <- dist_data %>%
   relocate(Year, Month, Day, Salinity, .after = DateTime) %>%
   relocate(33, 34, 35, .after = Salinity) %>%
   filter(distribution_family == 'gpd')

# Get threshold screening results
threshold_results <- load_covariance_results('ThresholdScreening')
threshold_predictions <- unnest_covariance_results(threshold_results, experiment_col = 'experiment_name')
threshold_predictions <- threshold_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>% 
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
   drop_na(exceedance_probability)

threshold_data <- left_join(threshold_predictions, data, by = 'DateTime')
threshold_data <- threshold_data %>%
   relocate(Year, Month, Day, Salinity, .after = DateTime) %>%
   relocate(29, 30, 31, .after = Salinity)

# Get window size screening results
window_results <- load_covariance_results('WindowSizeScreening')
window_predictions <- unnest_covariance_results(window_results, experiment_col = 'experiment_name')
window_predictions <- window_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>% 
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
   drop_na(exceedance_probability)

window_data <- left_join(window_predictions, data, by = 'DateTime')
window_data <- window_data %>%
   relocate(Year, Month, Day, Salinity, .after = DateTime) %>%
   relocate(29, 30, 31, .after = Salinity)


create_static_flow_schedule <- function(dist_data, 
                                        current_ferc_schedule,
                                        target_exceedance_prob = 0.05) {
   
   # Get the range of days with salinity data
   salinity_days <- dist_data %>%
      summarise(
         min_doy = min(DayOfYear, na.rm = TRUE),
         max_doy = max(DayOfYear, na.rm = TRUE)
      )
   
   cat("Salinity data available for days", salinity_days$min_doy, "to", salinity_days$max_doy, "\n")
   
   # Calculate typical exceedance probability for days with data
   daily_risk <- dist_data %>%
      group_by(DayOfYear) %>%
      summarise(
         median_exceedance_prob = median(exceedance_probability, na.rm = TRUE),
         q75_exceedance_prob = quantile(exceedance_probability, 0.75, na.rm = TRUE),
         q95_exceedance_prob = quantile(exceedance_probability, 0.95, na.rm = TRUE),
         n_observations = n(),
         .groups = 'drop'
      ) %>%
      mutate(
         # How much does typical risk exceed target?
         risk_excess_median = pmax(0, median_exceedance_prob - target_exceedance_prob),
         risk_excess_conservative = pmax(0, q75_exceedance_prob - target_exceedance_prob),
         
         # Flow adjustment needed (as multiplier of current FERC requirement)
         flow_multiplier_median = 1 + (risk_excess_median / target_exceedance_prob) * 0.5,
         flow_multiplier_conservative = 1 + (risk_excess_conservative / target_exceedance_prob) * 0.5,
         
         # New flow requirements
         current_ferc_flow = current_ferc_schedule[DayOfYear],
         new_flow_median = current_ferc_flow * flow_multiplier_median,
         new_flow_conservative = current_ferc_flow * flow_multiplier_conservative,
         
         # Additional flow beyond current FERC
         additional_flow_median = new_flow_median - current_ferc_flow,
         additional_flow_conservative = new_flow_conservative - current_ferc_flow
      )
   
   # Create complete 365-day schedule
   complete_schedule <- data.frame(DayOfYear = 1:365) %>%
      left_join(daily_risk, by = "DayOfYear") %>%
      mutate(
         # For days without salinity data, use original FERC
         has_salinity_data = !is.na(median_exceedance_prob),
         final_flow_median = ifelse(has_salinity_data, new_flow_median, current_ferc_schedule[DayOfYear]),
         final_flow_conservative = ifelse(has_salinity_data, new_flow_conservative, current_ferc_schedule[DayOfYear]),
         final_additional_flow_median = ifelse(has_salinity_data, additional_flow_median, 0),
         final_additional_flow_conservative = ifelse(has_salinity_data, additional_flow_conservative, 0),
         data_source = ifelse(has_salinity_data, "MODEL_BASED", "ORIGINAL_FERC")
      )
   
   return(complete_schedule)
}

static_update <- create_static_flow_schedule(dist_data = dist_data %>% filter(distribution_family == 'gpd'), 
                                             current_ferc_schedule = ferc$Discharge)

ggplot(static_update, aes(x = DayOfYear)) + 
   geom_line(aes(y = current_ferc_flow), na.rm = TRUE, color = 'black') + 
   geom_line(aes(y = final_flow_conservative), na.rm = TRUE, color = 'blue')




