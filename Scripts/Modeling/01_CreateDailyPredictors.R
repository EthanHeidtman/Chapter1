# =============================================================================
# Script Name:    01_CreateDailyPredictors.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Loads raw hourly data, tidies columns, and then creates a
#                 large number of variables that predict salinity. Categories
#                 include tide, wind, sustained discharge, flushing discharge.
#                 Data is written to .qs2 format. Also stacks the daily data into 
#                 a date-horizon format for the unified multi-horizon GAM.
# =============================================================================

source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/ComputePredictors.R')

library(here)
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)

# =============================================================================
# SYSTEM-SPECIFIC PARAMETERS
# Change these when moving to a new system
# =============================================================================

# Estuary principal axis, degrees clockwise from north.
# Defines the along-estuary direction for wind decomposition.
# Chesapeake Bay runs roughly N-S: axis = 0
ESTUARY_AXIS_DEG <- 0

# Maximum forecast horizon (days)
H_MAX <- 20

# =============================================================================
# LOAD DATA
# =============================================================================

data <- read.csv('Data/Tidied/Hourly/HourlyDataFinal.csv') %>%
   mutate(DateTime = as_datetime(DateTime)) %>%
   filter(Year > 2006 & Year < 2025) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   mutate_if(is.character, as.factor)

# =============================================================================
# DECOMPOSE HOURLY WIND ALONG ESTUARY AXIS
# =============================================================================
# ESTUARY_AXIS_DEG: degrees clockwise from North (e.g., 0 = North-South)
data_hourly <- data %>%
   mutate(
      direction_rad = WDIR * pi / 180,
      axis_rad      = ESTUARY_AXIS_DEG * pi / 180,
      # Negative sign preserves oceanographic convention:
      # WindAlong > 0 = wind blowing UP the estuary (toward headwaters)
      # WindAlong < 0 = wind blowing DOWN the estuary (toward ocean)
      WindAlong     = -WSPD * cos(direction_rad - axis_rad),
      WindCross     = -WSPD * sin(direction_rad - axis_rad)
   )

# =============================================================================
# AGGREGATE TO DAILY RESOLUTION
# Salinity:     daily maximum (captures intrusion peaks)
# Tide:         daily tidal range (max - min of fitted tide)
# Tide mean:    daily mean water level
# MaxDischarge: daily maximum (for pulse detection in flushing features)
# All others:   daily mean
# =============================================================================
data_daily <- data_hourly %>%
   mutate(DateTime = as.Date(DateTime)) %>%
   group_by(DateTime) %>%
   summarise(
      Salinity     = max(Salinity,     na.rm = TRUE),                           # Daily max salinity
      TideRange    = max(Tide,         na.rm = TRUE) - min(Tide, na.rm = TRUE), # Daily tidal range
      TideMean     = mean(Tide,        na.rm = TRUE),                           # Daily mean water level
      MaxDischarge = max(Discharge,    na.rm = TRUE),                           # Daily peak discharge
      Discharge    = mean(Discharge,   na.rm = TRUE),                           # Daily mean discharge
      WindAlong    = mean(WindAlong,   na.rm = TRUE),                           # Daily net along-estuary wind
      WindCross    = mean(WindCross,   na.rm = TRUE),                           # Daily net cross-estuary wind
      .groups      = 'drop'
   ) %>%
   mutate(
      Year      = year(DateTime),
      Month     = month(DateTime),
      Day       = day(DateTime),
      DayOfYear = yday(DateTime),
      .after    = DateTime
   )


# Replace NaN / Inf with NA and round numeric values
data_daily[] <- lapply(data_daily, function(x) { x[is.nan(x) | is.infinite(x)] <- NA; x })
data_daily   <- data_daily %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

# Save pre-predictor dataframe for sensitivity analysis in scripts 06 and 07
write_qs_files(list(data_daily), 'Data/Tidied/Final/Daily', 'DailyRawData')
# =============================================================================
# DERIVED PARAMETERS
# =============================================================================

# 90th percentile of max discharge in late summer/fall
FLUSH_THRESHOLD <- quantile(
   data_daily$MaxDischarge[month(data_daily$DateTime) %in% c(8, 9, 10, 11)],
   0.90, na.rm = TRUE
)

cat(sprintf("Flush Threshold: %.1f m3s (%.1f%% of intrusion-season days exceed)\n", 
            FLUSH_THRESHOLD, 
            100 * mean(data_daily$MaxDischarge[month(data_daily$DateTime) %in% c(8, 9, 10, 11)] > FLUSH_THRESHOLD, na.rm = TRUE)))

# =============================================================================
# MODEL DATA PREPARATION PIPELINE
# =============================================================================

model_data <- build_model_data(
   daily_raw        = data_daily,
   flush_threshold  = FLUSH_THRESHOLD,
   estuary_axis_deg = ESTUARY_AXIS_DEG
)

# =============================================================================
# STACKING FUNCTION
# For each issue date t and horizon h (1:H_MAX), generate one row where:
#   - all predictors are anchored to t (no leakage)
#   - Salinity_h is the observed salinity at t + h days
# Rows where t + h falls outside the available salinity record are dropped.
# =============================================================================

stack_horizons <- function(daily_data, h_max = H_MAX) {
   
   # Salinity lookup: maps each date to its observed salinity value
   salinity_lookup <- daily_data %>%
      dplyr::select(DateTime, Salinity) %>%
      rename(target_date = DateTime, Salinity_h = Salinity)
   
   # Drop Salinity from predictor columns (it becomes LagSalinity only)
   predictor_data <- daily_data %>%
      dplyr::select(-Salinity)
   
   # Expand each issue date across all horizons
   stacked <- purrr::map_dfr(1:h_max, function(h) {
      predictor_data %>%
         mutate(
            h           = h,
            target_date = DateTime + h
         ) %>%
         left_join(salinity_lookup, by = 'target_date') %>%
         dplyr::select(-target_date)
   }) %>%
      filter(!is.na(Salinity_h)) %>%
      arrange(DateTime, h) %>%
      relocate(h, Salinity_h, .after = DateTime)
   
   return(stacked)
}

# =============================================================================
# SPLIT, STACK, AND WRITE
# =============================================================================

# Daily (non-stacked) splits
daily_training <- model_data %>% filter(Year < 2023)
daily_holdout  <- model_data %>% filter(Year >= 2022) # Include 2022 just for predictors leaking into 2022

# Stacked splits
# Training stack: drop any rows where t+h reaches into holdout years
stacked_training <- stack_horizons(daily_training, h_max = H_MAX)
stacked_holdout  <- stack_horizons(daily_holdout,  h_max = H_MAX)

# Write all four outputs
outputs    <- list(daily_training, daily_holdout, stacked_training, stacked_holdout)
file_names <- c('DailyPredictors', 'DailyHoldout', 'StackedModelData', 'StackedHoldoutData')
write_qs_files(outputs, 'Data/Tidied/Final/Daily', file_names)

rm(list = ls())
