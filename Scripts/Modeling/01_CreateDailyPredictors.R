# =============================================================================
# Script Name:    01_CreateDailyPredictors.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Loads raw hourly data, tidies columns, and then creates a
#                 large number of variables that predict salinity. Categories
#                 include tide, wind, sustained discharge, flushing discharge.
#                 Data is written to .qs format. Also stacks the daily data into 
#                 a date-horizon format for the unified multi-horizon GAM.
# =============================================================================

source('Scripts/Utilities/LoadTextFiles.R')
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
# LOAD AND TIDY DATA
# =============================================================================

dir1 <- 'Data/Tidied/Processed/HourlyDataFinal.csv'
dir2 <- "Data/Raw/Text/SusquehannaBuoy/Meteo"

q_sal_data <- read.csv(dir1,
                       colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
q_sal_data <- q_sal_data %>%
   dplyr::select(-c(9, 10)) %>%
   mutate(DateTime = as_datetime(DateTime)) %>%
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00')) %>%
   mutate_if(is.character, as.factor)

meteo <- combine_txt_files(dir2)
meteo <- meteo %>%
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>%
   dplyr::select(-c(YY, MM, DD, hh, mm)) %>%
   relocate(DateTime) %>%
   mutate(across(
      where(is.numeric),
      ~ if_else(grepl("^9+\\.?9*$", as.character(.x)), NA_real_, .x)
   )) %>%
   dplyr::select(1:4) %>%
   mutate(Year  = year(DateTime),
          Month = month(DateTime),
          Day   = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   arrange(DateTime)

data <- merge(q_sal_data, meteo, by = c('DateTime', 'Year', 'Month', 'Day'), all.x = TRUE)
data <- data %>%
   filter(Year > 2006 & Year < 2025) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   rename(Gust = GST)

# Clean environment
rm(meteo, q_sal_data, dir1, dir2)

# =============================================================================
# AGGREGATE TO DAILY RESOLUTION
# Salinity:     daily maximum (captures intrusion peaks)
# Tide:         daily tidal range (max - min of fitted tide)
# Tide mean:    daily mean water level
# MaxDischarge: daily maximum (for pulse detection in flushing features)
# All others:   daily mean
# =============================================================================

data <- data %>%
   mutate(DateTime = as.Date(DateTime)) %>%
   group_by(DateTime) %>%
   summarise(
      Salinity     = max(Salinity,   na.rm = TRUE),
      TideRange    = max(Tide,       na.rm = TRUE) - min(Tide, na.rm = TRUE),
      TideMean     = mean(Tide,      na.rm = TRUE),
      MaxDischarge = max(Discharge,  na.rm = TRUE),
      across(
         where(is.numeric) & !all_of(c('Salinity', 'Tide', 'TideMean', 'MaxDischarge')),
         ~ mean(.x, na.rm = TRUE)
      ),
      .groups = 'drop') %>%
   mutate(
      Year      = as.numeric(format(DateTime, "%Y")),
      Month     = as.numeric(format(DateTime, "%m")),
      Day       = as.numeric(format(DateTime, "%d")),
      DayOfYear = as.numeric(format(DateTime, "%j"))
   ) %>%
   mutate(across(where(is.numeric), ~ round(.x, 3)))

# Set NaNs and Infs to NA
data[] <- lapply(data, function(x) { x[is.nan(x) | is.infinite(x)] <- NA; x })

# Save pre-predictor dataframe for sensitivity analysis in scripts 06 and 07
outputs    <- list(data)
file_names <- c('DailyRawData')
write_qs_files(outputs, 'Data/Tidied/Final/Daily', file_names)

# =============================================================================
# DERIVED PARAMETERS
# =============================================================================

# 90th percentile of max discharge in late summer/fall
FLUSH_THRESHOLD <- quantile(
   data$MaxDischarge[month(data$DateTime) %in% c(8, 9, 10, 11)],
   0.90, na.rm = TRUE
)

cat(sprintf("Flush Threshold: %.1f m3s (%.1f%% of intrusion-season days exceed)\n", 
            FLUSH_THRESHOLD, 
            100 * mean(data$MaxDischarge[month(data$DateTime) %in% c(8, 9, 10, 11)] > FLUSH_THRESHOLD, na.rm = TRUE)))

# Smoothed climatological discharge baseline
clim_discharge <- data %>%
   group_by(DayOfYear) %>%
   summarise(ClimDischarge = mean(Discharge, na.rm = TRUE), .groups = 'drop') %>%
   mutate(ClimDischarge = zoo::rollmean(ClimDischarge, 15, fill = 'extend', align = 'center'))

# =============================================================================
# MODEL DATA PREPARATION PIPELINE
# =============================================================================

model_data <- build_model_data(
   daily_raw        = data,
   clim_discharge   = clim_discharge,
   flush_threshold  = FLUSH_THRESHOLD,
   estuary_axis_deg = ESTUARY_AXIS_DEG
)

# Clean up baseline helper
rm(clim_discharge)

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