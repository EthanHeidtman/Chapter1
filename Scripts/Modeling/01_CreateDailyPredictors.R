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

# Flushing threshold computed after data load
# 90th percentile of max discharge during the intrusion season (August - November)
FLUSH_THRESHOLD <- NULL  # set after data load

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
      Tide         = max(Tide,       na.rm = TRUE) - min(Tide, na.rm = TRUE),
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
# Derived Parameters
# =============================================================================

# Set flushing threshold now that data is loaded: 90th percentile of max discharge in late summer/fall
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

# Compute Discharge Anomaly
data <- data %>%
   left_join(clim_discharge, by = 'DayOfYear') %>%
   mutate(DischargeAnomaly = Discharge / ClimDischarge)

rm(clim_discharge)

# =============================================================================
# MODEL DATA PREPARATION PIPELINE
# =============================================================================

model_data <- data %>%
   
   arrange(DateTime) %>%
   
# =======================================================================================
# PART 0: SALINITY PREDICTORS
# =======================================================================================

mutate(
   
   LagSalinity = lag(Salinity, 1)
   
) %>%
   
# =======================================================================================
# PART 1: TIDE PREDICTORS
# TideRange: rolling mean of daily tidal range, captures spring-neap cycle and surge
# TideMean: rolling mean of daily mean water level, captures subtidal variations
# =======================================================================================

mutate(
   
   # Tidal range
   TideRange1  = zoo::rollmean(Tide, 1,  fill = NA, align = "right", na.rm = TRUE),
   TideRange2  = zoo::rollmean(Tide, 2,  fill = NA, align = "right", na.rm = TRUE),
   TideRange3  = zoo::rollmean(Tide, 3,  fill = NA, align = "right", na.rm = TRUE),
   TideRange4  = zoo::rollmean(Tide, 4,  fill = NA, align = "right", na.rm = TRUE),
   TideRange5  = zoo::rollmean(Tide, 5,  fill = NA, align = "right", na.rm = TRUE),
   TideRange6  = zoo::rollmean(Tide, 6,  fill = NA, align = "right", na.rm = TRUE),
   TideRange7  = zoo::rollmean(Tide, 7,  fill = NA, align = "right", na.rm = TRUE),
   TideRange8  = zoo::rollmean(Tide, 8,  fill = NA, align = "right", na.rm = TRUE),
   TideRange9  = zoo::rollmean(Tide, 9,  fill = NA, align = "right", na.rm = TRUE),
   TideRange10 = zoo::rollmean(Tide, 10, fill = NA, align = "right", na.rm = TRUE),
   TideRange11  = zoo::rollmean(Tide, 11,  fill = NA, align = "right", na.rm = TRUE),
   TideRange12 = zoo::rollmean(Tide, 12, fill = NA, align = "right", na.rm = TRUE),
   TideRange13  = zoo::rollmean(Tide, 13,  fill = NA, align = "right", na.rm = TRUE),
   TideRange14 = zoo::rollmean(Tide, 14, fill = NA, align = "right", na.rm = TRUE),
   TideRange21 = zoo::rollmean(Tide, 21, fill = NA, align = "right", na.rm = TRUE),
   TideRange30 = zoo::rollmean(Tide, 30, fill = NA, align = "right", na.rm = TRUE),
   TideRange35 = zoo::rollmean(Tide, 35, fill = NA, align = "right", na.rm = TRUE),
   TideRange40 = zoo::rollmean(Tide, 40, fill = NA, align = "right", na.rm = TRUE),
   TideRange50 = zoo::rollmean(Tide, 50, fill = NA, align = "right", na.rm = TRUE),
   TideRange60 = zoo::rollmean(Tide, 60, fill = NA, align = "right", na.rm = TRUE),
   
   # Mean Water Level
   TideMean1  = zoo::rollmean(TideMean, 1,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean2  = zoo::rollmean(TideMean, 2,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean3  = zoo::rollmean(TideMean, 3,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean4  = zoo::rollmean(TideMean, 4,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean5  = zoo::rollmean(TideMean, 5,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean6  = zoo::rollmean(TideMean, 6,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean7  = zoo::rollmean(TideMean, 7,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean8  = zoo::rollmean(TideMean, 8,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean9  = zoo::rollmean(TideMean, 9,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean10 = zoo::rollmean(TideMean, 10, fill = NA, align = 'right', na.rm = TRUE),
   TideMean11  = zoo::rollmean(TideMean, 11,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean12 = zoo::rollmean(TideMean, 12, fill = NA, align = 'right', na.rm = TRUE),
   TideMean13  = zoo::rollmean(TideMean, 13,  fill = NA, align = 'right', na.rm = TRUE),
   TideMean14 = zoo::rollmean(TideMean, 14, fill = NA, align = 'right', na.rm = TRUE),
   TideMean21 = zoo::rollmean(TideMean, 21, fill = NA, align = 'right', na.rm = TRUE),
   TideMean30 = zoo::rollmean(TideMean, 30, fill = NA, align = 'right', na.rm = TRUE),
   TideMean35 = zoo::rollmean(TideMean, 30, fill = NA, align = 'right', na.rm = TRUE),
   TideMean40 = zoo::rollmean(TideMean, 30, fill = NA, align = 'right', na.rm = TRUE),
   TideMean50 = zoo::rollmean(TideMean, 30, fill = NA, align = 'right', na.rm = TRUE),
   TideMean60 = zoo::rollmean(TideMean, 30, fill = NA, align = 'right', na.rm = TRUE)
   
) %>%
   
# =======================================================================================
# PART 2: WIND PREDICTORS
# ESTUARY_AXIS_DEG: compass direction pointing UP-estuary (toward river head)
#   Susquehanna = 0 (north); 
#
# WindAlong: along-estuary component
#   positive = wind blowing toward river head   (UpEstuary)
#   negative = wind blowing toward ocean        (DownEstuary)
#
# WindCross: cross-estuary component
#   positive = wind toward right bank facing up-estuary  (RightBank)
#              westerly / seaward for Susquehanna
#   negative = wind toward left bank facing up-estuary   (LeftBank)
#              easterly / landward for Susquehanna
# =======================================================================================

mutate(
   
   direction_radians = WDIR * pi / 180,
   axis_rad          = ESTUARY_AXIS_DEG * pi / 180,
   
   WindAlong = -WSPD * cos(direction_radians - axis_rad),
   WindCross = -WSPD * sin(direction_radians - axis_rad)
   
) %>%
   select(-c(direction_radians, axis_rad, WDIR, WSPD)) %>%
   
   mutate(
      
      # Along-estuary rolling means
      RollingWindAlong1  = zoo::rollmean(WindAlong, 1,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong2  = zoo::rollmean(WindAlong, 2,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong3  = zoo::rollmean(WindAlong, 3,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong4  = zoo::rollmean(WindAlong, 4,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong5  = zoo::rollmean(WindAlong, 5,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong6  = zoo::rollmean(WindAlong, 6,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong7  = zoo::rollmean(WindAlong, 7,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong8  = zoo::rollmean(WindAlong, 8,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong9  = zoo::rollmean(WindAlong, 9,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong10 = zoo::rollmean(WindAlong, 10, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong11  = zoo::rollmean(WindAlong, 11,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong12 = zoo::rollmean(WindAlong, 12, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong13  = zoo::rollmean(WindAlong, 13,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong14 = zoo::rollmean(WindAlong, 14, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong21 = zoo::rollmean(WindAlong, 21, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong30 = zoo::rollmean(WindAlong, 30, fill = NA, align = "right", na.rm = TRUE),
      
      # Cross-estuary rolling means
      RollingWindCross1  = zoo::rollmean(WindCross, 1,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross2  = zoo::rollmean(WindCross, 2,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross3  = zoo::rollmean(WindCross, 3,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross4  = zoo::rollmean(WindCross, 4,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross5  = zoo::rollmean(WindCross, 5,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross6  = zoo::rollmean(WindCross, 6,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross7  = zoo::rollmean(WindCross, 7,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross8  = zoo::rollmean(WindCross, 8,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross9  = zoo::rollmean(WindCross, 9,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross10 = zoo::rollmean(WindCross, 10, fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross11  = zoo::rollmean(WindCross, 11,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross12 = zoo::rollmean(WindCross, 12, fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross13  = zoo::rollmean(WindCross, 13,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross14 = zoo::rollmean(WindCross, 14, fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross21 = zoo::rollmean(WindCross, 21, fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross30 = zoo::rollmean(WindCross, 30, fill = NA, align = "right", na.rm = TRUE),
      
   ) %>%
   
# =======================================================================================
# PART 3: DISCHARGE PREDICTORS
#
# Sustained Flow: rolling means of raw discharge & rolling means of discharge anomaly
# Flushing Flow: rolling maxima of discharge & flux above FLUSH_THRESHOLD
# =======================================================================================

mutate(
   
   # Rolling means of raw discharge
   RollingDischarge1  = zoo::rollmean(Discharge, 1,  fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge2  = zoo::rollmean(Discharge, 2,  fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge3  = zoo::rollmean(Discharge, 3,  fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge4  = zoo::rollmean(Discharge, 4,  fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge6  = zoo::rollmean(Discharge, 6,  fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge7  = zoo::rollmean(Discharge, 7,  fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge10 = zoo::rollmean(Discharge, 10, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge12 = zoo::rollmean(Discharge, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge14 = zoo::rollmean(Discharge, 14, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge21 = zoo::rollmean(Discharge, 21, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge30 = zoo::rollmean(Discharge, 30, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge35 = zoo::rollmean(Discharge, 35, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge40 = zoo::rollmean(Discharge, 40, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge50 = zoo::rollmean(Discharge, 50, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge60 = zoo::rollmean(Discharge, 60, fill = NA, align = "right", na.rm = TRUE),
   
   
   # # Rolling means of discharge anomaly
   # RollingAnomaly1  = zoo::rollmean(DischargeAnomaly, 1,  fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly2  = zoo::rollmean(DischargeAnomaly, 2,  fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly3  = zoo::rollmean(DischargeAnomaly, 3,  fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly4  = zoo::rollmean(DischargeAnomaly, 4,  fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly6  = zoo::rollmean(DischargeAnomaly, 6,  fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly7  = zoo::rollmean(DischargeAnomaly, 7,  fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly10 = zoo::rollmean(DischargeAnomaly, 10, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly12 = zoo::rollmean(DischargeAnomaly, 12, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly14 = zoo::rollmean(DischargeAnomaly, 14, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly21 = zoo::rollmean(DischargeAnomaly, 21, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly30 = zoo::rollmean(DischargeAnomaly, 30, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly35 = zoo::rollmean(DischargeAnomaly, 35, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly40 = zoo::rollmean(DischargeAnomaly, 40, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly50 = zoo::rollmean(DischargeAnomaly, 50, fill = NA, align = "right", na.rm = TRUE),
   # RollingAnomaly60 = zoo::rollmean(DischargeAnomaly, 60, fill = NA, align = "right", na.rm = TRUE),
   
   # Flushing discharge: rolling maxima
   MaxDischarge1  = zoo::rollmax(MaxDischarge, 1,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge2  = zoo::rollmax(MaxDischarge, 2,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge3  = zoo::rollmax(MaxDischarge, 3,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge4  = zoo::rollmax(MaxDischarge, 4,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge5  = zoo::rollmax(MaxDischarge, 5,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge6  = zoo::rollmax(MaxDischarge, 6,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge7  = zoo::rollmax(MaxDischarge, 7,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge8  = zoo::rollmax(MaxDischarge, 8,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge9  = zoo::rollmax(MaxDischarge, 9,  fill = NA, align = "right", na.rm = TRUE),
   MaxDischarge10 = zoo::rollmax(MaxDischarge, 10, fill = NA, align = "right", na.rm = TRUE),
   
   # Flushing discharge: exceedance flux over threshold
   ExceedFlux1  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 1,  fill = NA, align = "right"),
   ExceedFlux2  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 2,  fill = NA, align = "right"),
   ExceedFlux3  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 3,  fill = NA, align = "right"),
   ExceedFlux4  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 4,  fill = NA, align = "right"),
   ExceedFlux5  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 5,  fill = NA, align = "right"),
   ExceedFlux6  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 6,  fill = NA, align = "right"),
   ExceedFlux7  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 7,  fill = NA, align = "right"),
   ExceedFlux8  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 8,  fill = NA, align = "right"),
   ExceedFlux9  = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 9,  fill = NA, align = "right"),
   ExceedFlux10 = zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 10, fill = NA, align = "right"),
   
)

# =============================================================================
# CLEAN UP
# =============================================================================

# Set NaN and Inf to NA
model_data[] <- lapply(model_data, function(x) { x[is.nan(x) | is.infinite(x)] <- NA; x })

model_data <- model_data %>%
   relocate(FERC, Salinity, Discharge, .after = DayOfYear) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   relocate(Salinity, .after = DayOfYear) %>%
   relocate(FERC, .after = DayOfYear) %>%
   dplyr::select(-c(MaxDischarge, WindAlong, WindCross, TideMean, DischargeAnomaly, ClimDischarge))

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

# cat(sprintf("Daily training rows:   %d\n", nrow(daily_training)))
# cat(sprintf("Daily holdout rows:    %d\n", nrow(daily_holdout)))
# cat(sprintf("Stacked training rows: %d\n", nrow(stacked_training)))
# cat(sprintf("Stacked holdout rows:  %d\n", nrow(stacked_holdout)))

# Write all four outputs
outputs    <- list(daily_training, daily_holdout, stacked_training, stacked_holdout)
file_names <- c('DailyPredictors', 'DailyHoldout', 'StackedModelData', 'StackedHoldoutData')
write_qs_files(outputs, 'Data/Tidied/Final/Daily', file_names)

rm(list = ls())
