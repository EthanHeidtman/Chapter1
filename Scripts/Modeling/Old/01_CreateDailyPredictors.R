# =============================================================================
# Script Name:    01_CreateDailyPredictors.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Loads raw hourly data, tidies columns, and then creates a
#                 large number of variables that predict salinity. Categories
#                 include tide, wind, sustained discharge, flushing discharge.
#                 Continuous predictors are then normalized and data is written
#                 to .qs format as 'FinalModelData.qs'
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
# Susquehanna tidal reach runs roughly N-S: axis = 0
ESTUARY_AXIS_DEG <- 0

# Flushing threshold computed after data load
# 90th percentile of max discharge during the intrusion season (August - November)
FLUSH_THRESHOLD <- NULL  # set after data load

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

rm(meteo, q_sal_data, dir1, dir2)

# =============================================================================
# AGGREGATE TO DAILY RESOLUTION
# Salinity:    daily maximum (captures intrusion peaks)
# Tide:        daily tidal range (max - min of fitted tide)
# Tide mean:   daily mean water level
# MaxDischarge: daily maximum (for pulse detection in flushing features)
# All others:  daily mean
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

data[] <- lapply(data, function(x) { x[is.nan(x) | is.infinite(x)] <- NA; x })

# =============================================================================
# Derived Parameters
# =============================================================================

# Set threshold now that data is loaded
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
   
   LagSalinity = Salinity
   
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
   TideRange6  = zoo::rollmean(Tide, 6,  fill = NA, align = "right", na.rm = TRUE),
   TideRange7  = zoo::rollmean(Tide, 7,  fill = NA, align = "right", na.rm = TRUE),
   TideRange10 = zoo::rollmean(Tide, 10, fill = NA, align = "right", na.rm = TRUE),
   TideRange12 = zoo::rollmean(Tide, 12, fill = NA, align = "right", na.rm = TRUE),
   TideRange14 = zoo::rollmean(Tide, 14, fill = NA, align = "right", na.rm = TRUE),
   TideRange21 = zoo::rollmean(Tide, 21, fill = NA, align = "right", na.rm = TRUE),
   TideRange30 = zoo::rollmean(Tide, 30, fill = NA, align = "right", na.rm = TRUE),
   
   # Mean Water Level
   TideMean1  = zoo::rollmean(Tide, 1, fill = NA, align = 'right', na.rm = TRUE),
   TideMean2  = zoo::rollmean(Tide, 2, fill = NA, align = 'right', na.rm = TRUE),
   TideMean3  = zoo::rollmean(Tide, 3, fill = NA, align = 'right', na.rm = TRUE),
   TideMean4  = zoo::rollmean(Tide, 4, fill = NA, align = 'right', na.rm = TRUE),
   TideMean6  = zoo::rollmean(Tide, 6, fill = NA, align = 'right', na.rm = TRUE),
   TideMean7  = zoo::rollmean(Tide, 7, fill = NA, align = 'right', na.rm = TRUE),
   TideMean10 = zoo::rollmean(Tide, 10, fill = NA, align = 'right', na.rm = TRUE),
   TideMean12 = zoo::rollmean(Tide, 12, fill = NA, align = 'right', na.rm = TRUE),
   TideMean14 = zoo::rollmean(Tide, 14, fill = NA, align = 'right', na.rm = TRUE),
   TideMean21 = zoo::rollmean(Tide, 21, fill = NA, align = 'right', na.rm = TRUE),
   TideMean30 = zoo::rollmean(Tide, 30, fill = NA, align = 'right', na.rm = TRUE)
   
) %>%
   
# =======================================================================================
# PART 2: WIND PREDICTORS
# ESTUARY_AXIS_DEG: compass direction pointing UP-estuary (toward river head)
#   Susquehanna = 0 (north); Delaware ~10; Cape Fear ~315
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
      RollingWindAlong6  = zoo::rollmean(WindAlong, 6,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong7  = zoo::rollmean(WindAlong, 7,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong10 = zoo::rollmean(WindAlong, 10, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong12 = zoo::rollmean(WindAlong, 12, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong14 = zoo::rollmean(WindAlong, 14, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong21 = zoo::rollmean(WindAlong, 21, fill = NA, align = "right", na.rm = TRUE),
      RollingWindAlong30 = zoo::rollmean(WindAlong, 30, fill = NA, align = "right", na.rm = TRUE),
      
      # Cross-estuary rolling means
      RollingWindCross1  = zoo::rollmean(WindCross, 1,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross2  = zoo::rollmean(WindCross, 2,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross3  = zoo::rollmean(WindCross, 3,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross4  = zoo::rollmean(WindCross, 4,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross6  = zoo::rollmean(WindCross, 6,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross7  = zoo::rollmean(WindCross, 7,  fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross10 = zoo::rollmean(WindCross, 10, fill = NA, align = "right", na.rm = TRUE),
      RollingWindCross12 = zoo::rollmean(WindCross, 12, fill = NA, align = "right", na.rm = TRUE),
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
   
   # Rolling means of discharge anomaly
   RollingAnomaly1  = zoo::rollmean(DischargeAnomaly, 1,  fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly2  = zoo::rollmean(DischargeAnomaly, 2,  fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly3  = zoo::rollmean(DischargeAnomaly, 3,  fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly4  = zoo::rollmean(DischargeAnomaly, 4,  fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly6  = zoo::rollmean(DischargeAnomaly, 6,  fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly7  = zoo::rollmean(DischargeAnomaly, 7,  fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly10 = zoo::rollmean(DischargeAnomaly, 10, fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly12 = zoo::rollmean(DischargeAnomaly, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly14 = zoo::rollmean(DischargeAnomaly, 14, fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly21 = zoo::rollmean(DischargeAnomaly, 21, fill = NA, align = "right", na.rm = TRUE),
   RollingAnomaly30 = zoo::rollmean(DischargeAnomaly, 30, fill = NA, align = "right", na.rm = TRUE),
   
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
   
   
) %>%
   
# =======================================================================================
# PART 4: TEMPORAL FEATURES
# =======================================================================================

mutate(
   
   DaySin = sin(2 * pi * DayOfYear / 365.25),
   DayCos = cos(2 * pi * DayOfYear / 365.25),
   
)

# =============================================================================
# CLEAN UP
# =============================================================================

model_data[] <- lapply(model_data, function(x) { x[is.nan(x) | is.infinite(x)] <- NA; x })

model_data <- model_data %>%
   relocate(FERC, Salinity, Discharge, .after = DayOfYear) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   relocate(DaySin, DayCos, .after = DayOfYear) %>%
   relocate(Salinity, .after = DayOfYear) %>%
   relocate(FERC, .after = DayOfYear) %>%
   dplyr::select(-c(DaySin, DayCos, MaxDischarge, WindAlong, WindCross, TideMean, DischargeAnomaly, ClimDischarge))


# =============================================================================
# WRITE OUTPUT
# =============================================================================

# Save holdout data for validation later on (Script 8)
holdout_data <- model_data %>% 
   filter(Year >= 2022)

outputs <- list(holdout_data)
file_names <- c('HoldoutData2023_2024')
write_qs_files(outputs, 'Data/Tidied/Final/Daily', file_names)


# Write training data (2007-2022)
model_data <- model_data %>%
   filter(Year < 2023)

outputs    <- list(model_data)
file_names <- c('FinalModelData')
write_qs_files(outputs, 'Data/Tidied/Final/Daily', file_names)

rm(list = ls())
