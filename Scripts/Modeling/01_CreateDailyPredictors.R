# =============================================================================
# Script Name:    01_CreateDailyPredictors.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Loads raw hourly data, tidies columns, and then creates a 
#                 large number of variables that I think might predict salinity.
#                 Categories include tide, discharge, inflows, stress/drought. 
#                 Continuous predictors are then normalized and data is written
#                 to .qs format as 'FinalModelData.qs'
# =============================================================================

# Source necessary functions
source('Scripts/Utilities/LoadTextFiles.R')
source('Scripts/Utilities/WriteQS.R')

# Load necessary packages
library(here)        # For directory referencing
library(tidyverse)   # For data manipulation
library(dplyr)       # For data manipulation
library(zoo)         # For rolling computation
library(lubridate)   # For datetime related functions

# Directories where data are located
dir1 <- 'Data/Tidied/Processed/HourlyDataFinal.csv'
dir2 <- "Data/Raw/Text/SusquehannaBuoy/Meteo"

# Read in hourly discharge and salinity data
q_sal_data <- read.csv(dir1, 
                       colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
q_sal_data <- q_sal_data %>%
   dplyr::select(-c(9, 10)) %>%                              # Remove extra columns
   mutate(DateTime = as_datetime(DateTime)) %>%              # Make dates class datetime
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00')) %>% # Keep only dates before 
   mutate_if(is.character, as.factor)

# Read in meteorology data, including wind
meteo <- combine_txt_files(dir2)
meteo <- meteo %>%
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>% # Make a datetime column
   dplyr::select(-c(YY, MM, DD, hh, mm)) %>%
   relocate(DateTime) %>%
   mutate(across(
      where(is.numeric),
      ~ if_else(grepl("^9+\\.?9*$", as.character(.x)), NA_real_, .x)
   )) %>%
   dplyr::select(1 : 4) %>%
   mutate(Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   arrange(DateTime)

# Merge all data into 1 dataset
data <- merge(q_sal_data, meteo, by = c('DateTime', 'Year', 'Month', 'Day'), all.x = TRUE)
data <- data %>%
   filter(Year > 2006 & Year < 2025) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   rename(Gust = GST)

# Remove extra objects
rm(meteo, q_sal_data, dir1, dir2)

# Aggregate to daily resolution
data <- data %>%
   mutate(DateTime = as.Date(DateTime)) %>%   
   group_by(DateTime, Year, Month, Day) %>%
   summarise(
      Salinity = max(Salinity, na.rm = TRUE),
      Tide = max(Tide, na.rm = TRUE) - min(Tide, na.rm = TRUE),
      MaxDischarge = max(Discharge, na.rm = TRUE),
      Discharge = mean(Discharge, na.rm = TRUE),
      
      across(
         where(is.numeric) & !all_of(c("Salinity", "Tide", 'MaxDischarge')),
         ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
   ) %>%
   mutate(across(where(is.numeric), ~ round(.x, 2)))

# Make NaN NA
data[] <- lapply(data, function(x) {
   x[is.nan(x) | is.infinite(x)] <- NA
   x
})

FLUSH_THRESHOLD <- 600 # cubic m/s, based on October 2016 event and other similar flushing events

####################### MODEL DATA PREPARATION PIPELINE ##########################

# Create the model data
model_data <- data %>%
   
   arrange(DateTime) %>%
   
# =======================================================================================
# PART 0: SALINITY PREDICTORS
# ======================================================================================= 

mutate(
   
   # Lagged Salinity
   LagSalinity = Salinity
   
) %>%
   
# =======================================================================================
# PART 1: TIDE PREDICTORS
# =======================================================================================

mutate(
   
   # Lagged Tide Features
   LagTide1 = lag(Tide, 1),
   LagTide2 = lag(Tide, 2),
   LagTide3 = lag(Tide, 3),
   LagTide4 = lag(Tide, 4),
   LagTide6 = lag(Tide, 6),
   LagTide7 = lag(Tide, 7),
   LagTide10 = lag(Tide, 10),
   LagTide12 = lag(Tide, 12),
   LagTide14 = lag(Tide, 14),
   LagTide21 = lag(Tide, 21),
   LagTide30 = lag(Tide, 30),
   
   # Tidal Range Metrics: Mean Tidal range over X # of days
   TideRange1 = zoo::rollmean(Tide, 1, fill = NA, align = "right", na.rm = TRUE),
   TideRange2 = zoo::rollmean(Tide, 2, fill = NA, align = "right", na.rm = TRUE),
   TideRange3 = zoo::rollmean(Tide, 3, fill = NA, align = "right", na.rm = TRUE),
   TideRange4 = zoo::rollmean(Tide, 4, fill = NA, align = "right", na.rm = TRUE),
   TideRange6 = zoo::rollmean(Tide, 6, fill = NA, align = "right", na.rm = TRUE),
   TideRange7 = zoo::rollmean(Tide, 7, fill = NA, align = "right", na.rm = TRUE),
   TideRange10 = zoo::rollmean(Tide, 10, fill = NA, align = "right", na.rm = TRUE),
   TideRange12 = zoo::rollmean(Tide, 12, fill = NA, align = "right", na.rm = TRUE),
   TideRange14 = zoo::rollmean(Tide, 14, fill = NA, align = "right", na.rm = TRUE),
   TideRange21 = zoo::rollmean(Tide, 21, fill = NA, align = "right", na.rm = TRUE),
   TideRange30 = zoo::rollmean(Tide, 30, fill = NA, align = "right", na.rm = TRUE),
   
) %>%

# =======================================================================================
# PART 2: WIND PREDICTORS
# =======================================================================================

mutate(
   
   # U (east-west) and V (north-south) wind magnitudes
   direction_radians = WDIR * pi / 180,
   U = -WSPD * sin(direction_radians), # east-west, cross estuary: (+) = wind toward the east, (-) = wind toward the west
   V = -WSPD * cos(direction_radians), # north-south, along estuary: (+) = wind toward the north, (-) = wind toward the south
   
   # Lagged Wind Predictors 
   LagU1 = lag(U, 1),
   LagU2 = lag(U, 2),
   LagU3 = lag(U, 3),
   LagU4 = lag(U, 4),
   LagU6 = lag(U, 6),
   LagU7 = lag(U, 7),
   LagU10 = lag(U, 10),
   LagU12 = lag(U, 12),
   LagU14 = lag(U, 14),
   LagU24 = lag(U, 21),
   LagU30 = lag(U, 30),
   
   LagV1 = lag(V, 1),
   LagV2 = lag(V, 2),
   LagV3 = lag(V, 3),
   LagV4 = lag(V, 4),
   LagV6 = lag(V, 6),
   LagV7 = lag(V, 7),
   LagV10 = lag(V, 10),
   LagV12 = lag(V, 12),
   LagV14 = lag(V, 14),
   LagV24 = lag(V, 21),
   LagV30 = lag(V, 30),
   
   # Rolling Wind Predictors
   RollingU1 = zoo::rollmean(U, 1, fill = NA, align = "right", na.rm = TRUE),
   RollingU2 = zoo::rollmean(U, 2, fill = NA, align = "right", na.rm = TRUE),
   RollingU3 = zoo::rollmean(U, 3, fill = NA, align = "right", na.rm = TRUE),
   RollingU4 = zoo::rollmean(U, 4, fill = NA, align = "right", na.rm = TRUE),
   RollingU6 = zoo::rollmean(U, 6, fill = NA, align = "right", na.rm = TRUE),
   RollingU7 = zoo::rollmean(U, 7, fill = NA, align = "right", na.rm = TRUE),
   RollingU10 = zoo::rollmean(U, 10, fill = NA, align = "right", na.rm = TRUE),
   RollingU12 = zoo::rollmean(U, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingU14 = zoo::rollmean(U, 14, fill = NA, align = "right", na.rm = TRUE),
   RollingU21 = zoo::rollmean(U, 21, fill = NA, align = "right", na.rm = TRUE),
   RollingU30 = zoo::rollmean(U, 30, fill = NA, align = "right", na.rm = TRUE),

   RollingV1 = zoo::rollmean(V, 1, fill = NA, align = "right", na.rm = TRUE),
   RollingV2 = zoo::rollmean(V, 2, fill = NA, align = "right", na.rm = TRUE),
   RollingV3 = zoo::rollmean(V, 3, fill = NA, align = "right", na.rm = TRUE),
   RollingV4 = zoo::rollmean(V, 4, fill = NA, align = "right", na.rm = TRUE),
   RollingV6 = zoo::rollmean(V, 6, fill = NA, align = "right", na.rm = TRUE),
   RollingV7 = zoo::rollmean(V, 7, fill = NA, align = "right", na.rm = TRUE),
   RollingV10 = zoo::rollmean(V, 10, fill = NA, align = "right", na.rm = TRUE),
   RollingV12 = zoo::rollmean(V, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingV14 = zoo::rollmean(V, 14, fill = NA, align = "right", na.rm = TRUE),
   RollingV21 = zoo::rollmean(V, 21, fill = NA, align = "right", na.rm = TRUE),
   RollingV30 = zoo::rollmean(V, 30, fill = NA, align = "right", na.rm = TRUE),
   
) %>%
   select(-c(direction_radians, WDIR, WSPD)) %>%
   
# =======================================================================================
# PART 3: BASIC DISCHARGE FEATURES
# =======================================================================================

mutate(
   
   # Lagged Conowingo Discharges
   LagDischarge1 = lag(Discharge, 1),
   LagDischarge2 = lag(Discharge, 2),
   LagDischarge3 = lag(Discharge, 3),
   LagDischarge4 = lag(Discharge, 4),
   LagDischarge6 = lag(Discharge, 6),
   LagDischarge7 = lag(Discharge, 7),
   LagDischarge10 = lag(Discharge, 10),
   LagDischarge12 = lag(Discharge, 12),
   LagDischarge14 = lag(Discharge, 14),
   LagDischarge21 = lag(Discharge, 21),
   LagDischarge30 = lag(Discharge, 30),

   # Rolling Discharge
   RollingDischarge1 = zoo::rollmean(Discharge, 1, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge2 = zoo::rollmean(Discharge, 2, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge3 = zoo::rollmean(Discharge, 3, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge4 = zoo::rollmean(Discharge, 4, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge6 = zoo::rollmean(Discharge, 6, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge7 = zoo::rollmean(Discharge, 7, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge10 = zoo::rollmean(Discharge, 10, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge12 = zoo::rollmean(Discharge, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge14 = zoo::rollmean(Discharge, 14, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge21 = zoo::rollmean(Discharge, 21, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge30 = zoo::rollmean(Discharge, 30, fill = NA, align = "right", na.rm = TRUE),
   
   # 1. Cumulative flushing-range discharge over recent window
   # How much discharge above the threshold has occurred?
   # Zero during normal conditions, grows only during genuine flush-range events
   # Use 7-day window — long enough to capture sustained flush, 
   # short enough not to dilute a genuine event
   ExceedFlux7 = pmin(
      zoo::rollsum(pmax(0, MaxDischarge - FLUSH_THRESHOLD), 7, 
                   fill=NA, align="right"),
      40000
   ),
   
   # 2. Days since last exceedance of threshold
   # Captures recency — a flush 3 days ago is more relevant than one 12 days ago
   # This is the variable your PeakDrop was trying to approximate
   DaysSinceFlush = {
      exceed <- MaxDischarge >= FLUSH_THRESHOLD
      result <- rep(NA_real_, length(exceed))
      last <- NA_real_
      for (i in seq_along(exceed)) {
         if (!is.na(exceed[i]) && exceed[i]) last <- i
         result[i] <- if (is.na(last)) NA_real_ else i - last
      }
      result
   },
   
   # 3. Whether discharge is currently in flushing range (binary context)
   # Gives the GAM a clean regime indicator to interact with
   # Encode as 0/1 so the GAM smooth can fit different slopes above/below
   InFlushRegime = as.numeric(RollingDischarge7 >= FLUSH_THRESHOLD)
   
   # Max Discharge
   # MaxDischarge1 = zoo::rollapply(MaxDischarge, 1, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge2 = zoo::rollapply(MaxDischarge, 2, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge3 = zoo::rollapply(MaxDischarge, 3, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge4 = zoo::rollapply(MaxDischarge, 4, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge5 = zoo::rollapply(MaxDischarge, 5, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge7 = zoo::rollapply(MaxDischarge, 7, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge10 = zoo::rollapply(MaxDischarge, 10, max, fill = NA, align = 'right', na.rm = TRUE),
   # MaxDischarge14 = zoo::rollapply(MaxDischarge, 14, max, fill = NA, align = 'right', na.rm = TRUE),
   # 
   # # Pulsed discharges
   # Ramp1 = Discharge - lag(Discharge, 1), # Rate of change
   # 
   # # Discharge context
   # RelAnomaly7 = Discharge / RollingDischarge7, # Change detection
   # RelAnomaly14 = Discharge / RollingDischarge14, 
   # PulseContrast = MaxDischarge3 / RollingDischarge14,
   # 
   # # How big is the pulse
   # PulseVolume3 = zoo::rollsum(pmax(0, Ramp1), 3, fill = NA, align = "right"), # How big is the pulse?
   # PulseVolume7 = zoo::rollsum(pmax(0, Ramp1), 7, fill = NA, align = "right"),
   # PulseVolume14 = zoo::rollsum(pmax(0, Ramp1), 14, fill = NA, align = "right"),
   # 
   # # Pulse end signal
   # PeakDrop3 = MaxDischarge3 - Discharge,
   # PeakDrop7 = MaxDischarge7 - Discharge
   
) %>%
   
# =======================================================================================
# PART 4: TEMPORAL FEATURES
# =======================================================================================
mutate(
   
   # Cyclical encoding for smooth seasonality
   DaySin = sin(2 * pi * DayOfYear / 365.25),
   DayCos = cos(2 * pi * DayOfYear / 365.25),
   
) 

# Remove all NaNs and Infinites from computation
model_data[] <- lapply(model_data, function(x) {
   x[is.nan(x) | is.infinite(x)] <- NA
   x
})

model_data <- model_data %>%
   relocate(FERC, Salinity, Discharge, .after = DayOfYear) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   relocate(DaySin, DayCos, .after = DayOfYear) %>%
   relocate(Salinity, .after = DayOfYear) %>%
   relocate(FERC, .after = DayOfYear)

model_data <- model_data %>%
   dplyr::select(-c(DaySin, DayCos, MaxDischarge1))

# Write output files
outputs <- list(model_data)
file_names <- c('FinalModelData')
write_qs_files(outputs, 'Data/Tidied/Final/Daily', file_names)

# Clear global environment
rm(list = ls())