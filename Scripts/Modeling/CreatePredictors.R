# =============================================================================
# Script Name:    EngineerModelData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-06-01
# Last Updated:   2025-07-16
# Description:    Loads raw hourly data, tidies columns, and then creates a 
#                 large number of variables that I think might predict salinity.
#                 Categories include tide, discharge, inflows, stress/drought. 
#                 Continuous predictors are then normalized and data is written
#                 to .qs format as 'FinalModelData.qs'
# =============================================================================

# Source necessary functions
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

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
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>%
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

rm(meteo, q_sal_data, dir1, dir2, dirs)

####################### MODEL DATA PREPARATION PIPELINE ##########################

# Create the model data
model_data <- data %>%
   #filter(!is.na(Salinity)) %>%                              # Keep only times with available salinity data
   
# =======================================================================================
# PART 1: TIDE PREDICTORS
# =======================================================================================

mutate(
   # Lagged Tide Features
   LagTide1 = lag(Tide, 1),
   LagTide2 = lag(Tide, 2),
   LagTide4 = lag(Tide, 4),
   LagTide6 = lag(Tide, 6),
   LagTide12 = lag(Tide, 12),
   LagTide24 = lag(Tide, 24),
   
   # Tidal Range Metrics
   TideRange3 = rollapply(Tide, width = 3,
                           FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                           fill = NA, align = "right"),
   TideRange6 = rollapply(Tide, width = 6, 
                          FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                          fill = NA, align = "right"),
   TideRange12 = rollapply(Tide, width = 12,
                           FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                           fill = NA, align = "right"),
   TideRange24 = rollapply(Tide, width = 24,
                           FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                           fill = NA, align = "right"),
   TideRange48 = rollapply(Tide, width = 48,
                           FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                           fill = NA, align = "right"),
   
   TidalVelocity = Tide - lag(Tide, 1)
) %>%
   
# =======================================================================================
# PART 2: WIND PREDICTORS
# =======================================================================================

mutate(
   # U (east-west) and V (north-south) wind magnitudes
   direction_radians = WDIR * pi / 180,
   U = -WSPD * sin(direction_radians), # east-west, cross estuary
   V = -WSPD * cos(direction_radians), # north-south, along estuary
   
   # Lagged Wind Predictors 
   LagU1 = lag(U, 1),
   LagU3 = lag(U, 3),
   LagU6 = lag(U, 6),
   LagU10 = lag(U, 10),
   LagU12 = lag(U, 12),
   LagU24 = lag(U, 24),
   LagU36 = lag(U, 36),
   LagU48 = lag(U, 48),
   LagU72 = lag(U, 72),
   LagU168 = lag(U, 168),
   
   LagV1 = lag(V, 1),
   LagV3 = lag(V, 3),
   LagV6 = lag(V, 6),
   LagV10 = lag(V, 10),
   LagV12 = lag(V, 12),
   LagV24 = lag(V, 24),
   LagV36 = lag(V, 36),
   LagV48 = lag(V, 48),
   LagV72 = lag(V, 72),
   LagV168 = lag(V, 168),
   
   # Rolling Wind Predictors
   RollingU12 = zoo::rollmean(U, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingU24 = zoo::rollmean(U, 24, fill = NA, align = "right", na.rm = TRUE),
   RollingU48 = zoo::rollmean(U, 48, fill = NA, align = "right", na.rm = TRUE),
   RollingU72 = zoo::rollmean(U, 72, fill = NA, align = "right", na.rm = TRUE),
   RollingU168 = zoo::rollmean(U, 168, fill = NA, align = "right", na.rm = TRUE),
   
   RollingV12 = zoo::rollmean(V, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingV24 = zoo::rollmean(V, 24, fill = NA, align = "right", na.rm = TRUE),
   RollingV48 = zoo::rollmean(V, 48, fill = NA, align = "right", na.rm = TRUE),
   RollingV72 = zoo::rollmean(V, 72, fill = NA, align = "right", na.rm = TRUE),
   RollingV168 = zoo::rollmean(V, 168, fill = NA, align = "right", na.rm = TRUE),
   
   # Gust Predictors
   MaxGust24 = rollapply(Gust, width = 24, FUN = function(x) max(x, na.rm = TRUE), fill = NA, align = "right"),
   MaxGust72 = rollapply(Gust, width = 72, FUN = function(x) max(x, na.rm = TRUE), fill = NA, align = "right"),
   
   # Wind Magnitude Predictors
   WindSpeed = WSPD,
   RollingWindSpeed24 = zoo::rollmean(WSPD, 24, fill = NA, align = "right", na.rm = TRUE),
   RollingWindSpeed72 = zoo::rollmean(WSPD, 72, fill = NA, align = "right", na.rm = TRUE),
   
) %>%
   select(-c(direction_radians, WDIR, WSPD)) %>%
   
# =======================================================================================
# PART 3: BASIC DISCHARGE FEATURES
# =======================================================================================
mutate(
   # Lagged Conowingo Discharges
   LagDischarge1 = lag(Discharge, 1),
   LagDischarge3 = lag(Discharge, 3),
   LagDischarge6 = lag(Discharge, 6),
   LagDischarge10 = lag(Discharge, 10),
   LagDischarge12 = lag(Discharge, 12),
   LagDischarge24 = lag(Discharge, 24),
   LagDischarge36 = lag(Discharge, 36),
   LagDischarge48 = lag(Discharge, 48),
   LagDischarge72 = lag(Discharge, 72),
   LagDischarge96 = lag(Discharge, 96),
   
   # Lagged Marietta Inflows
   LagInflows48 = lag(Inflows, 48),
   LagInflows72 = lag(Inflows, 72),
   LagInflows96 = lag(Inflows, 96),
   LagInflows120 = lag(Inflows, 120),
   LagInflows144 = lag(Inflows, 144),
   
   # Rolling Discharge
   RollingDischarge3   = zoo::rollmean(Discharge, 3, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge6   = zoo::rollmean(Discharge, 6, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge12  = zoo::rollmean(Discharge, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge24  = zoo::rollmean(Discharge, 24, fill = NA, align = "right", na.rm = TRUE),
   RollingDischarge48  = zoo::rollmean(Discharge, 48, fill = NA, align = "right", na.rm = TRUE),
   
   # Rolling Inflows (by # of days)
   RollingInflows3   = zoo::rollmean(Inflows, 24 * 3, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows7   = zoo::rollmean(Inflows, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows14  = zoo::rollmean(Inflows, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows30  = zoo::rollmean(Inflows, 24 * 24, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows90  = zoo::rollmean(Inflows, 24 * 90, fill = NA, align = "right", na.rm = TRUE),
   
) %>%

# =======================================================================================
# PART 4: TEMPORAL FEATURES
# =======================================================================================
mutate(
   # Cyclical encoding for smooth seasonality
   MonthSin = sin(2 * pi * Month / 12),
   MonthCos = cos(2 * pi * Month / 12),
   DaySin = sin(2 * pi * DayOfYear / 365.25),
   DayCos = cos(2 * pi * DayOfYear / 365.25),
   
   # Hour of day (diurnal patterns in stratification/mixing)
   Hour = lubridate::hour(DateTime),
   HourSin = sin(2 * pi * Hour / 24),
   HourCos = cos(2 * pi * Hour / 24)
) %>%
   select(-Hour)  # Drop after encoding
   
# Remove all NaNs and Infinites from computation
model_data[] <- lapply(model_data, function(x) {
   x[is.nan(x) | is.infinite(x)] <- NA
   x
})

model_data <- model_data %>%
   relocate(FERC, Salinity, Discharge, .after = DayOfYear) %>%
   mutate_if(is.numeric, round, digits = 3) %>%
   relocate(DaySin, DayCos, MonthSin, MonthCos, HourSin, HourCos, .after = DayOfYear) %>%
   relocate(Salinity, .after = DayOfYear) %>%
   relocate(FERC, .after = DayOfYear)

# Normalize Predictors and Add to model_data
preds_to_normalize <- colnames(model_data)[which(colnames(model_data) == 'Discharge') : ncol(model_data)] # Starting from the discharge column

# Apply the normalization function
normalized_predictors <- normalize_multiple_predictors(model_data, preds_to_normalize)
model_data <- normalized_predictors$data
norm_params <- normalized_predictors$parameters

# Write output files
outputs <- list(model_data, norm_params)
file_names <- c('FinalModelData', 'FinalNormalizationParams')
write_qs_files(outputs, 'Data/Tidied/Final', file_names)

# Clear global environment
rm(list = ls())

