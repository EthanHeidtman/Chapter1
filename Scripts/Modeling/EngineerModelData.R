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

# Read in hourly discharge and salnity data
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
   relocate(Year, Month, Day, .after = DateTime)

# Merge all data into 1 dataset
data <- merge(q_sal_data, meteo, by = c('DateTime', 'Year', 'Month', 'Day'), all.x = TRUE)
data <- data %>%
   filter(Year > 2006 & Year < 2025) %>%
   mutate_if(is.numeric, round, digits = 2)

rm(meteo, q_sal_data)

# wind <- meteo %>%
#    mutate(
#       # Wind direction: convert FROM (meteorological) → TO (mathematical)
#       theta = (270 - WDIR) * pi / 180,
#       dx = WSPD * cos(theta),
#       dy = WSPD * sin(theta)
#    ) %>%
#    mutate(Year = year(DateTime),
#           Month = month(DateTime),
#           Day = day(DateTime)) %>%
#    relocate(Year, Month, Day, .after = DateTime)


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
   
   # # Basic tidal velocity (rate of change) - key for salt transport
   # TideVelocity = c(NA, diff(Tide) / 0.25), # 15-min intervals, units: m/hr
   # TideVelocity = zoo::rollmean(TideVelocity, k = 3, fill = NA, align = "center"), # Smooth
   # 
   # # Flood vs Ebb tide based on velocity
   # IsFloodTide = TideVelocity > 0.01,  # Positive = incoming tide (brings salt)
   # IsEbbTide = TideVelocity < -0.01,   # Negative = outgoing tide (flushes salt)
   # IsSlackTide = abs(TideVelocity) <= 0.01,
   # 
   # # Tidal acceleration (change in velocity) - indicates tidal strength
   # TideAcceleration = c(NA, diff(TideVelocity) / 0.25),
   
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
                           fill = NA, align = "right")
) %>%
   
# =======================================================================================
# PART 2: WIND PREDICTORS
# =======================================================================================

mutate(
   # U (east-west) and V (north-south) wind magnitudes
   direction_radians = WDIR * pi / 180,
   U = -WSPD * sin(direction_radians), # east-west
   V = -WSPD * cos(direction_radians), # north-south
   
   # Lagged Wind Predictors 
   LagU1 = lag(U, 1),
   LagU3 = lag(U, 3),
   LagU6 = lag(U, 6),
   LagU10 = lag(U, 10),
   LagU12 = lag(U, 12),
   LagU24 = lag(U, 24),
   LagU36 = lag(U, 36),
   
   LagV1 = lag(V, 1),
   LagV3 = lag(V, 3),
   LagV6 = lag(V, 6),
   LagV10 = lag(V, 10),
   LagV12 = lag(V, 12),
   LagV24 = lag(V, 24),
   LagV36 = lag(V, 36),
   
   # Rolling Wind Predictors
   RollingU3 = zoo::rollmean(U, 3, fill = NA, align = "right", na.rm = TRUE),
   RollingU6 = zoo::rollmean(U, 6, fill = NA, align = "right", na.rm = TRUE),
   RollingU12 = zoo::rollmean(U, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingU24 = zoo::rollmean(U, 24, fill = NA, align = "right", na.rm = TRUE),
   RollingU48 = zoo::rollmean(U, 48, fill = NA, align = "right", na.rm = TRUE),
   
   RollingV3 = zoo::rollmean(V, 3, fill = NA, align = "right", na.rm = TRUE),
   RollingV6 = zoo::rollmean(V, 6, fill = NA, align = "right", na.rm = TRUE),
   RollingV12 = zoo::rollmean(V, 12, fill = NA, align = "right", na.rm = TRUE),
   RollingV24 = zoo::rollmean(V, 24, fill = NA, align = "right", na.rm = TRUE),
   RollingV48 = zoo::rollmean(V, 48, fill = NA, align = "right", na.rm = TRUE),
   

) %>%
   select(-c(direction_radians, GST, WDIR, WSPD)) %>%
   
# =======================================================================================
# PART 1: BASIC DISCHARGE FEATURES
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
   LagInflows12 = lag(Inflows, 12),
   LagInflows24 = lag(Inflows, 24),
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
   RollingInflows0.5 = zoo::rollmean(Inflows, 24 * 0.5, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows1   = zoo::rollmean(Inflows, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows2   = zoo::rollmean(Inflows, 24 * 2, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows3   = zoo::rollmean(Inflows, 24 * 3, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows7   = zoo::rollmean(Inflows, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows10  = zoo::rollmean(Inflows, 24 * 10, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows14  = zoo::rollmean(Inflows, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows24  = zoo::rollmean(Inflows, 24 * 24, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows48  = zoo::rollmean(Inflows, 24 * 48, fill = NA, align = "right", na.rm = TRUE),
   RollingInflows90  = zoo::rollmean(Inflows, 24 * 90, fill = NA, align = "right", na.rm = TRUE),
   
   # # Power Law Transformations (-0.5 determined to be best)
   # # compared to -0.35 and -0.40 and a log transformation of discharge
   # PowDischarge = Discharge ^ (-0.5),
   # PowLagDischarge1 = LagDischarge1 ^ (-0.5),
   # PowLagDischarge3 = LagDischarge3 ^ (-0.5),
   # PowLagDischarge6 = LagDischarge6 ^ (-0.5),
   # PowLagDischarge10 = LagDischarge10 ^ (-0.5),
   # PowLagDischarge12 = LagDischarge12 ^ (-0.5),    
   # PowLagDischarge24 = LagDischarge24 ^ (-0.5),
   # PowLagDischarge36 = LagDischarge36 ^ (-0.5),
   # PowLagDischarge48 = LagDischarge48 ^ (-0.5),
   # PowLagDischarge72 = LagDischarge72 ^ (-0.5),
   # PowLagDischarge96 = LagDischarge96 ^ (-0.5),
   # PowInflows = Inflows ^ (-0.5),
   # PowLagInflows12 = LagInflows12 ^ (-0.5),
   # PowLagInflows24 = LagInflows24 ^ (-0.5),
   # PowLagInflows48 = LagInflows48 ^ (-0.5),        
   # PowLagInflows72 = LagInflows72 ^ (-0.5),
   # PowLagInflows96 = LagInflows96 ^ (-0.5),
   
   # # Rolling Averages (by # of days)
   # RollingPowDischarge0.5 = zoo::rollmean(PowDischarge, 24 * 0.5, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowDischarge1   = zoo::rollmean(PowDischarge, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowDischarge2   = zoo::rollmean(PowDischarge, 24 * 2, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowDischarge4   = zoo::rollmean(PowDischarge, 24 * 4, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowDischarge7   = zoo::rollmean(PowDischarge, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowDischarge10  = zoo::rollmean(PowDischarge, 24 * 10, fill = NA, align = "right", na.rm = TRUE),  
   # RollingPowDischarge14  = zoo::rollmean(PowDischarge, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowInflows1     = zoo::rollmean(PowInflows, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowInflows2     = zoo::rollmean(PowInflows, 24 * 2, fill = NA, align = "right", na.rm = TRUE),     
   # RollingPowInflows7     = zoo::rollmean(PowInflows, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowInflows10    = zoo::rollmean(PowInflows, 24 * 10, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowInflows12    = zoo::rollmean(PowInflows, 24 * 12, fill = NA, align = "right", na.rm = TRUE),
   # RollingPowInflows14    = zoo::rollmean(PowInflows, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   
)
   
# Remove all NaNs and Infinites from computation
model_data[] <- lapply(model_data, function(x) {
   x[is.nan(x) | is.infinite(x)] <- NA
   x
})


model_data <- model_data %>%
   relocate(FERC, Salinity, Discharge, .after = DayOfYear) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   mutate(
      DayOfYear_sin = sin(2 * pi * DayOfYear / 365.25),
      DayOfYear_cos = cos(2 * pi * DayOfYear / 365.25)
   ) %>%
   relocate(DayOfYear_sin, DayOfYear_cos, .after = DayOfYear) %>%
   filter(DateTime > '2008-11-01') # when all instruments are online and working

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

