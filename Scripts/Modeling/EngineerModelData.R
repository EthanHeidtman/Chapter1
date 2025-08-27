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

# Read in final hourly data
data <- read.csv('Data/Tidied/Processed/HourlyDataFinal.csv', 
                 colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

data <- data %>%
   dplyr::select(-c(9, 10)) %>%                              # Remove extra columns
   mutate(DateTime = as_datetime(DateTime)) %>%              # Make dates class datetime
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00')) %>% # Keep only dates before 
   mutate(Season = case_when(
      Month %in% c(12, 1, 2) ~ 'Winter',
      Month %in% c(3, 4, 5) ~ 'Spring',
      Month %in% c(6, 7, 8) ~ 'Summer',
      Month %in% c(9, 10, 11) ~ 'Fall'
   )) %>%
   mutate_if(is.character, as.factor) %>%
   relocate(Season, .after = DayOfYear)

####################### MODEL DATA PREPARATION PIPELINE ##########################

# Create the model data
model_data <- data %>%
   filter(!is.na(Salinity)) %>%                              # Keep only times with available salinity data
   
# =======================================================================================
# PART 0: BASIC TIDE FEATURES
# =======================================================================================

mutate(
   # Lagged Tide Features
   LagTide1 = lag(Tide, 1),
   LagTide2 = lag(Tide, 2),
   LagTide4 = lag(Tide, 4),
   
   # Basic tidal velocity (rate of change) - key for salt transport
   TideVelocity = c(NA, diff(Tide) / 0.25), # 15-min intervals, units: m/hr
   TideVelocity = zoo::rollmean(TideVelocity, k = 3, fill = NA, align = "center"), # Smooth
   
   # Flood vs Ebb tide based on velocity
   IsFloodTide = TideVelocity > 0.01,  # Positive = incoming tide (brings salt)
   IsEbbTide = TideVelocity < -0.01,   # Negative = outgoing tide (flushes salt)
   IsSlackTide = abs(TideVelocity) <= 0.01,
   
   # Tidal acceleration (change in velocity) - indicates tidal strength
   TideAcceleration = c(NA, diff(TideVelocity) / 0.25),
   
   # Tidal Range Metrics
   TideRange6 = rollapply(Tide, width = 6, 
                          FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                          fill = NA, align = "right"),
   TideRange12 = rollapply(Tide, width = 12,
                           FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                           fill = NA, align = "right"),
   TideRange24 = rollapply(Tide, width = 24,
                           FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                           fill = NA, align = "right")
) %>%
   
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
   
   # Lagged Marietta Inflows (account for residence time and travel)
   LagInflows12 = lag(Inflows, 12),
   LagInflows24 = lag(Inflows, 24),
   LagInflows48 = lag(Inflows, 48),
   LagInflows72 = lag(Inflows, 72),
   LagInflows96 = lag(Inflows, 96),
   
   # Power Law Transformations (-0.5 determined to be best)
   # compared to -0.35 and -0.40 and a log transformation of discharge
   PowDischarge = Discharge ^ (-0.5),
   PowLagDischarge1 = LagDischarge1 ^ (-0.5),
   PowLagDischarge3 = LagDischarge3 ^ (-0.5),
   PowLagDischarge6 = LagDischarge6 ^ (-0.5),
   PowLagDischarge10 = LagDischarge10 ^ (-0.5),
   PowLagDischarge12 = LagDischarge12 ^ (-0.5),    
   PowLagDischarge24 = LagDischarge24 ^ (-0.5),
   PowLagDischarge36 = LagDischarge36 ^ (-0.5),
   PowLagDischarge48 = LagDischarge48 ^ (-0.5),
   PowLagDischarge72 = LagDischarge72 ^ (-0.5),
   PowLagDischarge96 = LagDischarge96 ^ (-0.5),
   PowInflows = Inflows ^ (-0.5),
   PowLagInflows12 = LagInflows12 ^ (-0.5),
   PowLagInflows24 = LagInflows24 ^ (-0.5),
   PowLagInflows48 = LagInflows48 ^ (-0.5),        
   PowLagInflows72 = LagInflows72 ^ (-0.5),
   PowLagInflows96 = LagInflows96 ^ (-0.5),
   
   # Rolling Averages (by # of days)
   RollingPowDischarge0.5 = zoo::rollmean(PowDischarge, 24 * 0.5, fill = NA, align = "right", na.rm = TRUE),
   RollingPowDischarge1   = zoo::rollmean(PowDischarge, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
   RollingPowDischarge2   = zoo::rollmean(PowDischarge, 24 * 2, fill = NA, align = "right", na.rm = TRUE),
   RollingPowDischarge4   = zoo::rollmean(PowDischarge, 24 * 4, fill = NA, align = "right", na.rm = TRUE),
   RollingPowDischarge7   = zoo::rollmean(PowDischarge, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   RollingPowDischarge10  = zoo::rollmean(PowDischarge, 24 * 10, fill = NA, align = "right", na.rm = TRUE),  
   RollingPowDischarge14  = zoo::rollmean(PowDischarge, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   RollingPowInflows1     = zoo::rollmean(PowInflows, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
   RollingPowInflows2     = zoo::rollmean(PowInflows, 24 * 2, fill = NA, align = "right", na.rm = TRUE),     
   RollingPowInflows7     = zoo::rollmean(PowInflows, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   RollingPowInflows10    = zoo::rollmean(PowInflows, 24 * 10, fill = NA, align = "right", na.rm = TRUE)
) %>% 
   
# =======================================================================================
# PART 2: STRESS METRICS (Inflow based)
# =======================================================================================
   
arrange(DateTime) %>%
mutate(
   
   # Define vulnerability thresholds based on natural inflows
   LowInflowThreshold = quantile(Inflows, 0.50, na.rm = TRUE),     # 50th percentile
   VeryLowInflowThreshold = quantile(Inflows, 0.25, na.rm = TRUE), # 25th percentile
   FlushingThreshold = quantile(Inflows, 0.75, na.rm = TRUE),      # 75th percentile
   
   # Sustained low inflow conditions (key vulnerability indicator)
   IsLowInflow = Inflows < LowInflowThreshold,
   IsVeryLowInflow = Inflows < VeryLowInflowThreshold,
   IsFlushingFlow = Inflows > FlushingThreshold,
   
   # Duration of sustained low inflows (system vulnerability builds over time)
   ConsecutiveLowInflowHours = sequence(rle(IsLowInflow)$lengths) * IsLowInflow,
   ConsecutiveVeryLowInflowHours = sequence(rle(IsVeryLowInflow)$lengths) * IsVeryLowInflow,
   
   # Cumulative inflow deficit (how much flow is missing)
   InflowDeficit = pmax(0, LowInflowThreshold - Inflows, na.rm = TRUE),
   InflowDeficit = ifelse(is.na(Inflows), 0, InflowDeficit),  # Set deficit to 0 when inflows are missing
   
   # Cumulative stress over multiple time windows
   CumulativeInflowDeficit3 = zoo::rollsum(InflowDeficit, 24 * 3, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),
   CumulativeInflowDeficit7 = zoo::rollsum(InflowDeficit, 24 * 7, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),
   CumulativeInflowDeficit14 = zoo::rollsum(InflowDeficit, 24 * 14, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),
   CumulativeInflowDeficit30 = zoo::rollsum(InflowDeficit, 24 * 30, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),
   
   # Rolling count of low flow hours (frequency of stress)
   LowInflowHours7 = zoo::rollsum(as.numeric(IsLowInflow), 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   LowInflowHours14 = zoo::rollsum(as.numeric(IsLowInflow), 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   LowInflowHours30 = zoo::rollsum(as.numeric(IsLowInflow), 24 * 30, fill = NA, align = "right", na.rm = TRUE),
   
   # Time since last flushing flow (system memory)
   HoursSinceFlush = NA_real_,
   DaysSinceFlush = NA_real_
   
) %>%
   
   # Calculate hours since flushing flow
   group_by(1) %>%
   mutate(
      FlushEvent = cumsum(IsFlushingFlow),
      HoursSinceFlush = ifelse(IsFlushingFlow, 0, 
                               row_number() - ifelse(any(IsFlushingFlow), 
                                                     max(row_number()[IsFlushingFlow & FlushEvent == max(FlushEvent[IsFlushingFlow])]), 
                                                     0)),
      DaysSinceFlush = HoursSinceFlush / 24
   ) %>%
   ungroup() %>%
   select(-FlushEvent) %>%
   
# =======================================================================================
# PART 3: DROUGHT-PERSISTENCE METRICS & INDICATORS
# =======================================================================================

mutate(
   # Maximum consecutive stress hours in recent periods
   MaxConsecutiveStress7 = zoo::rollmax(ConsecutiveLowInflowHours, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
   MaxConsecutiveStress14 = zoo::rollmax(ConsecutiveLowInflowHours, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
   MaxConsecutiveStress30 = zoo::rollmax(ConsecutiveLowInflowHours, 24 * 30, fill = NA, align = "right", na.rm = TRUE),
   
   # Stress frequency (what fraction of time is stressed?)
   StressFrequency7 = LowInflowHours7 / (24 * 7),
   StressFrequency14 = LowInflowHours14 / (24 * 14),
   StressFrequency30 = LowInflowHours30 / (24 * 30),
   
   # Standardized Streamflow Index (fit using gamma distribution). Negative = drought, positive = flood
   SSI7 = compute_ssi(Inflows, datetime = DateTime, window_hours = 24 * 7, distribution = 'gamma'),
   SSI14 = compute_ssi(Inflows, datetime = DateTime, window_hours = 24 * 14, distribution = 'gamma'),
   SSI30 = compute_ssi(Inflows, datetime = DateTime, window_hours = 24 * 30, distribution = 'gamma')
) %>%
   
   # Clean up temporary variables
   select(-c(`1`, where(is.logical), Season, contains('Threshold')))
 
model_data <- model_data %>%
   relocate(FERC, Salinity, Discharge, .after = DayOfYear) %>%
   group_by(Year) %>%
   mutate(
      DayOfYear_sin = sin(2 * pi * DayOfYear / max(DayOfYear)),
      DayOfYear_cos = cos(2 * pi * DayOfYear / max(DayOfYear))
   ) %>%
   ungroup() %>%
   relocate(DayOfYear_sin, DayOfYear_cos, .after = DayOfYear)
   

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

