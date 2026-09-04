# =============================================================================
# Script Name:    TidyDischargeData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Reads raw Conowingo Dam discharge data (USGS 01578310,
#                 15-min, cfs), converts to cubic meters per second, tidies,
#                 aggregates to hourly, and writes a clean DateTime/Discharge
#                 CSV to Data/Tidied/Hourly/Discharge.csv.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

source('Scripts/Utilities/AggregateHourly.R')

# =============================================================================
# SYSTEM-SPECIFIC PARAMETERS
# =============================================================================
CFS_TO_M3S <- 0.0283

# =============================================================================
# LOAD AND TIDY
# =============================================================================
discharge_raw <- read.csv('Data/Raw/Conowingo_Discharge.csv')
#discharge_raw2 <- read.csv('Data/Raw/CSV/Conowingo_Discharge2.csv')
#discharge_raw <- bind_rows(discharge_raw, discharge_raw2)
#rm(discharge_raw2)

discharge <- discharge_raw %>%
   mutate(DateTime = paste(Date, Time, sep = ' ')) %>%
   mutate(DateTime = as.POSIXct(DateTime, format = '%m/%d/%y %H:%M', tz = 'UTC')) %>%
   mutate(DateTime = round_date(DateTime, unit = '15 mins')) %>%
   mutate(Discharge = Discharge * CFS_TO_M3S) %>%
   dplyr::select(DateTime, Discharge) %>%
   filter(!is.na(Discharge)) %>%
   arrange(DateTime)

# =============================================================================
# AGGREGATE TO HOURLY
# =============================================================================
discharge_hourly <- aggregate_hourly(discharge, value_cols = 'Discharge')

# =============================================================================
# WRITE
# =============================================================================
write.csv(discharge_hourly, 'Data/Tidied/Hourly/Discharge.csv', row.names = FALSE)

rm(list = ls())
