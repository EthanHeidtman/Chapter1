# =============================================================================
# Script Name:    TidySalinityData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Reads raw DNR salinity data (Susquehanna EOT buoy, 15-min,
#                 ppt), tidies, aggregates to hourly, and writes a clean
#                 DateTime/Salinity CSV to Data/Tidied/Hourly/Salinity.csv.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

source('Scripts/Utilities/AggregateHourly.R')

# =============================================================================
# LOAD AND TIDY
# =============================================================================
salinity_raw <- read.csv('Data/Raw/EOTBData_Susquehanna_30Mar07_TO_06Nov24.csv')

salinity <- salinity_raw %>%
   dplyr::select(3, 6, 7) %>%
   setNames(c('DateTime', 'Salinity', 'Temp')) %>%
   mutate(DateTime = as.POSIXct(DateTime, format = '%m/%d/%y %H:%M', tz = 'UTC')) %>%
   mutate(DateTime = round_date(DateTime, unit = '15 mins')) %>%
   dplyr::select(DateTime, Salinity) %>%
   filter(!is.na(Salinity)) %>%
   arrange(DateTime)

# =============================================================================
# AGGREGATE TO HOURLY
# =============================================================================
salinity_hourly <- aggregate_hourly(salinity, value_cols = 'Salinity')

# =============================================================================
# WRITE
# =============================================================================
write.csv(salinity_hourly, 'Data/Tidied/Hourly/Salinity.csv', row.names = FALSE)

rm(list = ls())