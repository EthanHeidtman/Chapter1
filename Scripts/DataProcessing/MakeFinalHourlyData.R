# =============================================================================
# Script Name:    MakeFinalHourlyData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Reads the already-tidied, already-hourly per-source CSVs
#                 (Tide, Salinity, Discharge, Wind) and joins them into a
#                 single hourly dataframe, 2007-2024. No raw reading or
#                 tidying happens here -- that lives in the per-source
#                 scripts (TidyTideData.R, TidySalinityData.R,
#                 TidyDischargeData.R, TidyWindData.R). This script only
#                 assembles their outputs.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

# =============================================================================
# PARAMETERS
# =============================================================================
RECORD_START <- as_datetime('2007-01-01 00:00:00', tz = 'UTC')
RECORD_END   <- as_datetime('2024-12-31 23:00:00', tz = 'UTC')

# =============================================================================
# LOAD TIDIED HOURLY SOURCE DATA
# =============================================================================
tide      <- read.csv('Data/Tidied/Hourly/Tide.csv')      %>% mutate(DateTime = as_datetime(DateTime, tz = 'UTC'))
salinity  <- read.csv('Data/Tidied/Hourly/Salinity.csv')  %>% mutate(DateTime = as_datetime(DateTime, tz = 'UTC'))
discharge <- read.csv('Data/Tidied/Hourly/Discharge.csv') %>% mutate(DateTime = as_datetime(DateTime, tz = 'UTC'))
wind      <- read.csv('Data/Tidied/Hourly/Wind.csv')      %>% mutate(DateTime = as_datetime(DateTime, tz = 'UTC'))

# =============================================================================
# MERGE (full outer join on DateTime -- gaps become NA, nothing truncated)
# =============================================================================
data <- tide %>%
   full_join(salinity, by = 'DateTime') %>%
   full_join(discharge, by = 'DateTime') %>%
   full_join(wind, by = 'DateTime') %>%
   rename(Gust = GST) %>%
   filter(DateTime >= RECORD_START, DateTime <= RECORD_END) %>%
   mutate(Year  = year(DateTime),
          Month = month(DateTime),
          Day   = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   arrange(DateTime)

# =============================================================================
# WRITE
# =============================================================================
write.csv(data, 'Data/Tidied/Hourly/HourlyDataFinal.csv', row.names = FALSE)

rm(list = ls())
