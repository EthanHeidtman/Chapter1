# =============================================================================
# Script Name:    TidyWindData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Reads raw Susquehanna buoy meteo text files (6-min, WDIR/
#                 WSPD/GST), tidies, replaces sentinel NA codes, aggregates
#                 to hourly using speed-weighted vector components, and writes 
#                 a clean hourly CSV to Data/Tidied/Hourly/Wind.csv.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

source('Scripts/Utilities/LoadTextFiles.R')   # provides combine_txt_files()

# =============================================================================
# 1. LOAD AND TIDY RAW 6-MINUTE DATA
# =============================================================================
meteo_raw <- combine_txt_files('Data/Raw/Text/SusquehannaBuoy/Meteo')

meteo <- meteo_raw %>%
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>%
   dplyr::select(-c(YY, MM, DD, hh, mm)) %>%
   relocate(DateTime) %>%
   # NDBC sentinel missing-value codes (e.g. 999, 99.0, 9999) -> NA
   mutate(across(where(is.numeric),
                 ~ if_else(grepl("^9+\\.?9*$", as.character(.x)), NA_real_, .x))) %>%
   dplyr::select(DateTime, WDIR, WSPD, GST) %>%
   filter(!is.na(WDIR) | !is.na(WSPD) | !is.na(GST)) %>%
   arrange(DateTime)

# =============================================================================
# 2. SPEED-WEIGHTED VECTOR AGGREGATION TO HOURLY
# =============================================================================
wind_hourly <- meteo %>%
   mutate(
      wdir_rad = WDIR * pi / 180,
      # Decompose into Cartesian u (eastward) and v (northward) vector components at 6-min
      u = -WSPD * sin(wdir_rad),
      v = -WSPD * cos(wdir_rad),
      DateTime = floor_date(DateTime, unit = 'hour')
   ) %>%
   group_by(DateTime) %>%
   summarise(
      u_mean  = mean(u, na.rm = TRUE),
      v_mean  = mean(v, na.rm = TRUE),
      WSPD    = mean(WSPD, na.rm = TRUE),  # Mean scalar wind speed
      GST     = max(GST, na.rm = TRUE),   # Peak gust within the hour
      .groups = 'drop'
   ) %>%
   mutate(
      # Reconstruct speed-weighted vector direction in degrees [0, 360)
      WDIR = (atan2(-u_mean, -v_mean) * 180 / pi) %% 360
   ) %>%
   # Replace NaN / Inf resulting from empty/all-NA hours with NA
   mutate(across(c(WDIR, WSPD, GST), ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x))) %>%
   dplyr::select(DateTime, WDIR, WSPD, GST) %>%
   arrange(DateTime)

# =============================================================================
# 3. WRITE OUTPUT
# =============================================================================
write.csv(wind_hourly, 'Data/Tidied/Hourly/Wind.csv', row.names = FALSE)

rm(list = ls())
