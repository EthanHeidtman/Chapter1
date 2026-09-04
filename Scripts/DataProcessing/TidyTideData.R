# =============================================================================
# Script Name:    TidyTideData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Reads raw USGS 01580620 gage height (Havre de Grace, MD,
#                 2012-2026, 6-min, NAVD88) and raw Chesapeake City verified
#                 tide data (2003-2026, hourly, MLLW). Converts USGS data to
#                 meters above MLLW, aggregates to hourly, and fits a
#                 CCity -> HdG transform on the full overlap period. That
#                 transform is used to gap-fill 2007-2012 (pre-USGS-gauge)
#                 using CCity data. Writes a single continuous hourly tide
#                 series, 2007-2026, meters above MLLW, to
#                 Data/Tidied/Hourly/Tide.csv.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(Metrics)
library(ggplot2)
library(patchwork)

source('Scripts/Utilities/AggregateHourly.R')
source('Scripts/Utilities/FitTidalData.R')   # provides tidal_fitting()

# =============================================================================
# SYSTEM-SPECIFIC PARAMETERS
# Change these when moving to a new system / gauge
# =============================================================================
# USGS 01580620 station metadata: "Datum of gage is 0.00 ft above NAVD88.
# To convert to tidal datum Mean Lower Low Water, add 1.03 ft to NAVD88 values."
NAVD88_TO_MLLW_OFFSET_FT <- 1.03

# Local timezone of the USGS tz_cd field (IANA tz name, handles EST/EDT
# transitions automatically rather than hardcoding fixed UTC offsets)
USGS_LOCAL_TZ <- 'America/New_York'

# Feet -> meters
FT_TO_M <- 0.3048

# Start of the desired output record (salinity data begins 2007-03-30, but
# the full calendar year 2007 is wanted)
RECORD_START <- as_datetime('2007-01-01 00:00:00', tz = 'UTC')

# =============================================================================
# LOAD AND TIDY: USGS HAVRE DE GRACE GAGE HEIGHT (2012-2026)
# =============================================================================
# Raw file is USGS tab-delimited instantaneous-values format:
#   - lines beginning with '#' are comments
#   - header row, followed by a units/format-spec row (e.g. '5s 15s 20d ...')
#     that must be dropped, not treated as data
#   - columns: agency_cd, site_no, datetime, tz_cd, <param>, <param>_cd

usgs_raw <- read.delim('Data/Raw/HdG_GageHeight_01580620.txt',
                       comment.char = '#',
                       sep = '\t',
                       colClasses = 'character')

# Drop the units/format-spec row (values like '5s', '15s', '20d', '6s', '14n', '10s')
usgs_raw <- usgs_raw %>%
   filter(!grepl('^[0-9]+[a-z]$', agency_cd))

# Identify the value column dynamically (named like '69562_00065'; the
# leading TS number can vary by station/pull, so match on the '_00065' suffix
# rather than hardcoding the full column name)
value_col <- names(usgs_raw)[grepl('_00065$', names(usgs_raw))]
if (length(value_col) != 1) {
   stop('TidyTideData: could not uniquely identify the gage height (00065) column')
}

usgs_tide <- usgs_raw %>%
   dplyr::select(datetime, tz_cd, all_of(value_col)) %>%
   rename(GageHeight_ft = all_of(value_col)) %>%
   mutate(GageHeight_ft = as.numeric(GageHeight_ft)) %>%
   filter(!is.na(GageHeight_ft)) %>%
   # Parse local datetime per-row respecting tz_cd (EST/EDT), then convert to UTC.
   # Using the IANA tz database (America/New_York) lets R resolve the correct
   # UTC offset for each timestamp automatically rather than hand-mapping
   # tz_cd strings, which avoids DST-transition errors.
   mutate(DateTime_local = as_datetime(datetime, format = '%Y-%m-%d %H:%M', tz = USGS_LOCAL_TZ),
          DateTime = with_tz(DateTime_local, tzone = 'UTC')) %>%
   # Convert NAVD88 (ft) -> MLLW (ft) -> meters
   mutate(USGS_HdG = (GageHeight_ft + NAVD88_TO_MLLW_OFFSET_FT) * FT_TO_M) %>%
   dplyr::select(DateTime, USGS_HdG) %>%
   arrange(DateTime)

# Aggregate 6-min -> hourly
usgs_tide_hourly <- aggregate_hourly(usgs_tide, value_cols = 'USGS_HdG')

# =============================================================================
# LOAD AND TIDY: CHESAPEAKE CITY VERIFIED TIDE (2003-2026, already hourly, MLLW)
# =============================================================================
# NOAA CO-OPS verified water level export. Datum = MLLW, units = meters,
# timezone = GMT (equivalent to UTC for this purpose).

ccity_raw <- read.csv('Data/Raw/ChesapeakeCity_Tides.csv', na.strings = c("-", "", "NA"))

ccity_tide <- ccity_raw %>%
   # mdy_hm handles M/D/YY H:MM format cleanly without needing manual string padding
   mutate(DateTime = mdy_hm(paste(Date, Time..GMT.), tz = 'GMT')) %>%
   mutate(DateTime = with_tz(DateTime, tzone = 'UTC')) %>%
   dplyr::select(DateTime, Verified..m.) %>%
   rename(CCity = Verified..m.) %>%
   mutate(CCity = as.numeric(CCity)) %>%
   filter(!is.na(DateTime), !is.na(CCity)) %>%
   arrange(DateTime)

# Already hourly, but run through the same utility for consistency /
# to collapse any duplicate timestamps
ccity_tide_hourly <- aggregate_hourly(ccity_tide, value_cols = 'CCity')

# =============================================================================
# FIT CCITY -> USGS HDG TRANSFORM ON FULL OVERLAP (2012-2026)
# =============================================================================
overlap <- inner_join(usgs_tide_hourly, ccity_tide_hourly, by = 'DateTime') %>%
   na.omit()

# Initial parameter estimates for sine fit (see Scripts/Utilities/FitTidalData.R)
A0 <- (max(overlap$USGS_HdG, na.rm = TRUE) - min(overlap$USGS_HdG, na.rm = TRUE)) / 2
B0 <- 2 * pi / 12.42
C0 <- 0
D0 <- mean(overlap$USGS_HdG, na.rm = TRUE)

overlap <- overlap %>%
   mutate(time_hours = as.numeric(difftime(DateTime, DateTime[1], units = 'hours')))

# Weight toward higher/lower tides (salt-intrusion relevant extremes), consistent
# with the original tidal_fitting() weighting scheme
overlap <- overlap %>%
   rename(HdG = USGS_HdG) %>%
   mutate(HdG_weights = ifelse(HdG > quantile(HdG, 0.75, na.rm = TRUE) |
                                  HdG < quantile(HdG, 0.25, na.rm = TRUE), 4, 1),
          CCity_weights = ifelse(CCity > quantile(CCity, 0.75, na.rm = TRUE) |
                                    CCity < quantile(CCity, 0.25, na.rm = TRUE), 4, 1))

fit_transform <- function(df, A, B, C, D) {
   HdG_fit <- nls(HdG ~ A * sin(B * time_hours + C) + D,
                  data = df, start = list(A = A, B = B, C = C, D = D),
                  weights = df$HdG_weights, algorithm = 'port')
   CCity_fit <- nls(CCity ~ A * sin(B * time_hours + C) + D,
                    data = df, start = list(A = A, B = B, C = C, D = D),
                    weights = df$CCity_weights, algorithm = 'port')
   
   HdG_params <- coef(HdG_fit)
   CCity_params <- coef(CCity_fit)
   
   if (HdG_params['A'] < 0) {
      HdG_params['A'] <- HdG_params['A'] * -1
      HdG_params['C'] <- HdG_params['C'] + pi
   }
   if (CCity_params['A'] < 0) {
      CCity_params['A'] <- CCity_params['A'] * -1
      CCity_params['C'] <- CCity_params['C'] + pi
   }
   
   list(ratio = HdG_params['A'] / CCity_params['A'],
        CCity_D = CCity_params['D'],
        HdG_D = HdG_params['D'])
}

# Fit on the full overlap
full_fit <- fit_transform(overlap, A0, B0, C0, D0)

# ---- Sub-period stability check ----
# Split the overlap into ~4-year chunks and refit independently to check
# whether the ratio/offset are stable across the 2012-2026 record, rather
# than assuming a single global calibration holds for the entire period.
overlap <- overlap %>% mutate(Year = year(DateTime))
year_breaks <- seq(min(overlap$Year), max(overlap$Year), by = 4)
chunk_labels <- cut(overlap$Year, breaks = c(year_breaks, max(overlap$Year) + 1),
                    include.lowest = TRUE, right = FALSE)
overlap$Chunk <- chunk_labels

sub_period_fits <- overlap %>%
   group_split(Chunk) %>%
   purrr::map_dfr(function(chunk_df) {
      if (nrow(chunk_df) < 100) return(NULL)   # skip chunks too small to fit reliably
      fit <- tryCatch(fit_transform(chunk_df, A0, B0, C0, D0), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      data.frame(Chunk = unique(chunk_df$Chunk),
                 Years = paste(min(chunk_df$Year), max(chunk_df$Year), sep = '-'),
                 N = nrow(chunk_df),
                 Ratio = fit$ratio,
                 CCity_D = fit$CCity_D,
                 HdG_D = fit$HdG_D)
   })

print(sub_period_fits)
# NOTE: inspect Ratio and CCity_D/HdG_D across chunks before trusting a single
# global transform for the 2007-2012 gap-fill. If these drift meaningfully
# across sub-periods, the global fit below should be reconsidered (e.g.
# piecewise, or trend-adjusted) rather than accepted as-is.

# =============================================================================
# APPLY TRANSFORM TO GAP-FILL 2007-2012
# =============================================================================
gap_fill <- ccity_tide_hourly %>%
   filter(DateTime >= RECORD_START, DateTime < min(usgs_tide_hourly$DateTime)) %>%
   mutate(Tide = full_fit$ratio * (CCity - full_fit$CCity_D) + full_fit$HdG_D) %>%
   dplyr::select(DateTime, Tide)

# =============================================================================
# ASSEMBLE FINAL SERIES: REAL USGS (2012-2026) + TRANSFORMED CCITY (2007-2012)
# =============================================================================
usgs_final <- usgs_tide_hourly %>%
   rename(Tide = USGS_HdG) %>%
   dplyr::select(DateTime, Tide)

tide_final <- bind_rows(gap_fill, usgs_final) %>%
   arrange(DateTime) %>%
   distinct(DateTime, .keep_all = TRUE)

# =============================================================================
# VALIDATION PLOTS
# =============================================================================
# Overlap-period accuracy: apply the fitted transform back onto CCity within
# the overlap window and compare against real USGS HdG
overlap_check <- overlap %>%
   mutate(CCity_transformed = full_fit$ratio * (CCity - full_fit$CCity_D) + full_fit$HdG_D)

overall_rmse <- rmse(overlap_check$HdG, overlap_check$CCity_transformed)
overall_r2 <- cor(overlap_check$HdG, overlap_check$CCity_transformed)^2

p1 <- ggplot(overlap_check, aes(x = DateTime)) +
   geom_line(aes(y = HdG, color = 'Measured USGS HdG'), linewidth = 1) +
   geom_line(aes(y = CCity_transformed, color = 'Transformed CCity'), linewidth = 0.8) +
   scale_x_datetime(limits = c(as_datetime('2015-08-01'), as_datetime('2015-08-10'))) +
   theme_bw() +
   labs(x = 'DateTime (UTC)', y = 'Tidal Height (m, MLLW)',
        title = 'Overlap-Period Check: Measured USGS vs. Transformed CCity',
        color = NULL) +
   theme(legend.position = 'bottom')

p2 <- ggplot(overlap_check, aes(x = HdG, y = CCity_transformed)) +
   geom_point(alpha = 0.3, color = '#97BEE5') +
   geom_abline(color = 'black') +
   theme_bw() +
   labs(x = 'Measured USGS HdG (m)', y = 'Transformed CCity (m)',
        title = paste0('Full Overlap Fit  |  RMSE = ', round(overall_rmse, 3),
                       ' m, R2 = ', round(overall_r2, 3)))

p3 <- ggplot(sub_period_fits, aes(x = Years, y = Ratio)) +
   geom_col(fill = '#97BEE5') +
   theme_bw() +
   labs(x = NULL, y = 'Amplitude Ratio (HdG/CCity)',
        title = 'Sub-Period Stability of Fitted Amplitude Ratio')

validation_plot <- (p1 / p2 / p3)

ggsave('TideTransformValidation.svg', validation_plot,
       path = 'Outputs/Plots/TidalFitting', height = 12, width = 8)
ggsave('TideTransformValidation.png', validation_plot,
       path = 'Outputs/Plots/TidalFitting', dpi = 600, height = 12, width = 8)

# =============================================================================
# WRITE FINAL HOURLY TIDE DATA
# =============================================================================
write.csv(tide_final, 'Data/Tidied/Hourly/Tide.csv', row.names = FALSE)

rm(list = ls())
