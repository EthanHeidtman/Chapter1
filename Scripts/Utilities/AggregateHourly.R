# =============================================================================
# Script Name:    AggregateHourly.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Utility function to aggregate an arbitrary-resolution time
#                 series to hourly resolution by taking the mean of all
#                 observations within each hour. Used by per-source tidying
#                 scripts (tide, discharge, salinity, wind, etc.) so the
#                 aggregation logic lives in one place.
# =============================================================================
library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------
# aggregate_hourly()
#
# Args:
#   data       - dataframe containing a DateTime column and one or more
#                numeric value columns
#   value_cols - character vector of column name(s) to aggregate (mean).
#                All other non-DateTime columns are dropped.
#   datetime_col - name of the datetime column (default 'DateTime')
#
# Returns:
#   dataframe with columns: DateTime (floored to the hour, UTC) and the
#   aggregated value_cols, one row per hour present in the input data.
# -----------------------------------------------------------------------------
aggregate_hourly <- function(data, value_cols, datetime_col = 'DateTime') {
   
   if (!datetime_col %in% names(data)) {
      stop(paste0("aggregate_hourly: '", datetime_col, "' not found in data"))
   }
   missing_cols <- setdiff(value_cols, names(data))
   if (length(missing_cols) > 0) {
      stop(paste0("aggregate_hourly: value_cols not found in data: ",
                  paste(missing_cols, collapse = ', ')))
   }
   
   data_hourly <- data %>%
      rename(DateTime = all_of(datetime_col)) %>%
      mutate(DateTime = floor_date(DateTime, unit = 'hour')) %>%
      group_by(DateTime) %>%
      summarise(across(all_of(value_cols),
                       ~ if (cur_column() == 'Salinity') max(.x, na.rm = TRUE) else mean(.x, na.rm = TRUE)),
                .groups = 'drop') %>%
      mutate(across(all_of(value_cols), ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x))) %>%
      arrange(DateTime)
   
   return(data_hourly)
}

# # -----------------------------------------------------------------------------
# # aggregate_hourly_circular()
# #
# # Aggregates a circular (directional, degrees 0-360) variable to hourly
# # resolution using a circular mean (average of unit-vector sin/cos
# # components, then atan2), rather than a plain arithmetic mean. A linear
# # mean is wrong for direction data near the 0/360 wraparound (e.g. mean of
# # 350 deg and 10 deg should be 0 deg, not 180 deg).
# #
# # Args:
# #   data         - dataframe containing a DateTime column and a directional
# #                  value column in degrees
# #   value_col    - name of the directional column to aggregate
# #   datetime_col - name of the datetime column (default 'DateTime')
# #
# # Returns:
# #   dataframe with columns: DateTime (floored to the hour, UTC) and the
# #   circular-mean-aggregated value_col, in degrees [0, 360).
# # -----------------------------------------------------------------------------
# aggregate_hourly_circular <- function(data, value_col, datetime_col = 'DateTime') {
#    
#    if (!datetime_col %in% names(data)) {
#       stop(paste0("aggregate_hourly_circular: '", datetime_col, "' not found in data"))
#    }
#    if (!value_col %in% names(data)) {
#       stop(paste0("aggregate_hourly_circular: '", value_col, "' not found in data"))
#    }
#    
#    data_hourly <- data %>%
#       rename(DateTime = all_of(datetime_col), Direction = all_of(value_col)) %>%
#       mutate(DateTime = floor_date(DateTime, unit = 'hour'),
#              Direction_rad = Direction * pi / 180) %>%
#       group_by(DateTime) %>%
#       summarise(sin_mean = mean(sin(Direction_rad), na.rm = TRUE),
#                 cos_mean = mean(cos(Direction_rad), na.rm = TRUE),
#                 .groups = 'drop') %>%
#       mutate(!!value_col := (atan2(sin_mean, cos_mean) * 180 / pi) %% 360) %>%
#       dplyr::select(DateTime, all_of(value_col)) %>%
#       arrange(DateTime)
#    
#    return(data_hourly)
# }