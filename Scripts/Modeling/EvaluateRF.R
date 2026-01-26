# =============================================================================
# Script Name:    EvaluateRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Uses a simple random forest to screen a dataset of ~90 predictors
#                 and identify the top variables in each grouping of variable
#                 types. Selects the best and saves a screened version of the data.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

# Source necessary functions 
dirs <- c("Scripts/Utilities", 'Scripts/Plots/RF')
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Read in Random Forest output
rf_hourly <- read_qs_files('Outputs/Experiments/Models/RFHourlyScreening.qs')
rf_daily <- read_qs_files('Outputs/Experiments/Models/RFDailyScreening.qs')

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
model_data <- model_data %>%
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   filter(Date > '2007-03-29') %>%
   dplyr::select(-contains('Inflows'))

# Build datasets
hourly_data <- model_data
daily_data <- model_data %>%
   group_by(Date) %>%
   summarise(Year      = first(Year),
             Month     = first(Month),
             Day       = first(Day),
             DayOfYear = first(DayOfYear),
             across(
                where(is.numeric),
                ~ mean(.x, na.rm = TRUE),
                .names = "{.col}"
             ),
             .groups = "drop"
   ) %>%
   select(-HourSin, -HourCos, -MonthSin, -MonthCos)


# Group predictors into clusters
salinity_cluster <- model_data %>% dplyr::select(c(contains('Salinity'))) 
discharge_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Discharge')))
tide_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Tide')))
wind_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
time_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('Sin', 'Cos'))))

# Define the list of groups
group_list <- list(
   salinity = salinity_cluster,
   discharge = discharge_cluster,
   tide = tide_cluster,
   wind = wind_cluster
)

# Collect the top variables for each group
top_vars_hourly <- get_top_vars_by_group(
   importance_df = rf_hourly$importance,
   group_dfs = group_list,
   n_top = list(salinity = 3, discharge = 3, tide = 3, wind = 3, time = 2),
   importance_col = "IncMSE_OOB",
   show_importance = TRUE
)
top_vars_daily <- get_top_vars_by_group(
   importance_df = rf_daily$importance,
   group_dfs = group_list,
   n_top = list(salinity = 3, discharge = 3, tide = 3, wind = 3, time = 2),
   importance_col = "IncMSE_OOB",
   show_importance = TRUE
)

# Error metrics (RMSE + MAE)
p1 <- plot_error_metrics(rf_hourly$metrics)
ggsave('Outputs/Plots/HourlyRF/ErrorMetrics.png', plot = p1, dpi = 600, width = 12, height = 8)

# Mean importance
p2 <- plot_mean_importance(rf_hourly$importance, top_vars_hourly)
ggsave('Outputs/Plots/HourlyRF/MeanImportance.png', plot = p2, dpi = 600, width = 12, height = 8)
#ggsave('Outputs/Plots/HourlyRF/MeanImportance.svg', plot = p2, dpi = 600, width = 12, height = 8)

# Importance heatmap
p3 <- plot_importance_heatmap(rf_hourly$importance, top_n = 20)
ggsave('Outputs/Plots/HourlyRF/ImportanceHeatmap.png', plot = p3, dpi = 600, width = 12, height = 8)

# Fold comparisons
p4 <- plot_fold_comparison(rf_hourly$importance, rf_hourly$metrics, test_years = c(2014, 2015, 2016, 2017))
ggsave('Outputs/Plots/HourlyRF/FoldComparisons.png', plot = p4, dpi = 600, width = 12, height = 8)

# Single Fold Details
p5 <- plot_single_fold_detail(rf_hourly$importance, rf_hourly$metrics, test_year = 2016, top_n = 20)
ggsave('Outputs/Plots/HourlyRF/Fold2016Test.png', plot = p5, dpi = 600, width = 12, height = 8)

# Wind Variable Trajectories
p6 <- plot_variable_group_trajectories(rf_hourly$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/HourlyRF/WindImportanceTrajectories.png', plot = p6, dpi = 600, width = 12, height = 8)

# Wind Variable Heatmap
p7 <- plot_variable_group_heatmap(rf_hourly$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/HourlyRF/WindImportanceHeatmap.png', plot = p7, dpi = 600, width = 12, height = 8)

# Variable Rank Stability
p8 <- plot_variable_rank_stability(rf_hourly$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/HourlyRF/WindRankStability.png', plot = p8, dpi = 600, width = 12, height = 8)

# Discharge Variable Heatmap
p9 <- plot_variable_group_heatmap(rf_hourly$importance, pattern = 'Discharge', pattern_name = 'Discharge Variables')
ggsave('Outputs/Plots/HourlyRF/DischargeImportanceHeatmap.png', plot = p9, dpi = 600, width = 12, height = 8)

# Tide Variable Heatmap
p10 <- plot_variable_group_heatmap(rf_hourly$importance, pattern = 'Tid', pattern_name = 'Tide Variables')
ggsave('Outputs/Plots/HourlyRF/TideImportanceHeatmap.png', plot = p10, dpi = 600, width = 12, height = 8)






# Error metrics (RMSE + MAE)
p1 <- plot_error_metrics(rf_daily$metrics)
ggsave('Outputs/Plots/DailyRF/ErrorMetrics.png', plot = p1, dpi = 600, width = 12, height = 8)

# Mean importance
p2 <- plot_mean_importance(rf_daily$importance, top_vars_daily)
ggsave('Outputs/Plots/DailyRF/MeanImportance.png', plot = p2, dpi = 600, width = 12, height = 8)
ggsave('Outputs/Plots/DailyRF/MeanImportance.svg', plot = p2, dpi = 600, width = 12, height = 8)

# Importance heatmap
p3 <- plot_importance_heatmap(rf_daily$importance, top_n = 20)
ggsave('Outputs/Plots/DailyRF/ImportanceHeatmap.png', plot = p3, dpi = 600, width = 12, height = 8)

# Fold comparisons
p4 <- plot_fold_comparison(rf_daily$importance, rf_daily$metrics, test_years = c(2014, 2015, 2016, 2017))
ggsave('Outputs/Plots/DailyRF/FoldComparisons.png', plot = p4, dpi = 600, width = 12, height = 8)

# Single Fold Details
p5 <- plot_single_fold_detail(rf_daily$importance, rf_daily$metrics, test_year = 2016, top_n = 20)
ggsave('Outputs/Plots/DailyRF/Fold2016Test.png', plot = p5, dpi = 600, width = 12, height = 8)

# Wind Variable Trajectories
p6 <- plot_variable_group_trajectories(rf_daily$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/DailyRF/WindImportanceTrajectories.png', plot = p6, dpi = 600, width = 12, height = 8)

# Wind Variable Heatmap
p7 <- plot_variable_group_heatmap(rf_daily$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/DailyRF/WindImportanceHeatmap.png', plot = p7, dpi = 600, width = 12, height = 8)

# Variable Rank Stability
p8 <- plot_variable_rank_stability(rf_daily$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/DailyRF/WindRankStability.png', plot = p8, dpi = 600, width = 12, height = 8)

# Discharge Variable Heatmap
p9 <- plot_variable_group_heatmap(rf_daily$importance, pattern = 'Discharge', pattern_name = 'Discharge Variables')
ggsave('Outputs/Plots/DailyRF/DischargeImportanceHeatmap.png', plot = p9, dpi = 600, width = 12, height = 8)

# Tide Variable Heatmap
p10 <- plot_variable_group_heatmap(rf_daily$importance, pattern = 'Tid', pattern_name = 'Tide Variables')
ggsave('Outputs/Plots/DailyRF/TideImportanceHeatmap.png', plot = p10, dpi = 600, width = 12, height = 8)

# Clear global environment
rm(list = ls())




