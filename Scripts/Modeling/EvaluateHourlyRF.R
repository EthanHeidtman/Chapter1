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
source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Plots/RF/RFEvalPlots.R')

# Define lead times that were run
lead_times <- c(0, 1, 2, 4, 6, 8, 10, 12, 24, 36, 48, 72, 168, 336, 504)

# Initialize lists to store results
rf_results <- list()
screened_data <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/RF/RFHourlyScreening_lag', k, '.qs')
   )
   
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/FinalHourlyDataScreened_lag', k, '.qs')
   )
}

# Initialize storage
top_vars_by_k <- list()

# Loop through each lead time
for (k in lead_times) {
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k to define groups
   model_data_k <- screened_data[[lag_name]]
   
   # Define groups for this specific k
   salinity_cluster_k <- model_data_k %>% dplyr::select(contains('Salinity'))
   discharge_cluster_k <- model_data_k %>% dplyr::select(c('Salinity', contains('Discharge')))
   tide_cluster_k <- model_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster_k <- model_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   
   group_list_k <- list(
      salinity = salinity_cluster_k,
      discharge = discharge_cluster_k,
      tide = tide_cluster_k,
      wind = wind_cluster_k
   )
   
   # Get top variables using your existing function
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df = rf_results[[lag_name]]$importance,
      group_dfs = group_list_k,
      n_top = list(salinity = 3, discharge = 3, tide = 3, wind = 3),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
}

# ==============================================================================
# Calculate average importance by group for each k
# ==============================================================================

group_importance_by_k <- data.frame()

for (k in lead_times) {
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k to define groups
   model_data_k <- screened_data[[lag_name]]
   
   # Define groups for this specific k
   group_list_k <- list(
      Salinity = model_data_k %>% dplyr::select(contains('Salinity')),
      Discharge = model_data_k %>% dplyr::select(c('Salinity', contains('Discharge'))),
      Tide = model_data_k %>% dplyr::select(c('Salinity', contains('Tide'))),
      Wind = model_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   )
   
   # Average importance across folds
   avg_importance <- rf_results[[lag_name]]$importance %>%
      group_by(Variable) %>%
      summarise(avg_imp = mean(IncMSE_OOB, na.rm = TRUE), .groups = 'drop')
   
   # Calculate mean importance for each group
   for (grp_name in names(group_list_k)) {
      group_vars <- setdiff(colnames(group_list_k[[grp_name]]), "Salinity")
      
      group_mean_imp <- avg_importance %>%
         filter(Variable %in% group_vars) %>%
         summarise(mean_importance = mean(avg_imp, na.rm = TRUE)) %>%
         pull(mean_importance)
      
      group_importance_by_k <- rbind(
         group_importance_by_k,
         data.frame(
            LeadTime = k,
            Group = grp_name,
            MeanImportance = group_mean_imp
         )
      )
   }
}

# Calculate relative importance (proportion of total at each k)
group_importance_by_k <- group_importance_by_k %>%
   group_by(LeadTime) %>%
   mutate(
      TotalImportance = sum(MeanImportance),
      RelativeImportance = MeanImportance / TotalImportance
   ) %>%
   ungroup()



# Usage:
p1 <- plot_relative_importance(group_importance_by_k)
ggsave('Outputs/Plots/HourlyRF/RelativeVarImpAcrossK.png', plot = p1, dpi = 600, width = 12, height = 8)

p2 <- plot_absolute_importance(group_importance_by_k)
ggsave('Outputs/Plots/HourlyRF/AbsoluteVarImpAcrossK.png', plot = p2, dpi = 600, width = 12, height = 8)





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







# Clear global environment
rm(list = ls())




