# =============================================================================
# Script Name:    03_EvaluateDailyRF.R
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
lead_times <- seq(0, 30, 1)

# Initialize lists to store results
rf_results <- list()
screened_data <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
}


# Initialize storage
top_vars_by_k <- list()

# Get top variables for each lead time
for (k in lead_times) {
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k to define groups
   model_data_k <- screened_data[[lag_name]]
   
   # Define groups for this specific k
   salinity_cluster <- model_data_k %>% dplyr::select(c(contains('Salinity')))
   rolling_discharge_cluster <- model_data_k %>% dplyr::select(c('Salinity', contains(c('RollingDischarge', 'LagDischarge'))))
   flushing_discharge_cluster <- model_data_k %>% dplyr::select(c('Salinity', contains(c('ExceedFlux', 'Flush', 'MaxDischarge'))))
   tide_cluster <- model_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster <- model_data_k %>% dplyr::select(c('Salinity', contains(c('RollingU', 'RollingV', 'Gust', 'Wind', 'LagU', 'LagV'))))
   
   group_list_k <- list(
      salinity = salinity_cluster,
      rolling_discharge = rolling_discharge_cluster,
      flushing_discharge = flushing_discharge_cluster,
      tide = tide_cluster,
      wind = wind_cluster
   )
   
   # Get top variables 
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df = rf_results[[lag_name]]$importance,
      group_dfs = group_list_k,
      n_top = list(salinity = 4, 
                   rolling_discharge = 4, 
                   flushing_discharge = 4, 
                   tide = 4, 
                   wind = 4),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
}


# Calculate average importance for each k
group_importance_by_k <- data.frame()
for (k in lead_times) {
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k to define groups
   model_data_k <- screened_data[[lag_name]]
   
   # Define groups for this specific k
   group_list_k <- list(
      Salinity = model_data_k %>% dplyr::select(contains('Salinity')),
      RollingDischarge = model_data_k %>% dplyr::select(c('Salinity', contains(c('RollingDischarge', 'LagDischarge')))),
      FlushingDischarge = model_data_k %>% dplyr::select(c('Salinity', contains(c('ExceedFlux', 'Flush', 'MaxDischarge')))),
      Tide = model_data_k %>% dplyr::select(c('Salinity', contains('Tide'))),
      Wind = model_data_k %>% dplyr::select(c('Salinity', contains(c('RollingU', 'RollingV', 'Gust', 'Wind', 'LagU', 'LagV'))))
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

# Create base directory structure
base_dir <- "Outputs/Plots/DailyRF"
if (!dir.exists(base_dir)) {
   dir.create(base_dir, recursive = TRUE)
}

# Plot GAM validation for each model
for (k in lead_times) {
   
   lag_name <- paste0("lag", k)
   
   # Get the rf_result for this k
   rf_result_k <- rf_results[[lag_name]]
   
   # Get the top variables for this k
   top_vars_k <- top_vars_by_k[[lag_name]]
   
   # Create lag-specific directory
   lag_dir <- file.path(base_dir, lag_name)
   if (!dir.exists(lag_dir)) {
      dir.create(lag_dir, recursive = TRUE)
   }
   
   # Create and save plots in specified order
   
   # 1. Error Metrics
   p_error <- plot_error_metrics(rf_result_k$metrics)
   ggsave(filename = file.path(lag_dir, 'error.png'),
          plot = p_error, width = 8, height = 6, dpi = 600)
   
   # 2. Mean Importance
   p_mean_imp <- plot_mean_importance(rf_result_k$importance, top_vars_k)
   ggsave(filename = file.path(lag_dir, 'mean_imp.png'),
          plot = p_mean_imp, width = 8, height = 6, dpi = 600)
   
   # 3. Importance Heatmap
   p_imp_heat <- plot_importance_heatmap(rf_result_k$importance, top_n = 20)
   ggsave(filename = file.path(lag_dir, 'imp_heatmap.png'),
          plot = p_imp_heat, width = 8, height = 6, dpi = 600)
   
   # 4. Wind Variable Heatmap
   p_wind_heat <- plot_variable_group_heatmap(rf_result_k$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
   ggsave(filename = file.path(lag_dir, 'wind_heatmap.png'),
          plot = p_wind_heat, width = 10, height = 8, dpi = 600)
   
   # 5. Discharge Variable Heatmap
   p_discharge_heat <- plot_variable_group_heatmap(rf_result_k$importance, pattern = 'Discharge', pattern_name = 'Discharge Variables')
   ggsave(filename = file.path(lag_dir, 'discharge_heatmap.png'),
          plot = p_discharge_heat, width = 10, height = 8, dpi = 600)
   
   # 6. Tide Variable Heatmap
   p_tide_heat <- plot_variable_group_heatmap(rf_result_k$importance, pattern = 'Tid', pattern_name = 'Tide Variables')
   ggsave(filename = file.path(lag_dir, 'tide_heatmap.png'),
          plot = p_tide_heat, width = 10, height = 8, dpi = 600)

   
   cat("Saved plots for", lag_name, "to", lag_dir, "\n")
}


p1 <- plot_relative_importance(group_importance_by_k, x_label = 'Lead Time (days)')
ggsave('Outputs/Plots/DailyRF/RelativeVarImpAcrossK.png', plot = p1, dpi = 600, width = 12, height = 8)
ggsave('Outputs/Plots/DailyRF/RelativeVarImpAcrossK.svg', plot = p1, dpi = 600, width = 12, height = 8)

p2 <- plot_absolute_importance(group_importance_by_k, x_label = 'Lead Time (days)')
ggsave('Outputs/Plots/DailyRF/AbsoluteVarImpAcrossK.png', plot = p2, dpi = 600, width = 12, height = 8)

# Clear global environment
rm(list = ls())
