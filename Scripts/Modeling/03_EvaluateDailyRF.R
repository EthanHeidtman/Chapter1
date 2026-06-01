# =============================================================================
# Script Name:    03_EvaluateDailyRF_final.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Evaluates RF screening results across all lead times.
#                 Plots variable importance by group, heatmaps, and relative
#                 importance across forecast horizons.
#
#                 Changes from original (v1):
#                 - Cluster definitions updated to match revised predictor
#                   engineering: sustained_discharge replaces rolling_discharge,
#                   ExpDecayDischarge added to sustained flow cluster,
#                   wind cluster updated to RollingU_along/RollingU_cross,
#                   LagDischarge/LagU/LagV/LagTide references removed.
#                 - ExceedFlux_vol excluded from flushing cluster (consistent
#                   with RF screening script).
#                 - Wind heatmap pattern updated from 'U|V' to 'U_along|U_cross'.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Plots/RF/RFEvalPlots.R')

# =============================================================================
# PARAMETERS
# =============================================================================

lead_times <- seq(0, 30, 1)

# =============================================================================
# LOAD RESULTS
# =============================================================================

rf_results   <- list()
screened_data <- list()

for (k in lead_times) {
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
}

# =============================================================================
# HELPER: define clusters for a given screened dataset
# Centralised here so cluster definitions are consistent across all loops.
# When transferring to a new system, only this function needs updating if
# predictor naming changes.
# =============================================================================

define_clusters <- function(model_data_k) {
   list(
      Salinity = model_data_k %>%
         dplyr::select(contains('Salinity')),
      
      SustainedDischarge = model_data_k %>%
         dplyr::select(Salinity,
                       contains(c('RollingDischarge', 'RollingAnomaly'))),
      
      FlushingDischarge = model_data_k %>%
         dplyr::select(Salinity,
                       contains(c('MaxDischarge', 'ExceedFlux'))),
      
      Tide = model_data_k %>%
         dplyr::select(Salinity,
                       contains(c('TideRange', 'TideMean'))),   
      
      Wind = model_data_k %>%
         dplyr::select(Salinity,
                       contains(c('RollingWindAlong', 'RollingWindCross')))
      
   )
}

# =============================================================================
# GET TOP VARIABLES PER GROUP ACROSS ALL LEAD TIMES
# =============================================================================

top_vars_by_k <- list()

for (k in lead_times) {
   lag_name     <- paste0("lag", k)
   model_data_k <- screened_data[[lag_name]]
   group_list_k <- define_clusters(model_data_k)
   
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df  = rf_results[[lag_name]]$importance,
      group_dfs      = group_list_k,
      n_top = 4,
      importance_col  = "IncMSE_OOB",
      show_importance = TRUE
   )
}

# =============================================================================
# GROUP IMPORTANCE ACROSS LEAD TIMES
# =============================================================================

group_importance_by_k <- data.frame()

for (k in lead_times) {
   lag_name     <- paste0("lag", k)
   model_data_k <- screened_data[[lag_name]]
   group_list_k <- define_clusters(model_data_k)
   
   avg_importance <- rf_results[[lag_name]]$importance %>%
      group_by(Variable) %>%
      summarise(avg_imp = mean(IncMSE_OOB, na.rm = TRUE), .groups = 'drop')
   
   for (grp_name in names(group_list_k)) {
      group_vars     <- setdiff(colnames(group_list_k[[grp_name]]), "Salinity")
      group_mean_imp <- avg_importance %>%
         filter(Variable %in% group_vars) %>%
         summarise(mean_importance = mean(avg_imp, na.rm = TRUE)) %>%
         pull(mean_importance)
      
      group_importance_by_k <- rbind(
         group_importance_by_k,
         data.frame(LeadTime       = k,
                    Group          = grp_name,
                    MeanImportance = group_mean_imp)
      )
   }
}

group_importance_by_k <- group_importance_by_k %>%
   group_by(LeadTime) %>%
   mutate(TotalImportance    = sum(MeanImportance),
          RelativeImportance = MeanImportance / TotalImportance) %>%
   ungroup()

# =============================================================================
# PLOTS PER LEAD TIME
# =============================================================================

base_dir <- "Outputs/Plots/DailyRF"
if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

for (k in lead_times) {
   
   lag_name     <- paste0("lag", k)
   rf_result_k  <- rf_results[[lag_name]]
   top_vars_k   <- top_vars_by_k[[lag_name]]
   
   lag_dir <- file.path(base_dir, lag_name)
   if (!dir.exists(lag_dir)) dir.create(lag_dir, recursive = TRUE)
   
   # 1. Error metrics
   p_error <- plot_error_metrics(rf_result_k$metrics)
   ggsave(file.path(lag_dir, 'error.png'),
          plot = p_error, width = 8, height = 6, dpi = 600)
   
   # 2. Mean importance by group
   p_mean_imp <- plot_mean_importance(rf_result_k$importance, top_vars_k)
   ggsave(file.path(lag_dir, 'mean_imp.png'),
          plot = p_mean_imp, width = 8, height = 6, dpi = 600)
   
   # 3. Overall importance heatmap (top 20 variables)
   p_imp_heat <- plot_importance_heatmap(rf_result_k$importance, top_n = 20)
   ggsave(file.path(lag_dir, 'imp_heatmap.png'),
          plot = p_imp_heat, width = 8, height = 6, dpi = 600)
   
   # # 4. Wind variable heatmap
   # # Pattern matches RollingU_along and RollingU_cross families
   # p_wind_heat <- plot_variable_group_heatmap(
   #    rf_result_k$importance,
   #    pattern      = 'U_along|U_cross',
   #    pattern_name = 'Wind Variables'
   # )
   # ggsave(file.path(lag_dir, 'wind_heatmap.png'),
   #        plot = p_wind_heat, width = 10, height = 8, dpi = 600)
   # 
   # # 5. Discharge variable heatmap (all discharge families)
   # p_discharge_heat <- plot_variable_group_heatmap(
   #    rf_result_k$importance,
   #    pattern      = 'Discharge|ExceedFlux',
   #    pattern_name = 'Discharge Variables'
   # )
   # ggsave(file.path(lag_dir, 'discharge_heatmap.png'),
   #        plot = p_discharge_heat, width = 10, height = 8, dpi = 600)
   # 
   # # 6. Tide variable heatmap
   # p_tide_heat <- plot_variable_group_heatmap(
   #    rf_result_k$importance,
   #    pattern      = 'TideRange',
   #    pattern_name = 'Tide Variables'
   # )
   # ggsave(file.path(lag_dir, 'tide_heatmap.png'),
   #        plot = p_tide_heat, width = 10, height = 8, dpi = 600)
   
   cat("Saved plots for", lag_name, "to", lag_dir, "\n")
}

# =============================================================================
# SUMMARY PLOTS ACROSS ALL LEAD TIMES
# =============================================================================

p1 <- plot_relative_importance(group_importance_by_k, x_label = 'Lead Time (days)')
ggsave('Outputs/Plots/DailyRF/RelativeVarImpAcrossK.png',
       plot = p1, dpi = 600, width = 12, height = 8)
ggsave('Outputs/Plots/DailyRF/RelativeVarImpAcrossK.svg',
       plot = p1, dpi = 600, width = 12, height = 8)

p2 <- plot_absolute_importance(group_importance_by_k, x_label = 'Lead Time (days)')
ggsave('Outputs/Plots/DailyRF/AbsoluteVarImpAcrossK.png',
       plot = p2, dpi = 600, width = 12, height = 8)

rm(list = ls())