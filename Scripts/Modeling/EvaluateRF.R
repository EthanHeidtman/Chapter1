# =============================================================================
# Script Name:    EvaluateRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-11-25
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
rf <- read_qs_files('Outputs/Experiments/Models/RFScreening.qs')

# Error metrics (RMSE + MAE)
p1 <- plot_error_metrics(rf$metrics)
ggsave('Outputs/Plots/RandomForest/ErrorMetrics.png', plot = p1, dpi = 600, width = 12, height = 8)

# Mean importance
p2 <- plot_mean_importance(rf$importance, top_n = 20)
ggsave('Outputs/Plots/RandomForest/MeanImportance.png', plot = p2, dpi = 600, width = 12, height = 8)

# Importance heatmap
p3 <- plot_importance_heatmap(rf$importance, top_n = 20)
ggsave('Outputs/Plots/RandomForest/ImportanceHeatmap.png', plot = p3, dpi = 600, width = 12, height = 8)

# Fold comparisons
p4 <- plot_fold_comparison(rf$importance, rf$metrics, test_years = c(2014, 2015, 2016, 2017))
ggsave('Outputs/Plots/RandomForest/FoldComparisons.png', plot = p4, dpi = 600, width = 12, height = 8)

# Single Fold Details
p5 <- plot_single_fold_detail(rf$importance, rf$metrics, test_year = 2016, top_n = 20)
ggsave('Outputs/Plots/RandomForest/Fold2016Test.png', plot = p5, dpi = 600, width = 12, height = 8)

# Wind Variable Trajectories
p6 <- plot_variable_group_trajectories(rf$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/RandomForest/WindImportanceTrajectories.png', plot = p6, dpi = 600, width = 12, height = 8)

# Wind Variable Heatmap
p7 <- plot_variable_group_heatmap(rf$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/RandomForest/WindImportanceHeatmap.png', plot = p7, dpi = 600, width = 12, height = 8)

# Variable Rank Stability
p8 <- plot_variable_rank_stability(rf$importance, pattern = 'U|V', pattern_name = 'Wind Variables')
ggsave('Outputs/Plots/RandomForest/WindRankStability.png', plot = p8, dpi = 600, width = 12, height = 8)

# Inflow Variable Heatmap
p9 <- plot_variable_group_heatmap(rf$importance, pattern = 'Inflows', pattern_name = 'Inflow Variables')
ggsave('Outputs/Plots/RandomForest/InflowImportanceHeatmap.png', plot = p9, dpi = 600, width = 12, height = 8)

# Discharge Variable Heatmap
p10 <- plot_variable_group_heatmap(rf$importance, pattern = 'Discharge', pattern_name = 'Discharge Variables')
ggsave('Outputs/Plots/RandomForest/DischargeImportanceHeatmap.png', plot = p10, dpi = 600, width = 12, height = 8)

# Tide Variable Heatmap
p11 <- plot_variable_group_heatmap(rf$importance, pattern = 'Tid', pattern_name = 'Tide Variables')
ggsave('Outputs/Plots/RandomForest/TideImportanceHeatmap.png', plot = p11, dpi = 600, width = 12, height = 8)

# Clear global environment
rm(list = ls())




