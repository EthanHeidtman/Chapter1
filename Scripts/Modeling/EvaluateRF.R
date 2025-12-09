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

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
model_data <- model_data %>%
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   filter(Date > '2007-03-29') 

# Group predictors into clusters
inflow_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Inflows')))
discharge_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Discharge')))
tide_cluster <- model_data %>% dplyr::select(c('Salinity', contains('Tide')))
wind_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
#time_cluster <- model_data %>% dplyr::select(c('Salinity', contains(c('Sin', 'Cos'))))


# Function to collect the top variables from each group
get_top_vars_by_group <- function(importance_df, group_dfs, n_top = 2, 
                                  importance_col = "IncMSE_OOB",
                                  show_importance = TRUE) {
   
   # Average importance across folds
   avg_importance <- importance_df %>%
      group_by(Variable) %>%
      summarise(avg_imp = mean(.data[[importance_col]], na.rm = TRUE)) %>%
      arrange(desc(avg_imp))
   
   # Function to get top n vars from a single group
   get_top_from_group <- function(group_df, n) {
      group_vars <- setdiff(colnames(group_df), "Salinity")
      
      group_importance <- avg_importance %>%
         filter(Variable %in% group_vars) %>%
         slice_head(n = n)
      
      if (show_importance) {
         return(group_importance)
      } else {
         return(group_importance$Variable)
      }
   }
   
   # Handle different input types for n_top
   if (is.list(n_top)) {
      # User provided specific n for each group
      top_vars <- mapply(get_top_from_group, 
                         group_dfs, 
                         n_top[names(group_dfs)],
                         SIMPLIFY = FALSE)
   } else {
      # Use same n for all groups
      top_vars <- lapply(group_dfs, get_top_from_group, n = n_top)
   }
   
   names(top_vars) <- names(group_dfs)
   return(top_vars)
}

# Define the list of groups
group_list <- list(
   inflow = inflow_cluster,
   discharge = discharge_cluster,
   tide = tide_cluster,
   wind = wind_cluster
)

# Collect the top variables for each group
top_vars <- get_top_vars_by_group(
   importance_df = rf$importance,
   group_dfs = group_list,
   n_top = list(inflow = 3, discharge = 3, tide = 3, wind = 3),
   importance_col = "IncMSE_OOB",
   show_importance = TRUE
)

# Error metrics (RMSE + MAE)
p1 <- plot_error_metrics(rf$metrics)
ggsave('Outputs/Plots/RandomForest/ErrorMetrics.png', plot = p1, dpi = 600, width = 12, height = 8)

# Mean importance
p2 <- plot_mean_importance(rf$importance, top_vars)
ggsave('Outputs/Plots/RandomForest/MeanImportance.png', plot = p2, dpi = 600, width = 12, height = 8)
ggsave('Outputs/Plots/RandomForest/MeanImportance.svg', plot = p2, dpi = 600, width = 12, height = 8)


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




