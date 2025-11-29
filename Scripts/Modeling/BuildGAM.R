# =============================================================================
# Script Name:    BuildGAM.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-11-25
# Last Updated:   2025-11-25
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(tidyverse)
library(tidymodels)
library(mgcv)
library(dplyr)
library(purrr)

# Source necessary functions
source('Scripts/Utilities/GamUtilities.R')
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelDataScreened.qs'))
model_data <- model_data %>%
   drop_na

# Make expanding CV folds
folds <- make_expanding_folds(model_data, initial_train_length = 5)

# Read in ridge output and get best predictors
ridge_linear <- read_qs_files('Outputs/Experiments/Models/RidgeLinearModel.qs')
predictors <- ridge_linear$selected_vars
predictors <- setdiff(predictors, c('RollingV72', 'TideRange48', 'RollingDischarge24')) # Exclude less-important ones

# Fit GAM
gam <- fit_gam(
   data = model_data,
   response = 'Salinity',
   predictors = predictors,
   folds = folds,
   k_flow_range = c(15, 30),
   k_physical_range = c(8, 25),
   k_temporal = 12,
   k_interaction = 10,
   interactions = list(
      list(vars = c('RollingInflows90', 'RollingDischarge48')),
      list(vars = c('RollingDischarge48', 'RollingV168')),
      list(vars = c('RollingDischarge48', 'TideRange24')),
      list(vars = c('TideRange24', 'RollingV168'))
   ),
   gam_levels = 4,
   nthreads = 4,
   use_weights = TRUE,
   weight_type = "quadratic",
   weight_threshold = 0.4
)


# Write output files
objects <- list(gam)
file_names <- list('GamModel')
write_qs_files(objects, 'Outputs/Experiments/Models', file_names)

# Clear global environment
rm(list = ls())
