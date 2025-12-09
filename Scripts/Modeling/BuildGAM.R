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
ridge_linear <- read_qs_files('Outputs/Experiments/Models/Ridge.qs')
predictors <- ridge_linear$selected_vars
predictors <- setdiff(predictors, c('TideRange24', 'RollingDischarge24', 'RollingV72')) # Exclude redundant less-important ones

gam1 <- fit_gam(
   data = model_data,
   response = 'Salinity',
   predictors = predictors,
   folds = folds, 
   family_type = 'gaussian',
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 30),
   k_temporal = 12,
   k_interaction = 12,
   interactions = list(
      list(vars = c('RollingInflows90', 'RollingDischarge48')),
      list(vars = c('RollingDischarge48', 'RollingV168')),
      list(vars = c('RollingDischarge48', 'TideRange48')),
      list(vars = c('RollingV168', 'TideRange48'))
   ),
   gam_levels = 7,
   nthreads = 4,
   use_weights = FALSE
)


# Remove tide predictors
predictors <- setdiff(predictors, c('TideRange48'))

gam2 <- fit_gam(
   data = model_data,
   response = 'Salinity',
   predictors = predictors,
   folds = folds, 
   family_type = 'gaussian',
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 30),
   k_temporal = 12,
   k_interaction = 12,
   interactions = list(
      list(vars = c('RollingInflows90', 'RollingDischarge48')),
      list(vars = c('RollingDischarge48', 'RollingV168'))
   ),
   gam_levels = 7,
   nthreads = 4,
   use_weights = FALSE
)



# Remove time predictors
predictors <- setdiff(predictors, c('DayCos', 'DaySin'))

gam3 <- fit_gam(
   data = model_data,
   response = 'Salinity',
   predictors = predictors,
   folds = folds, 
   family_type = 'gaussian',
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 30),
   k_temporal = 12,
   k_interaction = 12,
   interactions = list(
      list(vars = c('RollingInflows90', 'RollingDischarge48')),
      list(vars = c('RollingDischarge48', 'RollingV168'))
   ),
   gam_levels = 7,
   nthreads = 4,
   use_weights = FALSE
)

predictors <- ridge_linear$selected_vars
predictors <- setdiff(predictors, c('RollingInflows90', 'TideRange24', 'RollingDischarge24', 'RollingV72')) # Exclude redundant less-important ones

gam4 <- fit_gam(
   data = model_data,
   response = 'Salinity',
   predictors = predictors,
   folds = folds, 
   family_type = 'gaussian',
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 30),
   k_temporal = 12,
   k_interaction = 12,
   interactions = list(
      list(vars = c('RollingDischarge48', 'RollingV168')),
      list(vars = c('RollingDischarge48', 'TideRange48')),
      list(vars = c('RollingV168', 'TideRange48'))
   ),
   gam_levels = 7,
   nthreads = 4,
   use_weights = FALSE
)


# Write output files
objects <- list(gam1, gam2, gam3, gam4)
file_names <- list('GamAllVars', 'GamNoTide', 'GamNoTideNoTime', 'GamNoInflows')
write_qs_files(objects, 'Outputs/Experiments/Models', file_names)

# Clear global environment
rm(list = ls())
