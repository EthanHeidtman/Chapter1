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
   drop_na %>%
   dplyr::select(-contains('Inflows')) 

# Make expanding CV folds
folds <- make_expanding_folds(model_data, initial_train_length = 5)

# # Read in ridge output and get best predictors
# ridge_linear <- read_qs_files('Outputs/Experiments/Models/Ridge.qs')
# predictors <- ridge_linear$selected_vars
# predictors <- setdiff(predictors, c('TideRange24', 'RollingV72')) # Exclude redundant less-important ones

# Log transform discharge data
model_data <- model_data %>% 
   mutate(LogRollingDischarge24 = log(RollingDischarge24)) %>%
   relocate(LogRollingDischarge24, .after = RollingDischarge24) %>%
   dplyr::select(-c('RollingDischarge48', 'TideRange24'))

predictors <- colnames(model_data)[9 : ncol(model_data)]

# =============================================================================
# Gamma GAMs with Raw Discharge
# =============================================================================

# GAM 1: Gamma, just raw discharge
preds1 <- predictors[!grepl("Log", predictors)]
preds1 <- preds1[grepl('Discharge', preds1)]
gam1 <- fit_gam(
   data = model_data,
   predictors = preds1,
   folds = folds,
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   gam_levels = 3,
   nthreads = 4
)

# GAM 2: Gamma, raw discharge and time
preds2 <- predictors[!grepl('Log', predictors)]
preds2 <- preds2[grepl(paste(c('Discharge', 'Day'), collapse = '|'), preds2)]
gam2 <- fit_gam(
   data = model_data,
   predictors = preds2,
   folds = folds,
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# GAM 3: Gamma, raw discharge, time, tide
preds3 <- predictors[!grepl('Log', predictors)]
preds3 <- preds3[grepl(paste(c('Discharge', 'Day', 'Tide'), collapse = '|'), preds3)]
gam3 <- fit_gam(
   data = model_data,
   predictors = preds3,
   folds = folds, 
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# Gam 4: Gamma, raw discharge, time, tide, V wind
preds4 <- predictors[!grepl('Log', predictors)]
preds4 <- preds4[grepl(paste(c('Discharge', 'Day', 'Tide', 'V'), collapse = '|'), preds4)]
gam4 <- fit_gam(
   data = model_data,
   predictors = preds4,
   folds = folds, 
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 5: Gamma, raw discharge, time, tide, V wind, U wind
preds5 <- predictors[!grepl('Log', predictors)]
preds5 <- preds5[grepl(paste(c('Discharge', 'Day', 'Tide', 'V', 'U'), collapse = '|'), preds5)]
gam5 <- fit_gam(
   data = model_data,
   predictors = preds5,
   folds = folds, 
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels  = 3,
   nthreads = 4
)

# =============================================================================
# Gamma GAMs with LogDischarge 
# =============================================================================

# GAM 6: Gamma, just Log discharge
preds6 <- predictors[grepl('LogRollingDischarge', predictors)]
gam6 <- fit_gam(
   data = model_data,
   predictors = preds6,
   folds = folds,
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   gam_levels = 3,
   nthreads = 4
)

# GAM 7: Gamma, raw discharge and time
preds7 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day'), collapse = '|'), predictors)]
gam7 <- fit_gam(
   data = model_data,
   predictors = preds7,
   folds = folds,
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# GAM 8: Gamma, raw discharge, time, tide
preds8 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tide'), collapse = '|'), predictors)]
gam8 <- fit_gam(
   data = model_data,
   predictors = preds8,
   folds = folds, 
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# Gam 9: Gamma, raw discharge, time, tide, V wind
preds9 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tide', 'V'), collapse = '|'), predictors)]
gam9 <- fit_gam(
   data = model_data,
   predictors = preds9,
   folds = folds, 
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 10: Gamma, raw discharge, time, tide, V wind, U wind
preds10 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tide', 'V', 'U'), collapse = '|'), predictors)]
gam10 <- fit_gam(
   data = model_data,
   predictors = preds10,
   folds = folds, 
   family_type = 'Gamma',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 40),
   k_temporal_range = c(5, 40),
   gam_levels  = 10,
   nthreads = 4
)

# =============================================================================
# Gaussian GAMs with Raw Discharge 
# =============================================================================

# GAM 11: Gaussian, just discharge
preds11 <- predictors[!grepl('Log', predictors)]
preds11 <- preds11[grepl(paste(c('Discharge'), collapse = '|'), preds11)]
gam11 <- fit_gam(
   data = model_data,
   predictors = preds11,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   gam_levels = 3,
   nthreads = 4
)

# GAM 12: gaussian, raw discharge and time
preds12 <- predictors[!grepl('Log', predictors)]
preds12 <- preds12[grepl(paste(c('Discharge', 'Day'), collapse = '|'), preds12)]
gam12 <- fit_gam(
   data = model_data,
   predictors = preds12,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# GAM 13: Gaussian, raw discharge, time, tide
preds13 <- predictors[!grepl('Log', predictors)]
preds13 <- preds13[grepl(paste(c('Discharge', 'Day', 'Tide'), collapse = '|'), preds13)]
gam13 <- fit_gam(
   data = model_data,
   predictors = preds13,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# Gam 14: Gaussian, raw discharge, time, tide, V wind
preds14 <- predictors[!grepl('Log', predictors)]
preds14 <- preds14[grepl(paste(c('Discharge', 'Day', 'Tide', 'V'), collapse = '|'), preds14)]
gam14 <- fit_gam(
   data = model_data,
   predictors = preds14,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 15: Gaussian, raw discharge, time, tide, V wind, U wind
preds15 <- predictors[!grepl('Log', predictors)]
preds15 <- preds15[grepl(paste(c('Discharge', 'Day', 'Tide', 'V', 'U'), collapse = '|'), preds15)]
gam15 <- fit_gam(
   data = model_data,
   predictors = preds15,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 40),
   k_temporal_range = c(5, 40),
   gam_levels  = 3,
   nthreads = 4
)


# =============================================================================
# Gaussian GAMs with Log Discharge 
# =============================================================================

# GAM 16: Gaussian, just log discharge
preds16 <- predictors[grepl(paste(c('LogRollingDischarge'), collapse = '|'), predictors)]
gam16 <- fit_gam(
   data = model_data,
   predictors = preds16,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   gam_levels = 3,
   nthreads = 4
)

# GAM 17: gaussian, raw discharge and time
preds17 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day'), collapse = '|'), predictors)]
gam17 <- fit_gam(
   data = model_data,
   predictors = preds17,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# GAM 18: Gaussian, raw discharge, time, tide
preds18 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tide'), collapse = '|'), predictors)]
gam18 <- fit_gam(
   data = model_data,
   predictors = preds18,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels = 3,
   nthreads = 4
)

# Gam 19: Gaussian, raw discharge, time, tide, V wind
preds19 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tide', 'V'), collapse = '|'), predictors)]
gam19 <- fit_gam(
   data = model_data,
   predictors = preds19,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 15),
   k_temporal_range = c(5, 15),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 20: Gaussian, raw discharge, time, tide, V wind, U wind
preds20 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tide', 'V', 'U'), collapse = '|'), predictors)]
gam20 <- fit_gam(
   data = model_data,
   predictors = preds20,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 40),
   k_physical_range = c(5, 40),
   k_temporal_range = c(5, 40),
   gam_levels  = 3,
   nthreads = 4
)



# # Gam 6: Gamma, raw discharge, time, tide, V wind, U wind, INTERACTIONS
# preds6 <- predictors[!grepl('Log', predictors)]
# preds6 <- preds6[grepl(paste(c('Discharge', 'Day', 'Tide', 'V', 'U'), collapse = '|'), preds6)]
# gam6 <- fit_gam(
#    data = model_data,
#    predictors = preds6,
#    folds = folds, 
#    family_type = 'Gamma',
#    transform_response = 'none',
#    link = NULL,
#    k_flow_range = c(10, 40),
#    k_physical_range = c(5, 15),
#    k_temporal_range = c(5, 15),
#    k_interaction_range = c(5, 15),
#    interactions = list(
#       list(vars = c('RollingDischarge24', 'TideRange48')),
#       list(vars = c('RollingDischarge24', 'RollingV168')),
#       list(vars = c('RollingDischarge24', 'RollingU168')),
#       list(vars = c('TideRange48', 'RollingU168')),
#       list(vars = c('TideRange48', 'RollingV168'))
#    ),
#    gam_levels  = 3,
#    nthreads = 4
# )
# 

objects <- list(gam11, gam12, gam13, gam14, gam15, gam16, gam17, gam18, gam19, gam20)
file_names <- list('Gam11', 'Gam12', 'Gam13', 'Gam14', 'Gam15', 'Gam16', 'Gam17', 'Gam18', 'Gam19', 'Gam20')



# Write output files
objects <- list(gam1, gam2, gam3, gam4, gam5, gam6, gam7, gam8, gam9, gam10, gam11, gam12, gam13, gam14, gam15, gam16, gam17, gam18, gam19, gam20)
file_names <- list('Gam1', 'Gam2', 'Gam3', 'Gam4', 'Gam5',       # Gamma, raw discharge
                   'Gam6', 'Gam7', 'Gam8', 'Gam9', 'Gam10',      # Gamma, log discharge
                   'Gam11', 'Gam12', 'Gam13', 'Gam14', 'Gam15',  # Gaussian, raw discharge,
                   'Gam16', 'Gam17', 'Gam18', 'Gam19', 'Gam20')  # Gaussian, log discharge
write_qs_files(objects, 'Outputs/Experiments/Models', file_names)

# Clear global environment
rm(list = ls())
