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
hourly_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalHourlyDataScreened.qs'))

# Importance order: LagSalinity1, RollingDischarge48, RollingV168, TideRange48
hourly_data <- hourly_data %>%
   drop_na %>%
   dplyr::select(-contains('Inflows')) %>%
   dplyr::select(-c('LagSalinity2', 'RollingDischarge24', 'LagTide4', 'RollingWindSpeed72')) %>% # Keep only most important variable from each group
   mutate(LogRollingDischarge48 = log(RollingDischarge48)) %>%
   relocate(LogRollingDischarge48, .after = RollingDischarge48) %>%
   mutate(WindSign = factor(RollingV168 >= 0))

# Gather predictors
predictors <- colnames(hourly_data)[9 : ncol(hourly_data)]
#predictors <- predictors[!grepl(paste(c('LagSalinity2', 'RollingWindSpeed72'), collapse = '|'), predictors)]

# Make expanding CV folds
folds <- make_expanding_folds(hourly_data, initial_train_length = 5)

# =============================================================================
# Gaussian GAMs with Raw Discharge
# =============================================================================

# GAM 1: just lagged salinity
preds1 <- predictors[!grepl("Log", predictors)]
preds1 <- preds1[grepl('LagSalinity1', preds1)]
gam1 <- fit_gam(
   data = hourly_data,
   predictors = preds1,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   high_salinity_threshold = 0.15, # 70th %
   k_lagged_range = c(1, 5),
   gam_levels = 6,
   nthreads = 4,
)

# GAM 2: just raw discharge
preds2 <- predictors[!grepl("Log", predictors)]
preds2 <- preds2[grepl(paste(c('LagSalinity1', 'Discharge'), collapse = '|'), preds2)]
gam2 <- fit_gam(
   data = hourly_data,
   predictors = preds2,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   high_salinity_threshold = 0.15, # 70th %
   k_lagged_range = c(1, 5),
   k_flow_range = c(1, 50),
   gam_levels = 6,
   nthreads = 4
)

# GAM 3: raw discharge and time
preds3 <- predictors[!grepl('Log', predictors)]
preds3 <- preds3[grepl(paste(c('LagSalinity1', 'Discharge', 'Day'), collapse = '|'), preds3)]
gam3 <- fit_gam(
   data = hourly_data,
   predictors = preds3,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   high_salinity_threshold = 0.15, # 70th %
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   gam_levels = 3,
   nthreads = 4
)

# GAM 4: raw discharge, time, tide
preds4 <- predictors[!grepl('Log', predictors)]
preds4 <- preds4[grepl(paste(c('LagSalinity1', 'Discharge', 'Day', 'Tide'), collapse = '|'), preds4)]
gam4 <- fit_gam(
   data = hourly_data,
   predictors = preds4,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   high_salinity_threshold = 0.15, # 70th %
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   gam_levels = 3,
   nthreads = 4
)

# Gam 5: raw discharge, time, tide, V wind
preds5 <- predictors[!grepl('Log', predictors)]
preds5 <- preds5[grepl(paste(c('LagSalinity1', 'Discharge', 'Day', 'Tide', 'V'), collapse = '|'), preds5)]
gam5 <- fit_gam(
   data = hourly_data,
   predictors = preds5,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 6: raw discharge, time, tide, V wind, V wind by wind sign
preds6 <- predictors[!grepl('Log', predictors)]
preds6 <- preds6[grepl(paste(c('LagSalinity1', 'Discharge', 'Day', 'Tide', 'V', 'WindSign'), collapse = '|'), preds6)]
gam6 <- fit_gam(
   data = hourly_data,
   predictors = preds6,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   gam_levels  = 3,
   nthreads = 4
)


# Gam 7: raw discharge, time, tide, U wind, V wind by wind sign
gam7 <- fit_gam(
   data = hourly_data,
   predictors = preds6,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   k_interaction_range = c(3, 9),
   interactions = list(
      list(vars = c('RollingV168', 'RollingDischarge48'))
   ),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 8: raw discharge, time, tide, U wind, V wind by wind sign
gam8 <- fit_gam(
   data = hourly_data,
   predictors = preds6,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   k_interaction_range = c(3, 9),
   interactions = list(
      list(vars = c('RollingV168', 'RollingDischarge48')),
      list(vars = c('RollingV168', 'TideRange48'))
   ),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 9: raw discharge, time, tide, U wind, V wind by wind sign
gam9 <- fit_gam(
   data = hourly_data,
   predictors = preds6,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   k_interaction_range = c(3, 9),
   interactions = list(
      list(vars = c('RollingV168', 'RollingDischarge48')),
      list(vars = c('RollingV168', 'TideRange48')),
      list(vars = c('RollingDischarge48', 'TideRange48'))
   ),
   gam_levels  = 3,
   nthreads = 4
)

# Gam 10: raw discharge, time, tide, U wind, V wind by wind sign
gam10 <- fit_gam(
   data = hourly_data,
   predictors = preds6,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   k_interaction_range = c(3, 6),
   interactions = list(
      list(vars = c('RollingV168', 'RollingDischarge48')),
      list(vars = c('RollingV168', 'TideRange48')),
      list(vars = c('RollingDischarge48', 'LagTide48')),
      list(vars = c('RollingDischarge48', 'RollingV168', 'TideRange48'))
   ),
   gam_levels  = 3,
   nthreads = 4
)


# Gam no lagged salinity
preds_nosal <- preds6[!grepl('LagSalinity', preds6)]
gam_nosal <- fit_gam(
   data = hourly_data,
   predictors = preds_nosal,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_lagged_range = c(1, 3),
   k_flow_range = c(1, 50),
   k_temporal_range = c(1, 20),
   k_physical_range = c(1, 20),
   k_interaction_range = c(3, 6),
   interactions = list(
      list(vars = c('RollingV168', 'RollingDischarge48')),
      list(vars = c('RollingV168', 'TideRange48')),
      list(vars = c('RollingDischarge48', 'LagTide48')),
      list(vars = c('RollingDischarge48', 'RollingV168', 'TideRange48'))
   ),
   gam_levels  = 3,
   nthreads = 4
)

# # =============================================================================
# # Gaussian GAMs with LogDischarge 
# # =============================================================================
# 
# # GAM 11 
# preds11 <- preds11[grepl('LagSalinity1', predictors)]
# gam11 <- fit_gam(
#    data = hourly_data,
#    predictors = preds11,
#    folds = folds,
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    high_salinity_threshold = 0.15, # 70th %
#    k_lagged_range = c(1, 5),
#    gam_levels = 6,
#    nthreads = 4
# )
# 
# # GAM 12: just raw discharge
# preds12 <- predictors[grepl(paste(c('LagSalinity1', 'LogRollingDischarge'), collapse = '|'), predictors)]
# gam12 <- fit_gam(
#    data = hourly_data,
#    predictors = preds12,
#    folds = folds,
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    high_salinity_threshold = 0.15, # 70th %
#    k_lagged_range = c(1, 5),
#    k_flow_range = c(1, 50),
#    gam_levels = 6,
#    nthreads = 4
# )
# 
# # GAM 3: raw discharge and time
# preds13 <- predictors[grepl(paste(c('LagSalinity1', 'LogRollingDischarge', 'Day'), collapse = '|'), predictors)]
# gam13 <- fit_gam(
#    data = hourly_data,
#    predictors = preds13,
#    folds = folds,
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    high_salinity_threshold = 0.15, # 70th %
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    gam_levels = 3,
#    nthreads = 4
# )
# 
# # GAM 8: discharge, time, tide
# preds14 <- predictors[grepl(paste(c('LagSalinity1', 'LogRollingDischarge', 'Day', 'Tide'), collapse = '|'), predictors)]
# gam14 <- fit_gam(
#    data = hourly_data,
#    predictors = preds14,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    high_salinity_threshold = 0.15, # 70th %
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    gam_levels = 3,
#    nthreads = 4
# )
# 
# # Gam 9: discharge, time, tide, V wind
# preds15 <- predictors[grepl(paste(c('LagSalinity1', 'LogRollingDischarge', 'Day', 'Tide', 'V'), collapse = '|'), predictors)]
# gam15 <- fit_gam(
#    data = hourly_data,
#    predictors = preds15,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    gam_levels  = 3,
#    nthreads = 4
# )
# 
# # Gam 16: discharge, time, tide, V wind, U wind
# preds16 <- predictors[grepl(paste(c('LagSalinity1', 'LogRollingDischarge', 'Day', 'Tide', 'V', 'WindSign'), collapse = '|'), predictors)]
# gam16 <- fit_gam(
#    data = hourly_data,
#    predictors = preds16,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    gam_levels  = 3,
#    nthreads = 4
# )
# 
# 
# # Gam 17: discharge, time, tide, U wind, V wind by wind sign
# gam17 <- fit_gam(
#    data = hourly_data,
#    predictors = preds16,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    k_interaction_range = c(3, 9),
#    interactions = list(
#       list(vars = c('RollingV168', 'LogRollingDischarge24'))
#    ),
#    gam_levels  = 3,
#    nthreads = 4
# )
# 
# # Gam 18: discharge, time, tide, U wind, V wind by wind sign
# gam18 <- fit_gam(
#    data = hourly_data,
#    predictors = preds16,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    k_interaction_range = c(3, 9),
#    interactions = list(
#       list(vars = c('RollingV168', 'LogRollingDischarge24')),
#       list(vars = c('RollingV168', 'LagTide4'))
#    ),
#    gam_levels  = 3,
#    nthreads = 4
# )
# 
# # Gam 19: discharge, time, tide, U wind, V wind by wind sign
# gam19 <- fit_gam(
#    data = hourly_data,
#    predictors = preds16,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    k_interaction_range = c(3, 9),
#    interactions = list(
#       list(vars = c('RollingV168', 'LagRollingDischarge24')),
#       list(vars = c('RollingV168', 'LagTide4')),
#       list(vars = c('LagRollingDischarge24', 'LagTide4'))
#    ),
#    gam_levels  = 3,
#    nthreads = 4
# )
# 
# # Gam 20: discharge, time, tide, U wind, V wind by wind sign
# gam20 <- fit_gam(
#    data = hourly_data,
#    predictors = preds16,
#    folds = folds, 
#    family_type = 'gaussian',
#    transform_response = 'none',
#    link = NULL,
#    k_lagged_range = c(1, 3),
#    k_flow_range = c(1, 50),
#    k_temporal_range = c(1, 20),
#    k_physical_range = c(1, 20),
#    k_interaction_range = c(3, 6),
#    interactions = list(
#       list(vars = c('RollingV168', 'LogRollingDischarge24')),
#       list(vars = c('RollingV168', 'LagTide4')),
#       list(vars = c('LogRollingDischarge24', 'LagTide4')),
#       list(vars = c('LogRollingDischarge24', 'RollingV168', 'LagTide4'))
#    ),
#    gam_levels  = 3,
#    nthreads = 4
# )


# Write Raw Discharge Gam Files
objects <- list(gam1, gam2, gam3, gam4, gam5, gam6, gam7, gam8, gam9, gam10, gam_nosal)
file_names <- list('Gam1', 'Gam2', 'Gam3', 'Gam4', 'Gam5',      
                   'Gam6', 'Gam7', 'Gam8', 'Gam9', 'Gam10', 'GamNoSal')  
write_qs_files(objects, 'Outputs/Experiments/Models/HourlyGAM/RawDischarge', file_names)

# # Write Log Discharge Gam files
# objects <- list(gam12, gam13, gam14, gam15, gam16, gam17, gam18, gam19, gam20)
# file_names <- list('Gam12', 'Gam13', 'Gam14', 'Gam15', 'Gam16', 'Gam17', 'Gam18', 'Gam19', 'Gam20')
# write_qs_files(objects, 'Outputs/Experiments/Models/HourlyGAM/LogDischarge', file_names)


# Clear global environment
rm(list = ls())
