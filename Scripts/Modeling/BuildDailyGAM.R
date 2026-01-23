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
daily_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalDailyDataScreened.qs'))
daily_data <- daily_data %>%
   drop_na %>%
   mutate_if(is.numeric, round, digits = 3)

# Make expanding CV folds
folds <- make_expanding_folds(daily_data, date_col = 'Date', initial_train_length = 5)

# Log transform discharge data
daily_data <- daily_data %>% 
   mutate(LogRollingDischarge48 = log(RollingDischarge48)) %>%
   relocate(LogRollingDischarge48, .after = RollingDischarge48) %>%
   dplyr::select(-c('LagDischarge3', 'TideRange6', 'Gust')) %>%
   mutate(WindSign = factor(RollingV168 >= 0)) %>%
   rename(DateTime = Date)

predictors <- colnames(daily_data)[8 : ncol(daily_data)]

# Create a reference GAM to estimate rho and build autocorrelation in
preds_ref <- predictors[!grepl('Log', predictors)]
gam_ref <- fit_gam(
   data = daily_data,
   predictors = preds_ref,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingV168', 'RollingDischarge48')),
      list(vars = c('RollingV168', 'TideRange3')),
      list(vars = c('RollingV168', 'RollingDischarge48', 'TideRange3'))
   ),
   gam_levels  = 3,
   nthreads = 4
)

# Calculate AR.start for autoregressive terms
ar_start <- create_ar_start(daily_data$DateTime, max_gap_days = 2)

# Calculate initial rho
rho_initial <- calculate_rho_from_residuals(gam_ref$gam_object, ar_start)

# =============================================================================
# Gaussian GAMs with Raw Discharge
# =============================================================================

# GAM 1: just raw discharge
preds1 <- predictors[!grepl("Log", predictors)]
preds1 <- preds1[grepl('Discharge', preds1)]
gam1 <- fit_gam(
   data = daily_data,
   predictors = preds1,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   gam_levels  = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 2: Gamma, raw discharge and time
preds2 <- predictors[!grepl('Log', predictors)]
preds2 <- preds2[grepl(paste(c('Discharge', 'Day'), collapse = '|'), preds2)]
gam2 <- fit_gam(
   data = daily_data,
   predictors = preds2,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 3: Gamma, raw discharge, time, tide
preds3 <- predictors[!grepl('Log', predictors)]
preds3 <- preds3[grepl(paste(c('Discharge', 'Day', 'Tide'), collapse = '|'), preds3)]
gam3 <- fit_gam(
   data = daily_data,
   predictors = preds3,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# Gam 4: Gamma, raw discharge, time, tide, V wind
preds4 <- predictors[!grepl('Log', predictors)]
preds4 <- preds4[grepl(paste(c('Discharge', 'Day', 'Tide', 'V'), collapse = '|'), preds4)]
gam4 <- fit_gam(
   data = daily_data,
   predictors = preds4,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels  = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 5: Gamma, raw discharge, time, tide, V wind by wind sign
preds5 <- predictors[!grepl('Log', predictors)]
preds5 <- preds5[grepl(paste(c('Discharge', 'Day', 'Tide', 'V', 'WindSign'), collapse = '|'), preds5)]
gam5 <- fit_gam(
   data = daily_data,
   predictors = preds5,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels  = 5,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 6: Gamma, raw discharge, time, tide, V wind by wind sign
gam6 <- fit_gam(
   data = daily_data,
   predictors = preds5,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingDischarge48', 'TideRange3'))
   ),
   gam_levels  = 5,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 7: Gamma, raw discharge, time, tide, V wind by wind sign
gam7 <- fit_gam(
   data = daily_data,
   predictors = preds5,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingDischarge48', 'TideRange3')),
      list(vars = c('TideRange3', 'RollingV168'))
   ),
   gam_levels  = 3,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 8: Gamma, raw discharge, time, tide, V wind by wind sign
gam8 <- fit_gam(
   data = daily_data,
   predictors = preds5,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingDischarge48', 'TideRange3')),
      list(vars = c('TideRange3', 'RollingV168')),
      list(vars = c('RollingDischarge48', 'TideRange3', 'RollingV168'))
   ),
   gam_levels  = 3,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# =============================================================================
# Gaussian GAMs with Log Discharge
# =============================================================================

# GAM 1: just raw discharge
preds9 <- predictors[grepl(paste(c('LogRollingDischarge'), collapse = '|'), predictors)]
gam9 <- fit_gam(
   data = daily_data,
   predictors = preds9,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   gam_levels  = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 2: Gamma, raw discharge and time
preds10 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day'), collapse = '|'), predictors)]
gam10 <- fit_gam(
   data = daily_data,
   predictors = preds10,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 3: Gamma, raw discharge, time, tide
preds11 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tid'), collapse = '|'), predictors)]
gam11 <- fit_gam(
   data = daily_data,
   predictors = preds11,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels = 10,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# Gam 4: Gamma, raw discharge, time, tide, V wind
preds12 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tid', 'V'), collapse = '|'), predictors)]
gam12 <- fit_gam(
   data = daily_data,
   predictors = preds12,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels = 4,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 5: Gamma, raw discharge, time, tide, V wind by wind sign
preds13 <- predictors[grepl(paste(c('LogRollingDischarge', 'Day', 'Tid', 'V', 'WindSign'), collapse = '|'), predictors)]
gam13 <- fit_gam(
   data = daily_data,
   predictors = preds13,
   folds = folds,
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   gam_levels = 4,
   nthreads = 4,
   use_ar1 = TRUE,
   rho = 0.95,
   ar_start = ar_start
)

# GAM 6: Gamma, raw discharge, time, tide, V wind by wind sign
gam14 <- fit_gam(
   data = daily_data,
   predictors = preds13,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingDischarge48', 'TideRange3'))
   ),
   gam_levels  = 4,
   nthreads = 4
)

# GAM 7: Gamma, raw discharge, time, tide, V wind by wind sign
gam15 <- fit_gam(
   data = daily_data,
   predictors = preds13,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingDischarge48', 'TideRange3')),
      list(vars = c('TideRange3', 'RollingV168'))
   ),
   gam_levels  = 4,
   nthreads = 4
)

# GAM 8: Gamma, raw discharge, time, tide, V wind by wind sign
gam16 <- fit_gam(
   data = daily_data,
   predictors = preds13,
   folds = folds, 
   family_type = 'gaussian',
   transform_response = 'none',
   link = NULL,
   k_flow_range = c(10, 50),
   k_physical_range = c(5, 20),
   k_temporal_range = c(5, 20),
   k_interaction_range = c(3, 10),
   interactions = list(
      list(vars = c('RollingDischarge48', 'TideRange3')),
      list(vars = c('TideRange3', 'RollingV168')),
      list(vars = c('RollingDischarge48', 'TideRange3', 'RollingV168'))
   ),
   gam_levels  = 4,
   nthreads = 4
)


# Write output files
objects <- list(gam1, gam2, gam3, gam4, gam5, gam6, gam7, gam8)
file_names <- list('Gam1', 'Gam2', 'Gam3', 'Gam4', 'Gam5',      
                   'Gam6', 'Gam7', 'Gam8')  
write_qs_files(objects, 'Outputs/Experiments/Models/DailyGAM/RawDischarge', file_names)

# Write output files
objects <- list(gam9, gam10, gam11, gam12, gam13, gam14, gam15, gam16)
file_names <- list('Gam9', 'Gam10', 'Gam11', 'Gam12', 'Gam13',      
                   'Gam14', 'Gam15', 'Gam16')  
write_qs_files(objects, 'Outputs/Experiments/Models/DailyGAM/LogDischarge', file_names)

# Clear global environment
rm(list = ls())


