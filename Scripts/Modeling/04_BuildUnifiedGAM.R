# =============================================================================
# Script Name:    04_BuildUnifiedGAM.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Fits the unified multi-horizon salinity GAM on the stacked
#                 date-horizon dataset. Predictors are manually selected based
#                 on Script 02/03 RF screening and stability analysis. h is an
#                 explicit smooth predictor, with ti(h, predictor) interactions
#                 for each selected predictor. K is tuned via expanding window
#                 CV across grouped k-ranges (see FitGAM.R).
#
#                 Two-phase workflow:
#                   Phase 1 — CV tunes k, refits top N candidates to extract
#                             EDF, saves metadata + plots. NO bam objects saved.
#                   Phase 3 — After plot inspection, set SELECTED_CANDIDATE_RANK
#                             and run from here to refit + save the final model.
# =============================================================================

library(tidyverse)
library(mgcv)
library(dplyr)
library(purrr)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/FitGAM.R')

set.seed(123)

# =============================================================================
# PARAMETERS
# =============================================================================

H_MAX <- 20

# Manually selected predictors based on RF screening (Scripts 02/03)
# LagSalinity:        snapshot at issue date, linear main effect
# RollingDischarge30: sustained discharge, dominant across most horizons
# MaxDischarge10:      flushing discharge pulse signal
# TideMean30:         tide, weak but retained
# RollingWindCross12:  wind cross-estuary component, with WindDir by-variable
SELECTED_PREDICTORS <- c('h', 'LagSalinity', 'RollingDischarge30',
                         'MaxDischarge10', 'TideMean30', 'RollingWindCross12')

HIGH_SALINITY_THRESHOLD <- 0.16 # 75th percentile of daily maximum salinity
GAM_LEVELS              <- 3

# =============================================================================
# LOAD DATA
# =============================================================================

stacked_data <- as.data.frame(
   read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs')
)

stacked_data <- stacked_data %>%
   filter(h <= H_MAX) %>%
   arrange(DateTime, h)

cat(sprintf("Stacked training rows: %d (h = 1:%d)\n", nrow(stacked_data), H_MAX))

# =============================================================================
# EXPANDING WINDOW CV FOLDS
# =============================================================================

folds <- make_expanding_folds(stacked_data, initial_train_length = 6)
cat(sprintf("Number of CV folds: %d\n", length(folds)))

# =============================================================================
# PHASE 1: CV TUNING + CANDIDATE EDF EXTRACTION
# Runs all k combinations, refits top N on full data to extract EDF,
# saves selection plots. bam objects are NOT retained or saved.
# =============================================================================

cat("\n=== Phase 1: Building Unified Multi-Horizon GAM Candidates ===\n")
cat("Predictors:", paste(SELECTED_PREDICTORS, collapse = ", "), "\n\n")

gam_candidates <- fit_gam(
   data                    = stacked_data,
   response                = 'Salinity_h',
   predictors              = SELECTED_PREDICTORS,
   folds                   = folds,
   family_type             = 'gaussian',
   link                    = NULL,
   high_salinity_threshold = HIGH_SALINITY_THRESHOLD,
   gam_levels              = GAM_LEVELS,
   nthreads                = 4,
   n_top_candidates        = 10,
   plot_output_dir         = 'Outputs/Plots/UnifiedGAM/GAMSelection'
)

gc() # Clean the environment for stability in writing files

# =============================================================================
# SAVE METADATA
# gam_candidates contains: tune_grid, top_candidates (k-values + CV metrics),
# candidate_summary (EDF + metrics), edf_tables, data_clean, model_cols, fit_params. 
# =============================================================================

write_qs_files(
   list(gam_candidates),
   'Outputs/Models/UnifiedGAM',
   list('CandidateGAMs_Metadata')
)

cat("\nPhase 1 complete.\n")
cat("Open Outputs/Plots/UnifiedGAM/GAMSelection/ to inspect selection plots.\n")
cat("Then set SELECTED_CANDIDATE_RANK below and run Phase 3.\n\n")

# =============================================================================
# PHASE 2: INSPECT PLOTS — stop here, do not run Phase 3 yet
# -----------------------------------------------------------------------------
# 1. Open Outputs/Plots/UnifiedGAM/GAMSelection/
# 2. Review AccuracyVsComplexity, AccuracyVsConsistency, EDFHeatmap, FoldProfiles
# 3. Choose the candidate with the most physically interpretable smooth
#    structure among those with competitive high-salinity RMSE
# 4. Set SELECTED_CANDIDATE_RANK below and run Phase 3
# =============================================================================

SELECTED_CANDIDATE_RANK <- 1  # <-- update after plot inspection

# =============================================================================
# PHASE 3: REFIT SELECTED CANDIDATE + SAVE FINAL MODEL
# Refits from saved k-values and data_clean
# =============================================================================

cat(sprintf("\n=== Phase 3: Refitting Candidate Rank %d ===\n", SELECTED_CANDIDATE_RANK))

# Load metadata if starting a fresh session after plot inspection
# gam_candidates <- read_qs_files('Outputs/Models/UnifiedGAM/CandidateGAMs_Metadata.qs')

gam_unified <- select_gam_candidate(
   candidates_output = gam_candidates,
   rank              = SELECTED_CANDIDATE_RANK
)

write_qs_files(
   list(gam_unified),
   'Outputs/Models/UnifiedGAM',
   list('GamUnified')
)

cat("\nScript 04 complete. Final model saved to Outputs/Models/UnifiedGAM/GamUnified.qs\n")

rm(list = ls())
