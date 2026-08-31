# =============================================================================
# Script Name:    04_FitUnifiedGAM_PARALLEL.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Fits the unified multi-horizon salinity GAM on the stacked
#                 date-horizon dataset. Predictors are automatically selected
#                 from Script 02 RF stability screening (top variable per
#                 group). h is an explicit smooth predictor, with ti(h, predictor)
#                 interactions for each selected predictor. K is tuned via
#                 expanding window CV across grouped k-ranges.
# =============================================================================

library(tidyverse)
library(mgcv)
library(dplyr)
library(purrr)
library(future)
library(furrr)
library(progressr)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/FitGAM_PARALLEL.R')

set.seed(123)

# =============================================================================
# PARALLEL BACKEND SETUP
# =============================================================================

N_WORKERS <- max(1, parallel::detectCores() - 2)
plan(multisession, workers = N_WORKERS)
cat(sprintf("Parallel backend: multisession, %d workers\n\n", N_WORKERS))

# Progress bar handler
if (interactive()) {
   handlers("progress")
} else {
   handlers("txtprogressbar")
}
handlers(global = TRUE)

# =============================================================================
# PARAMETERS
# =============================================================================

H_MAX      <- 20
GAM_LEVELS <- 6  # number of k combos to try when fitting

# =============================================================================
# DYNAMIC PREDICTOR SELECTION FROM SCRIPT 02 RF STABILITY
# =============================================================================

stability_summary <- read_qs_files('Outputs/Models/StackedRF/RFStabilitySummary.qs2')

target_groups <- c("LagSalinity", "SustainedDischarge", "Wind", 
                   "FlushingDischarge", "Tide")

# Extract top variable (lowest MeanRank) per group
auto_selected_vars <- stability_summary %>%
   filter(Group %in% target_groups) %>%
   group_by(Group) %>%
   slice_min(MeanRank, n = 1, with_ties = FALSE) %>%
   ungroup() %>%
   # Maintain consistent group ordering
   mutate(Group = factor(Group, levels = target_groups)) %>%
   arrange(Group) %>%
   pull(Variable)

SELECTED_PREDICTORS <- c('h', auto_selected_vars)

cat("=== AUTOMATICALLY SELECTED PREDICTORS (Script 02 RF Stability) ===\n")
cat(paste(sprintf("  - %s", SELECTED_PREDICTORS), collapse = "\n"), "\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

stacked_data <- as.data.frame(
   read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs2')
)

stacked_data <- stacked_data %>%
   filter(h <= H_MAX) %>%
   arrange(DateTime, h)

HIGH_SALINITY_THRESHOLD <- quantile(
   stacked_data$Salinity_h[stacked_data$h == 1],
   probs = 0.75,
   na.rm = TRUE
)

cat(sprintf("Stacked training rows: %d (h = 1:%d)\n", nrow(stacked_data), H_MAX))

# =============================================================================
# EXPANDING WINDOW CV FOLDS
# =============================================================================

folds <- make_expanding_folds(stacked_data, initial_train_length = 9)
cat(sprintf("Number of CV folds: %d\n", length(folds)))

# =============================================================================
# CV TUNING + CANDIDATE EDF EXTRACTION
# =============================================================================

cat("\n=== Building Unified Multi-Horizon GAM Candidates ===\n")
cat("Predictors:", paste(SELECTED_PREDICTORS, collapse = ", "), "\n\n")

gam_fitting <- system.time({
   gam_candidates <- fit_gam(
      data                    = stacked_data,
      response                = 'Salinity_h',
      predictors              = SELECTED_PREDICTORS,
      folds                   = folds,
      family_type             = 'gaussian',
      link                    = NULL,
      high_salinity_threshold = HIGH_SALINITY_THRESHOLD,
      gam_levels              = GAM_LEVELS,
      nthreads                = 4,   # used only for the serial top-10 EDF refit stage
      n_top_candidates        = 10000,
      n_workers               = N_WORKERS,
      show_progress           = TRUE,
      wind_ti_by              = TRUE
   )
})

cat(sprintf("\nFitting wall time: %.1f min\n\n", gam_fitting["elapsed"] / 60))

gc() # Clean the environment for stability in writing files

# =============================================================================
# SAVE METADATA
# gam_candidates contains: tune_grid, top_candidates (k-values + CV metrics),
# candidate_summary (EDF + metrics + n_folds_converged), edf_tables,
# fold_cv_all (per-fold detail incl. converged/warning_text), data_clean,
# model_cols, fit_params.
# =============================================================================

write_qs_files(list(gam_candidates), 'Outputs/Models/UnifiedGAM', list('CandidateGAMs_Metadata'))

plan(sequential)  # release workers

cat("\nScript 04 complete. Candidate metadata saved to Outputs/Models/UnifiedGAM/CandidateGAMs_Metadata.qs2\n")
cat("Proceed to Script 05 for diagnostic plotting and model selection.\n")

rm(list = ls())