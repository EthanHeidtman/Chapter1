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
#                   Phase 1 — CV tunes k (parallelized across k-combos via
#                             future/furrr), refits top N candidates to
#                             extract EDF (serial), saves metadata + plots.
#                   Phase 3 — After plot inspection, set SELECTED_CANDIDATE_RANK
#                             and run from here to refit + save the final model.
#
#                 PARALLELIZATION NOTES:
#                   - plan() is set here, not inside fit_gam, so the same
#                     fit_gam works unmodified on a laptop, a Windows machine,
#                     or a cluster -- only this block needs to change per
#                     platform.
#                   - nthreads inside every CV bam() call is forced to 1
#                     inside fit_gam itself (not configurable here) -- this
#                     was confirmed necessary by benchmarking; see project
#                     notes on the k-combo-level parallelization decision.
#                   - Convergence is tracked per fold (n_folds_converged) and
#                     reported in candidate_summary as a diagnostic. It does
#                     NOT filter candidates automatically. Inspect it
#                     alongside the four selection plots before setting
#                     SELECTED_CANDIDATE_RANK.
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
#
# multisession works identically on macOS / Windows / Linux, and can later be
# swapped for a cluster backend (e.g. future.batchtools with a SLURM/PBS
# template) by changing ONLY this block -- fit_gam itself does not need to
# change. Workers are left at detectCores() - 2 by default to leave headroom
# for the OS on a laptop; override N_WORKERS directly on a dedicated machine
# or cluster node where full core count is available.
# =============================================================================

N_WORKERS <- max(1, parallel::detectCores() - 2)
plan(multisession, workers = N_WORKERS)
cat(sprintf("Parallel backend: multisession, %d workers\n\n", N_WORKERS))

# Progress bar handler -- prints a live bar in interactive sessions; falls
# back to periodic text updates when run non-interactively (e.g. Rscript).
if (interactive()) {
   handlers("progress")
} else {
   handlers("txtprogressbar")
}
handlers(global = TRUE)

# =============================================================================
# PARAMETERS
# =============================================================================

H_MAX <- 20

# Manually selected predictors based on RF screening (Scripts 02/03)
# LagSalinity:         snapshot at issue date, linear main effect
# RollingDischarge30:  sustained discharge, dominant across most horizons
# MaxDischarge10:      flushing discharge pulse signal
# TideMean30:          tide, weak but retained
# RollingWindCross12:  wind cross-estuary component, with WindDir by-variable

SELECTED_PREDICTORS <- c('h', 'LagSalinity', 'RollingDischarge50',
                         'MaxDischarge10', 'TideRange60', 'RollingWindCross12')

GAM_LEVELS          <- 6   # number of k combos to try when fitting
# (raise this now that the grid search is parallelized
# -- e.g. 8-10 for a more thorough search; wall time
# scales with nrow(k_grid) / N_WORKERS, not nrow(k_grid))

# =============================================================================
# LOAD DATA
# =============================================================================

stacked_data <- as.data.frame(
   read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs')
)

stacked_data <- stacked_data %>%
   filter(h <= H_MAX) %>%
   arrange(DateTime, h)

HIGH_SALINITY_THRESHOLD <- quantile(stacked_data$Salinity_h, 0.75, na.rm = TRUE)

cat(sprintf("Stacked training rows: %d (h = 1:%d)\n", nrow(stacked_data), H_MAX))

# =============================================================================
# EXPANDING WINDOW CV FOLDS
# =============================================================================

folds <- make_expanding_folds(stacked_data, initial_train_length = 6)
cat(sprintf("Number of CV folds: %d\n", length(folds)))

# =============================================================================
# PHASE 1: CV TUNING + CANDIDATE EDF EXTRACTION
# K-combo grid search is parallelized (see plan() above). Top-N refit for EDF
# extraction stays serial (cheap; not worth parallelizing). bam objects from
# the grid search / refit are NOT retained or saved.
# =============================================================================

cat("\n=== Phase 1: Building Unified Multi-Horizon GAM Candidates ===\n")
cat("Predictors:", paste(SELECTED_PREDICTORS, collapse = ", "), "\n\n")

t_phase1 <- system.time({
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
      n_top_candidates        = 10,
      plot_output_dir         = 'Outputs/Plots/UnifiedGAM/GAMSelection',
      n_workers               = N_WORKERS,
      show_progress           = TRUE
   )
})

cat(sprintf("\nPhase 1 wall time: %.1f min\n\n", t_phase1["elapsed"] / 60))

gc() # Clean the environment for stability in writing files

# =============================================================================
# SAVE METADATA
# gam_candidates contains: tune_grid, top_candidates (k-values + CV metrics),
# candidate_summary (EDF + metrics + n_folds_converged), edf_tables,
# fold_cv_all (per-fold detail incl. converged/warning_text), data_clean,
# model_cols, fit_params.
# =============================================================================

write_qs_files(list(gam_candidates), 'Outputs/Models/UnifiedGAM', list('CandidateGAMs_Metadata'))

# =============================================================================
# PHASE 2: INSPECT PLOTS — stop here, do not run Phase 3 yet
# 1. Open Outputs/Plots/UnifiedGAM/GAMSelection/
# 2. Review AccuracyVsComplexity, AccuracyVsConsistency, EDFHeatmap, FoldProfiles
# 3. ALSO check candidate_summary$n_folds_converged for each candidate --
#    a candidate with strong RMSE but a low convergence count may be winning
#    on folds that didn't actually settle to a stable fit. This is a
#    diagnostic only; nothing is auto-excluded. If you want to inspect WHY a
#    fold didn't converge, gam_candidates$fold_cv_all$warning_text has the
#    captured warning message(s) for that (k_index, fold) pair.
# 4. Choose the candidate with the most physically interpretable smooth
#    structure among those with competitive high-salinity RMSE and an
#    acceptable convergence count
# 5. Set SELECTED_CANDIDATE_RANK below and run Phase 3
# =============================================================================

SELECTED_CANDIDATE_RANK <- 1  # <-- update after plot + convergence inspection

# =============================================================================
# PHASE 3: REFIT SELECTED CANDIDATE + SAVE FINAL MODEL
# Refits from saved k-values and data_clean. Single fit -- no parallelization
# needed or used here.
# =============================================================================

cat(sprintf("\n=== Phase 3: Refitting Candidate Rank %d ===\n", SELECTED_CANDIDATE_RANK))

# Load metadata if starting a fresh session after plot inspection
# gam_candidates <- read_qs_files('Outputs/Models/UnifiedGAM/CandidateGAMs_Metadata.qs')

gam_unified <- select_gam_candidate(candidates_output = gam_candidates, rank = SELECTED_CANDIDATE_RANK)

write_qs_files(list(gam_unified), 'Outputs/Models/UnifiedGAM', list('GamUnified'))

cat("\nScript 04 complete. Final model saved to Outputs/Models/UnifiedGAM/GamUnified.qs\n")

plan(sequential)  # release workers

rm(list = ls())