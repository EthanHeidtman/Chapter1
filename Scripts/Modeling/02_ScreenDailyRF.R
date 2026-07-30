# =============================================================================
# Script Name:    02_ScreenStackedRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Runs a single random forest on the stacked date-horizon
#                 dataset (h = 1:20) to screen predictors for the unified
#                 multi-horizon GAM. Importance is h-stratified, computed via
#                 OOB trees (predict.all + inbag masking), repeat-averaged
#                 permutation. A 10-seed stability analysis (parallelized)
#                 tracks rank stability of this same stream to select each
#                 group's representative predictor.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ranger)
library(future)
library(furrr)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/PerformRFCV.R')

# =============================================================================
# PARAMETERS
# =============================================================================

SEED             <- 123
ntree            <- 300
mtry             <- 10  
N_STABLE_SEEDS   <- 10    
N_REPEATS        <- 2    
N_SCREEN         <- 10
VARS_PER_CHUNK   <- 2
N_WORKERS        <- 4
N_THREADS_SERIAL <- max(1, parallel::detectCores(logical = FALSE) - 1)

checkpoint_dir <- 'Outputs/Models/StackedRF/SeedCheckpoints'
if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE)

# =============================================================================
# LOAD DATA
# =============================================================================

stacked_data <- as.data.frame(
   read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs')
) %>% arrange(DateTime, h)

non_predictor_cols <- c('DateTime', 'Year', 'Month', 'Day', 'DayOfYear',
                        'FERC', 'Discharge', 'Salinity_h',
                        'Inflows', 'Gust', 'Tide')
predictor_cols <- setdiff(names(stacked_data), non_predictor_cols)
group_map      <- build_group_map(predictor_cols)

cat(sprintf("Total predictors (including h): %d\n", length(predictor_cols)))

set.seed(SEED)
folds <- make_expanding_folds(stacked_data, initial_train_length = 9)

# =============================================================================
# MAIN RF RUN + SCREENING
# =============================================================================

cat("\nRunning main RF (seed =", SEED, ")...\n")
set.seed(SEED)

main_rf <- system.time({
   rf_stacked <- run_rf_cv(
      data = stacked_data, folds = folds, response_col = 'Salinity_h',
      predictor_cols = predictor_cols, ntree = ntree, mtry = mtry,
      num_threads = N_THREADS_SERIAL, calc_test_imp = TRUE
   )
})
cat(sprintf("\nMain RF time: %.1f min\n\n", main_rf["elapsed"] / 60))
print(rf_stacked$metrics)
gc()

screened_cols <- screen_predictors_per_group(rf_stacked, predictor_cols, group_map,
                                             n_screen = N_SCREEN)
cat(sprintf("Screening: %d of %d predictors retained for h-stratified step\n",
            length(screened_cols), length(predictor_cols)))

cat("\nComputing primary h-stratified importance for main RF...\n")

main_rf_importance <- system.time({
   h_importance <- compute_h_importance(rf_stacked, screened_cols, group_map,
                                        n_repeats = N_REPEATS, num_threads = N_THREADS_SERIAL,
                                        vars_per_chunk = VARS_PER_CHUNK)
   
})

cat(sprintf("\nMain RF Importance calculations time: %.1f min\n\n", main_rf_importance["elapsed"] / 60))

gc()

# =============================================================================
# STABILITY ANALYSIS -- PARALLELIZED ACROSS SEEDS
# =============================================================================

cat(sprintf("\n=== STABILITY ANALYSIS (%d seeds, %d workers) ===\n",
            N_STABLE_SEEDS, N_WORKERS))

stable_seeds <- (1:N_STABLE_SEEDS) * 17
utils_path   <- here('Scripts', 'Utilities', 'PerformRFCV.R')

run_one_seed <- function(seed_s, stacked_data, folds, predictor_cols, screened_cols,
                         group_map, ntree, mtry, n_repeats, vars_per_chunk,
                         utils_path, checkpoint_dir) {
   
   source(utils_path)
   library(dplyr); library(ranger)
   
   checkpoint_file <- file.path(checkpoint_dir, sprintf('seed_%d.rds', seed_s))
   if (file.exists(checkpoint_file)) return(readRDS(checkpoint_file))
   
   result <- tryCatch({
      
      set.seed(seed_s)
      # Skip test-set importance during stability seeds (calc_test_imp = FALSE)
      rf_s <- run_rf_cv(
         data = stacked_data, folds = folds, response_col = 'Salinity_h',
         predictor_cols = predictor_cols, ntree = ntree, mtry = mtry,
         num_threads = 1, calc_test_imp = FALSE
      )
      
      h_imp_s <- compute_h_importance(rf_s, screened_cols, group_map,
                                      n_repeats = n_repeats, num_threads = 1,
                                      vars_per_chunk = vars_per_chunk)
      
      mag_s <- h_imp_s %>%
         group_by(Variable, Group) %>%
         summarise(HStratMag = mean(MeanImportance, na.rm = TRUE), .groups = 'drop') %>%
         group_by(Group) %>%
         mutate(RankWithinGroup = rank(-HStratMag)) %>%
         ungroup() %>%
         mutate(Seed = seed_s)
      
      out <- list(seed = seed_s, h_imp_s = h_imp_s %>% mutate(Seed = seed_s),
                  mag_s = mag_s, status = 'ok')
      saveRDS(out, checkpoint_file)
      out
      
   }, error = function(e) {
      out <- list(seed = seed_s, h_imp_s = NULL, mag_s = NULL,
                  status = 'error', error_message = conditionMessage(e))
      saveRDS(out, checkpoint_file)
      out
   })
   
   result
}

plan(multisession, workers = N_WORKERS)


rf_seeds <- system.time({
   seed_results <- future_map(
      stable_seeds, run_one_seed,
      stacked_data = stacked_data, folds = folds, predictor_cols = predictor_cols,
      screened_cols = screened_cols, group_map = group_map, ntree = ntree, mtry = mtry,
      n_repeats = N_REPEATS, vars_per_chunk = VARS_PER_CHUNK,
      utils_path = utils_path, checkpoint_dir = checkpoint_dir,
      .options = furrr_options(seed = TRUE), .progress = TRUE
   )
})

cat(sprintf("\nRF all seeds time: %.1f min\n\n", rf_seeds["elapsed"] / 60))

plan(sequential)

statuses <- map_chr(seed_results, "status")
if (any(statuses == 'error')) {
   failed <- stable_seeds[statuses == 'error']
   msgs   <- map_chr(seed_results[statuses == 'error'], "error_message")
   warning(sprintf("Seeds failed: %s\nErrors: %s",
                   paste(failed, collapse = ', '), paste(msgs, collapse = ' | ')))
}

ok_results             <- seed_results[statuses == 'ok']
h_importance_per_seed  <- map(ok_results, "h_imp_s")
h_stability_records    <- map(ok_results, "mag_s")

cat(sprintf("\n%d of %d seeds completed successfully.\n",
            length(ok_results), length(stable_seeds)))

# =============================================================================
# STABILITY SUMMARY + SEED-AVERAGED IMPORTANCE
# =============================================================================

h_stability_all <- do.call(rbind, h_stability_records)
h_stability_summary <- h_stability_all %>%
   group_by(Group, Variable) %>%
   summarise(
      MeanImportance = mean(HStratMag,       na.rm = TRUE),
      SDImportance   = sd(HStratMag,         na.rm = TRUE),
      MeanRank       = mean(RankWithinGroup, na.rm = TRUE),
      SDRank         = sd(RankWithinGroup,   na.rm = TRUE),
      TopThreeFreq   = mean(RankWithinGroup <= 3, na.rm = TRUE),
      .groups        = 'drop'
   ) %>%
   arrange(Group, MeanRank)

cat("\n=== STABILITY SUMMARY ===\n")
print(h_stability_summary %>%
         select(Group, Variable, MeanRank, SDRank, TopThreeFreq) %>%
         arrange(Group, MeanRank), n = 100)

h_importance_seeds <- do.call(rbind, h_importance_per_seed) %>%
   group_by(Variable, h, Group) %>%
   summarise(
      MeanImportance = mean(MeanImportance, na.rm = TRUE),
      SDImportance   = sd(MeanImportance,   na.rm = TRUE),
      .groups = 'drop'
   ) %>%
   arrange(Group, Variable, h)

cat(sprintf("\nSeed-averaged h-stratified importance: %d rows across %d seeds\n",
            nrow(h_importance_seeds), length(h_importance_per_seed)))

# =============================================================================
# WRITE OUTPUTS
# =============================================================================

outputs <- list(rf_stacked, h_importance, h_stability_summary,
                h_stability_all, h_importance_seeds)
file_names <- c('RFStacked', 'RFImportanceByHorizon', 'RFStabilitySummary',
                'RFStabilityAll', 'RFImportanceByHorizonSeeded')

write_qs_files(outputs, 'Outputs/Models/StackedRF', file_names)

cat("\nScript 02 complete.\n")

rm(list = ls())