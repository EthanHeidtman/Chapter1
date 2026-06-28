# =============================================================================
# Script Name:    02_ScreenStackedRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Runs a single random forest on the stacked date-horizon
#                 dataset (h = 1:20) to screen predictors for the unified
#                 multi-horizon GAM. Variable importance is stored stratified
#                 by horizon h. A stability analysis
#                 across 10 seeds tracks importance rank and weighted mean
#                 horizon variance to flag collinearity-driven instability.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ranger)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/PerformRFCV.R')

# =============================================================================
# PARAMETERS
# =============================================================================

SEED           <- 123
ntree          <- 250 # number of trees
mtry           <- 10  # number of splits to make
N_STABLE_SEEDS <- 10  # number of seeds to try

# =============================================================================
# LOAD DATA
# =============================================================================

stacked_data <- as.data.frame(
   read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs')
)
stacked_data <- stacked_data %>% arrange(DateTime, h)

# =============================================================================
# DEFINE PREDICTOR COLUMNS
# =============================================================================

# Remove columns that aren't predictors
non_predictor_cols <- c('DateTime', 'Year', 'Month', 'Day', 'DayOfYear',
                        'FERC', 'Discharge', 'Salinity_h',
                        'Inflows', 'Gust', 'Tide')
predictor_cols <- setdiff(names(stacked_data), non_predictor_cols)

cat(sprintf("Total predictors (including h): %d\n", length(predictor_cols)))

# Predictor group map
group_map <- list(
   LagSalinity        = grep("LagSalinity",                    predictor_cols, value = TRUE),
   Tide               = grep("TideRange|TideMean",             predictor_cols, value = TRUE),
   Wind               = grep("RollingWind",                    predictor_cols, value = TRUE),
   SustainedDischarge = grep("RollingDischarge|RollingAnomaly",predictor_cols, value = TRUE),
   FlushingDischarge  = grep("MaxDischarge|ExceedFlux",        predictor_cols, value = TRUE)
)

assign_group <- function(var) {
   for (grp in names(group_map)) {
      if (var %in% group_map[[grp]]) return(grp)
   }
   if (var == 'h') return('Horizon')
   return('other')
}

# =============================================================================
# EXPANDING WINDOW CV FOLDS
# =============================================================================

set.seed(SEED)
folds <- make_expanding_folds(stacked_data, initial_train_length = 6)

# =============================================================================
# HELPER: compute h-stratified permutation importance from stored fold models
# =============================================================================

compute_h_importance <- function(rf_result, stacked_data, predictor_cols) {
   
   h_importance_list <- list()
   
   for (i in seq_along(rf_result$folds)) {
      
      fold_result <- rf_result$folds[[i]]
      if (is.null(fold_result)) next
      
      rf_model <- fold_result$model
      if (is.null(rf_model)) next
      
      test_rows <- stacked_data %>%
         filter(Year %in% fold_result$test_years, !is.na(Salinity_h))
      
      train_rows <- stacked_data %>%
         filter(Year %in% fold_result$train_years, !is.na(Salinity_h))
      
      # Month-median imputation consistent with run_rf_cv
      for (col in predictor_cols) {
         month_medians <- tapply(train_rows[[col]], train_rows$Month,
                                 function(x) median(x, na.rm = TRUE))
         test_rows[[col]] <- ifelse(
            is.na(test_rows[[col]]),
            month_medians[as.character(test_rows$Month)],
            test_rows[[col]]
         )
      }
      
      for (hval in sort(unique(test_rows$h))) {
         
         h_slice <- test_rows %>% filter(h == hval)
         if (nrow(h_slice) < 10) next
         
         obs        <- h_slice[['Salinity_h']]
         preds_base <- predict(rf_model, data = h_slice)$predictions
         base_rmse  <- sqrt(mean((obs - preds_base)^2))
         
         imp_h <- sapply(predictor_cols, function(var) {
            h_perm        <- h_slice
            h_perm[[var]] <- sample(h_perm[[var]])
            preds_perm    <- predict(rf_model, data = h_perm)$predictions
            sqrt(mean((obs - preds_perm)^2)) - base_rmse
         })
         
         h_importance_list[[length(h_importance_list) + 1]] <- data.frame(
            Variable   = names(imp_h),
            Importance = as.numeric(imp_h),
            h          = hval,
            Fold       = i,
            row.names  = NULL
         )
      }
   }
   
   if (length(h_importance_list) == 0) return(NULL)
   
   do.call(rbind, h_importance_list) %>%
      group_by(Variable, h) %>%
      summarise(
         MeanImportance = mean(Importance, na.rm = TRUE),
         SDImportance   = sd(Importance,   na.rm = TRUE),
         .groups = 'drop'
      ) %>%
      mutate(Group = sapply(Variable, assign_group)) %>%
      arrange(Group, Variable, h)
}

# =============================================================================
# HELPER: compute weighted mean horizon from h-importance table
# =============================================================================

compute_weighted_h <- function(h_imp_df) {
   h_imp_df %>%
      mutate(Importance = pmax(MeanImportance, 0)) %>%
      group_by(Variable, Group) %>%
      summarise(
         WeightedMeanH  = if_else(
            sum(Importance) > 0,
            sum(h * Importance) / sum(Importance),
            mean(h)
         ),
         MeanImportance = mean(Importance),
         .groups = 'drop'
      )
}

# =============================================================================
# MAIN RF RUN
# =============================================================================

cat("\nRunning main RF (seed =", SEED, ")...\n")
set.seed(SEED)

rf_stacked <- run_rf_cv(
   data           = stacked_data,
   folds          = folds,
   response_col   = 'Salinity_h',
   predictor_cols = predictor_cols,
   ntree          = ntree,
   mtry           = mtry
)

print(rf_stacked$metrics)

# OOB importance summary
importance_summary <- rf_stacked$importance_oob %>%
   group_by(Variable) %>%
   summarise(
      MeanIncMSE = mean(IncMSE_OOB, na.rm = TRUE),
      SDIncMSE   = sd(IncMSE_OOB,   na.rm = TRUE),
      .groups    = 'drop'
   ) %>%
   mutate(Group = sapply(Variable, assign_group)) %>%
   arrange(desc(MeanIncMSE))

cat("\nImportance summary:\n")
print(importance_summary %>% arrange(Group, desc(MeanIncMSE)), n = 100)

# H-stratified importance (single main seed, fold-averaged)
cat("\nComputing h-stratified importance for main RF...\n")
h_importance <- compute_h_importance(rf_stacked, stacked_data, predictor_cols)

if (is.null(h_importance)) {
   warning("H-stratified importance is NULL — ensure run_rf_cv stores model objects.")
} else {
   cat(sprintf("H-stratified importance: %d rows\n", nrow(h_importance)))
}

# =============================================================================
# STABILITY ANALYSIS
# Reruns full CV across N_STABLE_SEEDS seeds. For each seed, stores:
#   - fold-averaged OOB importance per variable
#   - h-stratified importance (fold-averaged within seed)
#   - importance-weighted mean horizon per variable
# Summary across seeds: selection frequency (rank <=3 within group),
# mean and SD of weighted mean horizon — flags collinearity-driven instability.
#
# h_importance_seeds: the seed-averaged h-stratified importance used in
# Script 03 plots. MeanImportance = mean across seeds (each seed itself
# fold-averaged); SDImportance = SD across seeds.
# =============================================================================

cat(sprintf("\n=== STABILITY ANALYSIS (%d seeds) ===\n", N_STABLE_SEEDS))

stable_seeds          <- (1:N_STABLE_SEEDS) * 17  # deterministic but varied
stability_records     <- list()
h_importance_per_seed <- list()  # accumulate h-stratified importance per seed

for (s in seq_along(stable_seeds)) {
   
   seed_s <- stable_seeds[s]
   cat(sprintf("\nSeed %d of %d (seed = %d)...\n", s, N_STABLE_SEEDS, seed_s))
   set.seed(seed_s)
   
   rf_s <- run_rf_cv(
      data           = stacked_data,
      folds          = folds,
      response_col   = 'Salinity_h',
      predictor_cols = predictor_cols,
      ntree          = ntree,
      mtry           = mtry
   )
   
   # Fold-averaged OOB importance
   imp_s <- rf_s$importance_oob %>%
      group_by(Variable) %>%
      summarise(MeanIncMSE = mean(IncMSE_OOB, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Group = sapply(Variable, assign_group))
   
   # Rank within group
   imp_s <- imp_s %>%
      group_by(Group) %>%
      mutate(RankWithinGroup = rank(-MeanIncMSE)) %>%
      ungroup()
   
   # H-stratified importance (fold-averaged within this seed)
   h_imp_s <- compute_h_importance(rf_s, stacked_data, predictor_cols)
   
   if (!is.null(h_imp_s)) {
      
      # Weighted mean horizon for stability summary
      wh_s <- compute_weighted_h(h_imp_s) %>%
         select(Variable, WeightedMeanH)
      imp_s <- imp_s %>% left_join(wh_s, by = 'Variable')
      
      # Tag with seed index and accumulate for cross-seed averaging
      h_importance_per_seed[[s]] <- h_imp_s %>%
         mutate(Seed = seed_s)
      
   } else {
      imp_s$WeightedMeanH <- NA_real_
   }
   
   imp_s$Seed <- seed_s
   stability_records[[s]] <- imp_s
   
   cat(sprintf("  Seed %d complete.\n", s))
}

# Combine and summarise stability across seeds (OOB importance)
stability_all <- do.call(rbind, stability_records)

stability_summary <- stability_all %>%
   filter(Group %in% c('LagSalinity', 'Tide', 'Wind',
                       'SustainedDischarge', 'FlushingDischarge')) %>%
   group_by(Group, Variable) %>%
   summarise(
      MeanImportance    = mean(MeanIncMSE,      na.rm = TRUE),
      SDImportance      = sd(MeanIncMSE,        na.rm = TRUE),
      MeanRank          = mean(RankWithinGroup,  na.rm = TRUE),
      SDRank            = sd(RankWithinGroup,    na.rm = TRUE),
      MeanWeightedH     = mean(WeightedMeanH,    na.rm = TRUE),
      SDWeightedH       = sd(WeightedMeanH,      na.rm = TRUE),
      TopThreeFreq      = mean(RankWithinGroup <= 3, na.rm = TRUE),
      .groups           = 'drop'
   ) %>%
   arrange(Group, MeanRank)

cat("\n=== STABILITY SUMMARY ===\n")
print(stability_summary %>%
         select(Group, Variable, MeanRank, SDRank,
                MeanWeightedH, SDWeightedH, TopThreeFreq) %>%
         arrange(Group, MeanRank),
      n = 100)

# =============================================================================
# SEED-AVERAGED H-STRATIFIED IMPORTANCE
# Each seed contributes one fold-averaged MeanImportance per Variable x h.
# Averaging across seeds gives a more stable importance surface for plotting.
# This replaces the single-seed h_importance in Script 03 visualizations.
# =============================================================================

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

outputs <- list(
   rf_stacked,
   importance_summary,
   h_importance,          # single-seed, fold-averaged (retained for reference)
   stability_summary,
   stability_all,
   h_importance_seeds     # seed-averaged, fold-averaged — used in Script 03 plots
)

file_names <- c(
   'RFStacked',
   'RFImportanceSummary',
   'RFImportanceByHorizon',
   'RFStabilitySummary',
   'RFStabilityAll',
   'RFImportanceByHorizonSeeded'
)

write_qs_files(outputs, 'Outputs/Models/StackedRF', file_names)

cat("\nScript 02 complete.\n")

rm(list = ls())