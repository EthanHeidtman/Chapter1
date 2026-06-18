# =============================================================================
# Script Name:    02_ScreenDailyRF_final.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Uses a random forest to screen predictors and identify the
#                 top variables in each group. Selects the best and saves a
#                 screened version of the data for GAM fitting.
#
#           
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ranger)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/ShiftPredictors.R')
source('Scripts/Utilities/MakeCVFolds.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Utilities/PerformRFCV.R')

# =============================================================================
# PARAMETERS
# =============================================================================

# Lead times to screen (days)
lead_times <- seq(0, 30, 1)

# Random forest hyperparameters
SEED  <- 123
ntree <- 500
mtry  <- 10

# Stability check: run RF across multiple seeds to assess selection consistency.
# Slow - disable for production runs, enable for diagnostics.
RUN_STABILITY_CHECK <- TRUE
N_SEEDS <- 5

# =============================================================================
# LOAD DATA
# =============================================================================

daily_data <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/FinalModelData.qs'))
daily_data <- daily_data %>%
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   relocate(Tide, .after = Salinity) %>%
   filter(Date > '2007-03-29') %>%
   dplyr::select(-contains('Inflows'))

# =============================================================================
# MAIN SCREENING LOOP
# =============================================================================

set.seed(SEED)

for (k in lead_times) {
   
   cat("\n=== Processing lead time k =", k, "days ===\n")
   
   # Shift predictors by k days
   if (k == 0) {
      daily_data_k <- daily_data
   } else {
      daily_data_k <- shift_predictors_by_k(daily_data, k = k)
   }
   
   
   salinity_cluster <- daily_data_k %>%
      dplyr::select(contains('Salinity'))
   
   sustained_discharge_cluster <- daily_data_k %>%
      dplyr::select(Salinity,
                    contains(c('RollingDischarge', 'RollingAnomaly')))
   
   flushing_discharge_cluster <- daily_data_k %>%
      dplyr::select(Salinity,
                    contains(c('MaxDischarge', 'ExceedFlux'))) 
   
   tide_cluster <- daily_data_k %>%
      dplyr::select(Salinity,
                    contains(c('TideRange', 'TideMean')))     
   
   wind_cluster <- daily_data_k %>%
      dplyr::select(Salinity,
                    contains(c('RollingWindAlong', 'RollingWindCross')))
   
   # ---------------------------------------------------------------------------
   # RUN RF WITH EXPANDING WINDOW CV
   # ---------------------------------------------------------------------------
   
   folds_daily <- make_expanding_folds(daily_data_k, initial_train_length = 6)
   
   rf_daily <- run_rf_cv(
      data          = daily_data_k,
      folds         = folds_daily,
      response_col  = 'Salinity',
      predictor_cols = 9:ncol(daily_data_k),
      ntree         = ntree,
      mtry          = mtry
   )
   
   # ---------------------------------------------------------------------------
   # SELECT TOP VARIABLES PER GROUP
   # ---------------------------------------------------------------------------
   
   group_list <- list(
      salinity             = salinity_cluster,
      sustained_discharge  = sustained_discharge_cluster,
      flushing_discharge   = flushing_discharge_cluster,
      tide                 = tide_cluster,
      wind                 = wind_cluster
   )
   
   top_vars_daily <- get_top_vars_by_group(
      importance_df  = rf_daily$importance,
      group_dfs      = group_list,
      n_top          = list(salinity            = 4,
                            sustained_discharge = 4,
                            flushing_discharge  = 4,
                            tide                = 4,
                            wind                = 4),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   if ("Variable" %in% names(top_vars_daily[[1]])) {
      selected_vars_daily <- unlist(lapply(top_vars_daily, function(x) x$Variable),
                                    use.names = FALSE)
   } else {
      selected_vars_daily <- unlist(top_vars_daily, use.names = FALSE)
   }
   
   # ---------------------------------------------------------------------------
   # STABILITY CHECK (optional)
   # Reruns RF across N_SEEDS seeds and reports selection frequency per group.
   # Features appearing in the top selection across most seeds are stable;
   # features that swap frequently with neighbors flag collinearity.
   # ---------------------------------------------------------------------------
   
   if (RUN_STABILITY_CHECK) {
      
      cat("  Running stability check across", N_SEEDS, "seeds...\n")
      
      seed_selections <- vector("list", N_SEEDS)
      
      for (s in seq_len(N_SEEDS)) {
         
         set.seed(s * 17)  # deterministic but varied seeds
         
         rf_s <- run_rf_cv(
            data           = daily_data_k,
            folds          = folds_daily,
            response_col   = 'Salinity',
            predictor_cols = 9:ncol(daily_data_k),
            ntree          = ntree,
            mtry           = mtry
         )
         
         top_s <- get_top_vars_by_group(
            importance_df   = rf_s$importance,
            group_dfs       = group_list,
            n_top           = list(salinity            = 1,
                                   sustained_discharge = 1,
                                   flushing_discharge  = 1,
                                   tide                = 1,
                                   wind                = 1),
            importance_col  = "IncMSE_OOB",
            show_importance = FALSE
         )
         
         seed_selections[[s]] <- unlist(top_s, use.names = TRUE)
      }
      
      # Summarise selection frequency per group
      selection_df <- do.call(rbind, lapply(seq_len(N_SEEDS), function(s) {
         data.frame(seed = s, group = names(seed_selections[[s]]),
                    variable = seed_selections[[s]], stringsAsFactors = FALSE)
      }))
      
      stability_summary <- selection_df %>%
         group_by(group, variable) %>%
         summarise(freq = n(), pct = 100 * n() / N_SEEDS, .groups = "drop") %>%
         arrange(group, desc(freq))
      
      cat("\n  Selection stability summary (k =", k, "):\n")
      print(stability_summary, n = 50)
      
      # Write stability results
      write_qs_files(
         list(stability_summary),
         'Outputs/Experiments/Models/DailyRF',
         list(paste0('RFStability_lag', k))
      )
      
      # Reset seed for main run consistency
      set.seed(SEED)
   }
   
   # ---------------------------------------------------------------------------
   # SAVE OUTPUTS
   # ---------------------------------------------------------------------------
   
   daily_data_screened <- daily_data_k %>%
      dplyr::select(c(1:'Salinity'), all_of(selected_vars_daily))
   
   write_qs_files(
      list(daily_data_screened),
      'Data/Tidied/Final/Daily',
      list(paste0('FinalDataScreened_lag', k))
   )
   
   write_qs_files(
      list(rf_daily),
      'Outputs/Experiments/Models/DailyRF',
      list(paste0('RFDailyScreening_lag', k))
   )
   
   cat("Completed lead time k =", k, "days\n")
}

rm(list = ls())