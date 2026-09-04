# =============================================================================
# Script Name:    06_EvaluateUnifiedGAM.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    First, adjusts the final GAM covariance structure to account 
#                 for residual autocorrelation when fitting. 
#                 Evaluates the unified multi-horizon GAM on holdout data.
#                 All helper and plotting functions live in GamEvaluationPlots.R.
#                 This script: loads data, sets parameters, calls functions.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(mgcv)
library(patchwork)
library(svglite)
library(lubridate)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Plots/GamEvaluationPlots.R')   # all helpers + plot functions

# =============================================================================
# PARAMETERS
# =============================================================================

EVENT_DATE_RANGE        <- c('2016-09-15', '2016-11-15')
N_CAL_BINS              <- 10
EVENT_HORIZONS          <- c(14, 10, 7, 3)

predictor_colors <- list(
   LagSalinity        = "#E07B3F",
   SustainedDischarge = "#009bba",
   FlushingDischarge  = "#2E8B57",
   Tide               = "#D4AC0D",
   Wind               = "#8B4789"
)

gam_colors <- list(
   primary   = "#f58220",
   secondary = "#009bba",
   tertiary  = "#fdb515", 
   dark      = "#002030",
   threshold = "#002030"
)

# Output directories
base_dir    <- "Outputs/Plots/UnifiedGAM/FinalGAM"
error_dir   <- file.path(base_dir, 'Error')
smooth_dir  <- file.path(base_dir, "Smooths")
acf_dir     <- file.path(base_dir, "ACF")
panel_dir   <- file.path(base_dir, "ForecastPanels")
panel_grids_dir <- file.path(base_dir, "PanelGrids")

for (d in c(base_dir, error_dir, smooth_dir, acf_dir, panel_dir, panel_grids_dir)) {
   if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# =============================================================================
# LOAD MODEL AND DATA
# =============================================================================

gam_unified   <- read_qs_files('Outputs/Models/UnifiedGAM/GamUnified.qs2')
stacked_train <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs2'))
stacked_hold  <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/StackedHoldoutData.qs2'))

gam_obj <- gam_unified$gam_object

H_MAX <- max(gam_obj$model$h)

# 75th percentile of non-stacked daily training response (h == 1 un-duplicates the time series)
HIGH_SALINITY_THRESHOLD <- quantile(
   stacked_train$Salinity_h[stacked_train$h == 1],
   probs = 0.75,
   na.rm = TRUE
)

model_vars <- setdiff(all.vars(gam_unified$formula), "Response")
group_vars <- discover_predictor_groups(model_vars)

PAIRED_GROUP_ORDER <- c(
   "LagSalinity",
   "Wind",
   "SustainedDischarge",
   "FlushingDischarge",
   "Tide"
)

PAIRED_ROW_ORDER <- unname(group_vars[PAIRED_GROUP_ORDER])
smooth_grid_vars <- unname(group_vars[c("SustainedDischarge", "Wind", "FlushingDischarge", "Tide")])
lag_salinity_var <- group_vars[["LagSalinity"]]
wind_var_name    <- group_vars[["Wind"]]

wind_convention      <- get_wind_convention(gam_unified, stacked_train)
reference_wind_level <- wind_convention$positive_level

smooth_labels <- sapply(gam_obj$smooth, function(x) x$label)
ti_labels     <- smooth_labels[grepl("^ti\\(", smooth_labels)]
s_labels      <- smooth_labels[grepl("^s\\(",  smooth_labels)]

cat("Smooth terms:\n");  print(smooth_labels)
cat("\nTensor (ti):\n"); print(ti_labels)
cat("\nMarginal s():\n"); print(s_labels)

# Replicate exact filtering and sorting constraints used in Script 04 training
stacked_train <- add_wind_dir(stacked_train, gam_obj, wind_var_name, wind_convention) %>% 
   filter(h <= H_MAX) %>%
   arrange(DateTime, h)

stacked_hold  <- add_wind_dir(stacked_hold, gam_obj, wind_var_name, wind_convention) %>%
   filter(h <= H_MAX, Year > 2022) %>%
   arrange(DateTime, h)

reference_wind_value <- median(
   stacked_train[[wind_var_name]][stacked_train$WindDir == reference_wind_level],
   na.rm = TRUE
)

# =============================================================================
# APPLY CLUSTER-ROBUST COVARIANCE ADJUSTMENT
# =============================================================================

# 1. Calculate the robust sandwich matrix using our uncompressed score method
robust_vcov <- get_cluster_robust_vcov(
   model            = gam_obj, 
   data             = stacked_train, 
   cluster_var_name = "DateTime"
)

# 2. Inject robust errors back into standard internal slots. 
# mgcv's prediction engines automatically read these to compute ribbon limits.
gam_obj$Vp <- robust_vcov
gam_obj$Vc <- robust_vcov
gam_obj$Ve <- robust_vcov

# 3. Update the Bayesian standard deviation vector cache if present
if (!is.null(gam_obj$bayes.cstd)) {
   gam_obj$bayes.cstd <- sqrt(diag(robust_vcov))
}

# Write adjusted version
gam_adjusted <- gam_unified
gam_adjusted$gam_object <- gam_obj

write_qs_files(list(gam_adjusted), 'Outputs/Models/UnifiedGAM', list('GamUnified_Adjusted'))

# =============================================================================
# PREDICT (Capturing robust standard errors for the plot ribbons)
# =============================================================================
cat("\nGenerating predictions and robust standard errors...\n")

# Holdout Data Predictions
pred_hold <- predict(gam_obj, newdata = stacked_hold, type = "response", se.fit = TRUE)
stacked_hold$Predicted    <- as.numeric(pred_hold$fit)
stacked_hold$Predicted_SE <- as.numeric(pred_hold$se.fit)
stacked_hold$Residual     <- stacked_hold$Salinity_h - stacked_hold$Predicted

# Training Data Predictions
pred_train <- predict(gam_obj, newdata = stacked_train, type = "response", se.fit = TRUE)
stacked_train$Predicted    <- as.numeric(pred_train$fit)
stacked_train$Predicted_SE <- as.numeric(pred_train$se.fit)
stacked_train$Residual     <- stacked_train$Salinity_h - stacked_train$Predicted

# =============================================================================
# PERFORMANCE METRICS
# NSE retained in table for dissertation text/appendix; not plotted.
# =============================================================================

perf_hold <- stacked_hold %>%
   filter(!is.na(Salinity_h), !is.na(Predicted), !is.na(LagSalinity)) %>%
   group_by(h) %>%
   summarise(
      RMSE                  = sqrt(mean((Salinity_h - Predicted)^2)),
      MAE                   = mean(abs(Salinity_h - Predicted)),
      Bias                  = mean(Predicted - Salinity_h),
      R2                    = cor(Salinity_h, Predicted)^2,
      NSE                   = 1 - sum((Salinity_h - Predicted)^2) /
         sum((Salinity_h - mean(Salinity_h))^2),
      
      # High-salinity GAM performance
      RMSE_High             = { hi <- Salinity_h > HIGH_SALINITY_THRESHOLD
      if (sum(hi) > 1) sqrt(mean((Salinity_h[hi] - Predicted[hi])^2))
      else NA_real_ },
      MAE_High              = { hi <- Salinity_h > HIGH_SALINITY_THRESHOLD
      if (sum(hi) > 1) mean(abs(Salinity_h[hi] - Predicted[hi]))
      else NA_real_ },
      
      # Baseline Persistence (Overall vs High-Salinity)
      RMSE_Persistence      = sqrt(mean((Salinity_h - LagSalinity)^2)),
      RMSE_Persistence_High = { hi <- Salinity_h > HIGH_SALINITY_THRESHOLD
      if (sum(hi) > 1) sqrt(mean((Salinity_h[hi] - LagSalinity[hi])^2))
      else NA_real_ },
      
      # Matched Skill Scores
      Skill_Overall         = 1 - (RMSE / RMSE_Persistence),
      Skill_High            = 1 - (RMSE_High / RMSE_Persistence_High),
      
      N                     = n(),
      N_High                = sum(Salinity_h > HIGH_SALINITY_THRESHOLD, na.rm = TRUE),
      .groups               = "drop"
   ) %>%
   rename(LeadTime = h)

# =============================================================================
# PLOTS
# =============================================================================

cat("\nPlotting performance metrics...\n")
plot_performance_metrics(perf_hold, H_MAX, gam_colors, error_dir)

cat("\nPlotting residual diagnostics...\n")
plot_residual_diagnostics(stacked_hold, H_MAX, HIGH_SALINITY_THRESHOLD, gam_colors, error_dir)

cat("\nPlotting calibration...\n")
plot_calibration(stacked_hold, H_MAX, N_CAL_BINS, gam_colors, error_dir)

cat("\nCalculating CSI metrics...\n")
# Dynamic Q75 threshold calculated directly from training dataset
q75_val   <- as.numeric(quantile(stacked_train$Salinity_h, 0.75, na.rm = TRUE))
q75_label <- sprintf("Q75 (%.2f ppt)", q75_val)

# Helper function for temporal CSI calculation across lead times
compute_csi <- function(df_h, threshold, tol_days = 0) {
   df_clean <- df_h %>% 
      filter(!is.na(Salinity_h), !is.na(Predicted)) %>% 
      arrange(DateTime)
   
   if (nrow(df_clean) == 0) return(NA_real_)
   
   obs_idx  <- which(df_clean$Salinity_h >= threshold)
   pred_idx <- which(df_clean$Predicted >= threshold)
   
   if (length(obs_idx) == 0 && length(pred_idx) == 0) return(1.0)
   if (length(obs_idx) == 0) return(0.0)
   if (length(pred_idx) == 0) return(0.0)
   
   if (tol_days == 0) {
      hits <- sum(df_clean$Salinity_h >= threshold & df_clean$Predicted >= threshold)
      fa   <- sum(df_clean$Salinity_h <  threshold & df_clean$Predicted >= threshold)
      miss <- sum(df_clean$Salinity_h >= threshold & df_clean$Predicted <  threshold)
   } else {
      times <- df_clean$DateTime
      
      pred_matched <- vapply(pred_idx, function(p) {
         any(abs(as.numeric(difftime(times[obs_idx], times[p], units = "days"))) <= tol_days)
      }, logical(1))
      
      obs_matched <- vapply(obs_idx, function(o) {
         any(abs(as.numeric(difftime(times[pred_idx], times[o], units = "days"))) <= tol_days)
      }, logical(1))
      
      hits <- sum(obs_matched)
      fa   <- sum(!pred_matched)
      miss <- sum(!obs_matched)
   }
   
   denom <- hits + fa + miss
   if (denom == 0) return(NA_real_)
   hits / denom
}

# Construct pre-calculated CSI dataset for GamEvaluationPlots.R
label_05_0day <- "0 Days"
label_05_1day   <- "1 Day"
label_05_2day  <- "2 Days"
#label_q75_hold  <- paste0(q75_label, " Holdout - 0 Days")
label_05_hold <- '0.5 ppt Holdout - 0 Days'

curve_levels <- c(label_05_0day, label_05_1day, label_05_2day, label_05_hold)

csi_records <- list()
for (h_val in 1:H_MAX) {
   train_h <- stacked_train %>% filter(h == h_val)
   hold_h  <- stacked_hold  %>% filter(h == h_val)
   
   csi_records[[length(csi_records) + 1]] <- data.frame(
      LeadTime = h_val,
      CSI      = compute_csi(train_h, threshold = 0.5, tol_days = 0),
      Curve    = label_05_0day
   )
   csi_records[[length(csi_records) + 1]] <- data.frame(
      LeadTime = h_val,
      CSI      = compute_csi(train_h, threshold = 0.5, tol_days = 1),
      Curve    = label_05_1day
   )
   csi_records[[length(csi_records) + 1]] <- data.frame(
      LeadTime = h_val,
      CSI      = compute_csi(train_h, threshold = 0.5, tol_days = 2),
      Curve    = label_05_2day
   )
   csi_records[[length(csi_records) + 1]] <- data.frame(
      LeadTime = h_val,
      #CSI      = compute_csi(hold_h, threshold = q75_vl, tol_days = 0),
      #Curve    = label_q75_hold
      CSI      = compute_csi(hold_h, threshold = 0.5, tol_days = 0),
      Curve    = label_05_hold
   )
}

csi_df <- bind_rows(csi_records) %>% 
   mutate(Curve = factor(Curve, levels = curve_levels))

cat("Generating combined CSI and Calibration plot...\n")
skill_cal_plots <- plot_skill_and_calibration(
   csi_df        = csi_df,
   stacked_hold  = stacked_hold,
   stacked_train = stacked_train,
   H_MAX         = H_MAX,
   N_CAL_BINS    = N_CAL_BINS,
   gam_colors    = gam_colors,
   dir           = error_dir
)

cat("\nPlotting ACF/PACF...\n")
plot_acf_pacf(stacked_hold, H_MAX, gam_colors, acf_dir)

cat("\nPlotting 1D smooths...\n")
# Captured — needed downstream to build the paired smooth/tensor grid.
smooth_plots <- plot_1d_smooths(
   gam_obj, s_labels, stacked_train, model_vars,
   wind_var_name, wind_convention, reference_wind_level, reference_wind_value,
   predictor_colors, smooth_dir
)

cat("\nBuilding LagSalinity rug panel (no marginal smooth exists for this term)...\n")
smooth_plots[[lag_salinity_var]] <- plot_lag_salinity_rug(
   stacked_train, predictor_colors, lag_salinity_var
)

# Set up a new unified directory for the combined robust surfaces
pred_surfaces_dir <- file.path(base_dir, "PredictionSurfaces")
if (!dir.exists(pred_surfaces_dir)) dir.create(pred_surfaces_dir, recursive = TRUE)

cat("\nPlotting robust tensor interaction surfaces...\n")
# Captured — nested list: tensor_output[[SeasonName]][[PredictorName]]
tensor_output <- plot_robust_tensor_surfaces(
   gam_obj               = gam_obj, 
   ti_labels             = ti_labels, 
   stacked_train         = stacked_train, 
   model_vars            = model_vars,
   wind_var_name         = wind_var_name, 
   wind_convention       = wind_convention,
   reference_wind_level  = reference_wind_level, 
   reference_wind_value  = reference_wind_value,
   predictor_colors      = predictor_colors, 
   H_MAX                 = H_MAX, 
   output_dir            = pred_surfaces_dir
)

cat("\nBuilding paired smooth/tensor grids (one per season, for internal review)...\n")
build_paired_grid(
   smooth_plots = smooth_plots,
   tensor_plots = tensor_output$DrySeason,
   row_order    = PAIRED_ROW_ORDER,
   output_dir   = panel_grids_dir,
   season_name  = "DrySeason"
)
build_paired_grid(
   smooth_plots = smooth_plots,
   tensor_plots = tensor_output$WetSeason,
   row_order    = PAIRED_ROW_ORDER,
   output_dir   = panel_grids_dir,
   season_name  = "WetSeason"
)

cat("\nBuilding supplemental 2x2 smooth grid...\n")
build_smooth_grid(
   smooth_plots = smooth_plots,
   var_order    = smooth_grid_vars,
   output_dir   = panel_grids_dir,
   ncol         = 2,
   nrow         = 2
)

cat("\nBuilding main-text 5-panel tensor grids (one per season)...\n")
build_tensor_grid(
   tensor_plots = tensor_output$DrySeason,
   var_order    = PAIRED_ROW_ORDER,
   H_MAX        = H_MAX,
   output_dir   = panel_grids_dir,
   season_name  = "DrySeason",
   ncol         = 3,
   nrow         = 2
)
build_tensor_grid(
   tensor_plots = tensor_output$WetSeason,
   var_order    = PAIRED_ROW_ORDER,
   H_MAX        = H_MAX,
   output_dir   = panel_grids_dir,
   season_name  = "WetSeason",
   ncol         = 3,
   nrow         = 2
)

cat("\nBuilding forecast panel plots...\n")
p_event <- plot_salinity_forecast_panels(
   data       = stacked_train,
   date_range = EVENT_DATE_RANGE,
   horizons   = EVENT_HORIZONS,
   epa_line   = TRUE,
   threshold  = 0.5,
   title      = NULL
)
ggsave(file.path(panel_dir, "Oct2016Event_Panels.png"),
       plot = p_event, width = 14, height = 2.5 * length(EVENT_HORIZONS), dpi = 600)
ggsave(file.path(panel_dir, "Oct2016Event_Panels.svg"),
       plot = p_event, width = 14, height = 2.5 * length(EVENT_HORIZONS))

p_holdout <- plot_salinity_forecast_panels(
   data      = stacked_hold,
   horizons  = EVENT_HORIZONS,
   epa_line  = FALSE,
   threshold = 0.5,
   title     = NULL,
   y_expand  = c(0.10, 0.10)
)
ggsave(file.path(panel_dir, "Holdout_Panels.png"),
       plot = p_holdout, width = 14, height = 2.5 * length(EVENT_HORIZONS), dpi = 600)
ggsave(file.path(panel_dir, "Holdout_Panels.svg"),
       plot = p_holdout, width = 14, height = 2.5 * length(EVENT_HORIZONS))

# =============================================================================
# WRITE PERFORMANCE TABLE
# =============================================================================

write_qs_files(
   list(perf_hold),
   'Outputs/Models/UnifiedGAM',
   list('HoldoutPerformanceByH')
)

cat("\nScript 06 complete. Plots saved to:", base_dir, "\n")
rm(list = ls())