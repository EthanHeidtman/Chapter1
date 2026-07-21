# =============================================================================
# Script Name:    05_EvaluateUnifiedGAM.R
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

H_MAX                   <- 20
HIGH_SALINITY_THRESHOLD <- 0.16
EVENT_DATE_RANGE        <- c('2016-09-15', '2016-11-15')
N_CAL_BINS              <- 10
EVENT_HORIZONS          <- c(3, 7, 10, 14)

predictor_colors <- list(
   LagSalinity        = "#E07B3F",
   RollingDischarge50 = "#009bba",
   MaxDischarge10      = "#2E8B57",
   TideRange60         = "#D4AC0D",
   RollingWindCross12  = "#8B4789"
)

gam_colors <- list(
   primary   = "#f58220",
   secondary = "#009bba",
   tertiary  = "#fdb515",
   dark      = "#002030",
   threshold = "#002030"
)

# Row order for the paired smooth/tensor grid — used by build_paired_grid()
PAIRED_ROW_ORDER <- c("LagSalinity", "RollingDischarge50", "RollingWindCross12",
                      "MaxDischarge10", "TideRange60")

# Output directories
base_dir    <- "Outputs/Plots/UnifiedGAM/FinalGAM"
smooth_dir  <- file.path(base_dir, "Smooths")
acf_dir     <- file.path(base_dir, "ACF")
panel_dir   <- file.path(base_dir, "ForecastPanels")
paired_grid_dir    <- file.path(base_dir, "PairedGrids")

for (d in c(base_dir, smooth_dir, acf_dir, panel_dir, paired_grid_dir)) {
   if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# =============================================================================
# LOAD MODEL AND DATA
# =============================================================================

gam_unified   <- read_qs_files('Outputs/Models/UnifiedGAM/GamUnified.qs')
stacked_train <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs'))
stacked_hold  <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/StackedHoldoutData.qs'))

gam_obj <- gam_unified$gam_object

smooth_labels <- sapply(gam_obj$smooth, function(x) x$label)
ti_labels     <- smooth_labels[grepl("^ti\\(", smooth_labels)]
s_labels      <- smooth_labels[grepl("^s\\(",  smooth_labels)]

cat("Smooth terms:\n");  print(smooth_labels)
cat("\nTensor (ti):\n"); print(ti_labels)
cat("\nMarginal s():\n"); print(s_labels)

# Replicate exact filtering and sorting constraints used in Script 04 training
stacked_train <- add_wind_dir(stacked_train, gam_obj) %>% 
   filter(h <= H_MAX) %>%
   arrange(DateTime, h)

stacked_hold  <- add_wind_dir(stacked_hold,  gam_obj) %>%
   filter(h <= H_MAX, Year > 2022) %>%
   arrange(DateTime, h)


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

# -----------------------------------------------------------------------------

# Wind reference values for prediction grids
wind_var_name        <- setdiff(all.vars(formula(gam_obj)),
                                c("Response", 'h', 'LagSalinity', 'RollingDischarge50',
                                  'MaxDischarge10', 'TideRange60', "WindDir"))

wind_var_name        <- wind_var_name[grepl("^RollingWind", wind_var_name)]
reference_wind_level <- levels(stacked_train$WindDir)[2]
reference_wind_value <- median(
   stacked_train[[wind_var_name]][stacked_train$WindDir == reference_wind_level],
   na.rm = TRUE
)

model_vars <- setdiff(all.vars(gam_unified$formula), "Response")

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
   filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
   group_by(h) %>%
   summarise(
      RMSE      = sqrt(mean((Salinity_h - Predicted)^2)),
      MAE       = mean(abs(Salinity_h - Predicted)),
      Bias      = mean(Predicted - Salinity_h),
      R2        = cor(Salinity_h, Predicted)^2,
      NSE       = 1 - sum((Salinity_h - Predicted)^2) /
         sum((Salinity_h - mean(Salinity_h))^2),
      RMSE_High = { hi <- Salinity_h > HIGH_SALINITY_THRESHOLD
      if (sum(hi) > 1) sqrt(mean((Salinity_h[hi] - Predicted[hi])^2))
      else NA_real_ },
      MAE_High  = { hi <- Salinity_h > HIGH_SALINITY_THRESHOLD
      if (sum(hi) > 1) mean(abs(Salinity_h[hi] - Predicted[hi]))
      else NA_real_ },
      N         = n(),
      .groups   = "drop"
   ) %>%
   rename(LeadTime = h)

# =============================================================================
# PLOTS
# =============================================================================

cat("\nPlotting performance metrics...\n")
plot_performance_metrics(perf_hold, H_MAX, gam_colors, base_dir)

cat("\nPlotting residual diagnostics...\n")
plot_residual_diagnostics(stacked_hold, H_MAX, HIGH_SALINITY_THRESHOLD, gam_colors, base_dir)

cat("\nPlotting calibration...\n")
plot_calibration(stacked_hold, N_CAL_BINS, gam_colors, base_dir)

cat("\nPlotting ACF/PACF...\n")
plot_acf_pacf(stacked_hold, H_MAX, gam_colors, acf_dir)

cat("\nPlotting 1D smooths...\n")
# Captured — needed downstream to build the paired smooth/tensor grid.
smooth_plots <- plot_1d_smooths(
   gam_obj, s_labels, stacked_train, model_vars,
   wind_var_name, reference_wind_level, reference_wind_value,
   predictor_colors, smooth_dir
)

cat("\nBuilding LagSalinity rug panel (no marginal smooth exists for this term)...\n")
smooth_plots[["LagSalinity"]] <- plot_lag_salinity_rug(
   stacked_train, predictor_colors
)

# Set up a new unified directory for the combined robust surfaces
tensor_robust_dir <- file.path(base_dir, "TensorSurfaces")
if (!dir.exists(tensor_robust_dir)) dir.create(tensor_robust_dir, recursive = TRUE)

cat("\nPlotting robust tensor interaction surfaces...\n")
# Captured — nested list: tensor_output[[SeasonName]][[PredictorName]]
tensor_output <- plot_robust_tensor_surfaces(
   gam_obj               = gam_obj, 
   ti_labels             = ti_labels, 
   stacked_train         = stacked_train, 
   model_vars            = model_vars,
   wind_var_name         = wind_var_name, 
   reference_wind_level  = reference_wind_level, 
   reference_wind_value  = reference_wind_value,
   predictor_colors      = predictor_colors, 
   H_MAX                 = H_MAX, 
   output_dir            = tensor_robust_dir
)

cat("\nBuilding paired smooth/tensor grids (one per season)...\n")
build_paired_grid(
   smooth_plots = smooth_plots,
   tensor_plots = tensor_output$DrySeason,
   row_order    = PAIRED_ROW_ORDER,
   output_dir   = paired_grid_dir,
   season_name  = "DrySeason"
)
build_paired_grid(
   smooth_plots = smooth_plots,
   tensor_plots = tensor_output$WetSeason,
   row_order    = PAIRED_ROW_ORDER,
   output_dir   = paired_grid_dir,
   season_name  = "WetSeason"
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
   epa_line  = TRUE,
   threshold = 0.5,
   title     = NULL
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

cat("\nScript 05 complete. Plots saved to:", base_dir, "\n")
rm(list = ls())