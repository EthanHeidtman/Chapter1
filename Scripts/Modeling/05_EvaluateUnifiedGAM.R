# =============================================================================
# Script Name:    05_EvaluateUnifiedGAM.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Evaluates the unified multi-horizon GAM on holdout data.
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
EVENT_HORIZONS          <- c(1, 7, 14)

predictor_colors <- list(
   LagSalinity        = "#E07B3F",
   RollingDischarge50 = "#009bba",
   MaxDischarge10      = "#2E8B57",
   TideRange60         = "#D4AC0D",
   RollingWindCross12  = "#8B4789"
)
TENSOR_NEGATIVE_COLOR <- "#002030"

gam_colors <- list(
   primary   = "#f58220",
   secondary = "#009bba",
   tertiary  = "#fdb515",
   dark      = "#002030",
   threshold = "#002030"
)

# Output directories
base_dir    <- "Outputs/Plots/UnifiedGAM/FinalGAM"
smooth_dir  <- file.path(base_dir, "Smooths")
acf_dir     <- file.path(base_dir, "ACF")
panel_dir   <- file.path(base_dir, "ForecastPanels")

tensor_full_dir    <- file.path(base_dir, "TensorSurfaces", "Full")
tensor_partial_dir <- file.path(base_dir, "TensorSurfaces", "Partial")
slice_dir          <- file.path(base_dir, "TensorSurfaces", "Slices")
deriv_dir          <- file.path(base_dir, "TensorSurfaces", "Derivatives")

for (d in c(base_dir, smooth_dir, acf_dir, panel_dir,
            tensor_full_dir, tensor_partial_dir, slice_dir, deriv_dir)) {
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

# Add WindDir and filter horizons
stacked_train <- add_wind_dir(stacked_train, gam_obj) %>% filter(h <= H_MAX)
stacked_hold  <- add_wind_dir(stacked_hold,  gam_obj) %>%
   filter(h <= H_MAX, Year > 2022)

# Wind reference values for prediction grids
wind_var_name        <- setdiff(all.vars(formula(gam_obj)),
                                c("Response", 'h', 'LagSalinity', 'RollingDischarge50',
                                  'MaxDischarge10', 'TideRange60', "WindDir"))

#c(h', 'LagSalinity', 'RollingDischarge50',
 # 'MaxDischarge10', 'TideRange60', 'RollingWindCross12'
wind_var_name        <- wind_var_name[grepl("^RollingWind", wind_var_name)]
reference_wind_level <- levels(stacked_train$WindDir)[2]
reference_wind_value <- median(
   stacked_train[[wind_var_name]][stacked_train$WindDir == reference_wind_level],
   na.rm = TRUE
)

model_vars <- setdiff(all.vars(gam_unified$formula), "Response")

# =============================================================================
# PREDICT
# =============================================================================

stacked_hold$Predicted  <- as.numeric(predict(gam_obj, newdata = stacked_hold,  type = "response"))
stacked_hold$Residual   <- stacked_hold$Salinity_h - stacked_hold$Predicted

stacked_train$Predicted <- as.numeric(predict(gam_obj, newdata = stacked_train, type = "response"))
stacked_train$Residual  <- stacked_train$Salinity_h - stacked_train$Predicted

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
plot_residual_diagnostics(stacked_hold, H_MAX, HIGH_SALINITY_THRESHOLD,
                          gam_colors, base_dir)

cat("\nPlotting calibration...\n")
plot_calibration(stacked_hold, N_CAL_BINS, gam_colors, base_dir)

cat("\nPlotting ACF/PACF...\n")
plot_acf_pacf(stacked_hold, H_MAX, gam_colors, acf_dir)

cat("\nPlotting 1D smooths...\n")
plot_1d_smooths(
   gam_obj, s_labels, stacked_train, model_vars,
   wind_var_name, reference_wind_level, reference_wind_value,
   predictor_colors, smooth_dir
)

cat("\nPlotting tensor partial surfaces...\n")
plot_tensor_partial(
   gam_obj, ti_labels, stacked_train, model_vars,
   wind_var_name, reference_wind_level, reference_wind_value,
   predictor_colors, TENSOR_NEGATIVE_COLOR, H_MAX, tensor_partial_dir
)

cat("\nPlotting tensor full conditional surfaces...\n")
plot_tensor_full(
   gam_obj, ti_labels, stacked_train, model_vars,
   wind_var_name, reference_wind_level, reference_wind_value,
   predictor_colors, TENSOR_NEGATIVE_COLOR, H_MAX, tensor_full_dir
)

cat("\nPlotting tensor slices...\n")
plot_tensor_slices(
   gam_obj, ti_labels, stacked_train, model_vars,
   wind_var_name, reference_wind_level, reference_wind_value,
   predictor_colors, H_MAX, n_slices = 3, slice_dir
)

cat("\nPlotting tensor derivatives...\n")
plot_tensor_derivatives(
   gam_obj, ti_labels, stacked_train, model_vars,
   wind_var_name, reference_wind_level, reference_wind_value,
   predictor_colors, H_MAX, n_slices = 3, deriv_dir
)

cat("\nBuilding forecast panel plots...\n")

p_event <- plot_salinity_forecast_panels(
   data       = stacked_train,
   date_range = EVENT_DATE_RANGE,
   horizons   = EVENT_HORIZONS,
   epa_line   = TRUE,
   threshold  = 0.5,
   title      = "October 2016 Salinity Intrusion \u2014 Forecasts by Lead Time"
)
ggsave(file.path(panel_dir, "Oct2016Event_Panels.png"),
       plot = p_event, width = 10, height = 2.5 * length(EVENT_HORIZONS), dpi = 600)
ggsave(file.path(panel_dir, "Oct2016Event_Panels.svg"),
       plot = p_event, width = 10, height = 2.5 * length(EVENT_HORIZONS))

p_holdout <- plot_salinity_forecast_panels(
   data      = stacked_hold,
   horizons  = EVENT_HORIZONS,
   epa_line  = TRUE,
   threshold = 0.5,
   title     = "Holdout Period \u2014 Forecasts by Lead Time"
)
ggsave(file.path(panel_dir, "Holdout_Panels.png"),
       plot = p_holdout, width = 12, height = 2.5 * length(EVENT_HORIZONS), dpi = 600)
ggsave(file.path(panel_dir, "Holdout_Panels.svg"),
       plot = p_holdout, width = 12, height = 2.5 * length(EVENT_HORIZONS))

# Additional time periods: call plot_salinity_forecast_panels() directly with
# date_range = c('YYYY-MM-DD', 'YYYY-MM-DD') and any horizons vector.

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