# =============================================================================
# Script Name:    05_EvaluateUnifiedGAM.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Evaluates the unified multi-horizon GAM. Performance metrics
#                 are computed on the holdout dataset stratified by lead time h.
#                 Two tensor surface types are plotted per predictor:
#                   (1) Full conditional surface — total predictor contribution
#                       relative to median conditions, in PSU. Headline figure.
#                   (2) Partial interaction surface — ti() term only, showing
#                       how horizon modulates each predictor's effect. Diagnostic.
#                 1D marginal smooths plotted with SE ribbons.
#                 Residual diagnostics and calibration computed across h-slices.
#                 October 2016 event evaluated on training data.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(mgcv)
library(patchwork)
library(svglite)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')

# =============================================================================
# PARAMETERS
# =============================================================================

H_MAX                   <- 20
HIGH_SALINITY_THRESHOLD <- 0.16
EVENT_DATE_RANGE        <- c('2016-09-15', '2016-11-15')
N_CAL_BINS              <- 10

# Color palette
gam_colors <- list(
   primary   = "#f58220",   # orange
   secondary = "#009bba",   # blue
   tertiary  = "#fdb515",   # yellow
   dark      = "#002030",   # dark blue
   threshold = "#002030"
)

# Output directories
base_dir   <- "Outputs/Plots/UnifiedGAM/FinalGAM"
tensor_dir <- file.path(base_dir, "TensorSurfaces")
smooth_dir <- file.path(base_dir, "Smooths")
acf_dir    <- file.path(base_dir, "ACF")

for (d in c(base_dir, tensor_dir, smooth_dir, acf_dir)) {
   if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# =============================================================================
# SHARED THEME
# =============================================================================

theme_eval <- function() {
   theme_bw() +
      theme(
         plot.title        = element_text(size = 14, face = "bold",  color = gam_colors$dark),
         plot.subtitle     = element_text(size = 11,                  color = gam_colors$dark),
         axis.title        = element_text(size = 12, face = "bold",  color = gam_colors$dark),
         axis.text         = element_text(size = 10,                  color = gam_colors$dark),
         panel.border      = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.title      = element_text(size = 11, face = "bold",  color = gam_colors$dark),
         legend.text       = element_text(size = 10,                  color = gam_colors$dark),
         legend.background = element_rect(fill = "white", color = gam_colors$dark, linewidth = 0.5),
         legend.key        = element_rect(fill = "white", color = NA),
         strip.text        = element_text(size = 11, face = "bold",  color = gam_colors$dark),
         strip.background  = element_rect(fill = "grey92",           color = gam_colors$dark)
      )
}

save_plot <- function(p, name, w = 10, h = 6) {
   ggsave(file.path(base_dir, paste0(name, ".png")), plot = p, width = w, height = h, dpi = 600)
   ggsave(file.path(base_dir, paste0(name, ".svg")), plot = p, width = w, height = h)
}

save_plot_dir <- function(p, dir, name, w = 10, h = 6) {
   ggsave(file.path(dir, paste0(name, ".png")), plot = p, width = w, height = h, dpi = 600)
   ggsave(file.path(dir, paste0(name, ".svg")), plot = p, width = w, height = h)
}

# =============================================================================
# LOAD MODEL AND DATA
# =============================================================================

gam_unified   <- read_qs_files('Outputs/Models/UnifiedGAM/GamUnified.qs')
stacked_train <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/StackedModelData.qs'))
stacked_hold  <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/StackedHoldoutData.qs'))

gam_obj <- gam_unified$gam_object

smooth_labels <- sapply(gam_obj$smooth, function(x) x$label)
cat("Smooth terms in model:\n"); print(smooth_labels)

# ti() terms for tensor surfaces
ti_labels <- smooth_labels[grepl("^ti\\(", smooth_labels)]
cat("\nTensor (ti) terms:\n"); print(ti_labels)

# s() marginal terms (excludes ti)
s_labels <- smooth_labels[grepl("^s\\(", smooth_labels)]
cat("\nMarginal s() terms:\n"); print(s_labels)

# =============================================================================
# HELPER: add WindDir consistent with fit_gam convention
# =============================================================================

add_wind_dir <- function(df, gam_obj) {
   wind_var <- setdiff(all.vars(formula(gam_obj)),
                       c("Response", "h", "LagSalinity", "RollingDischarge30",
                         "MaxDischarge9", "TideMean30", "WindDir"))
   wind_var <- wind_var[grepl("^RollingWind", wind_var)]
   if (length(wind_var) != 1) stop("Could not uniquely identify wind predictor.")
   
   wind_smooth <- Filter(function(s) grepl("WindDir", s$label), gam_obj$smooth)
   if (length(wind_smooth) == 0) stop("No WindDir smooth found.")
   wind_levels <- sapply(wind_smooth, `[[`, "by.level")
   
   if (all(c("LeftBank", "RightBank") %in% wind_levels)) {
      df$WindDir <- factor(ifelse(df[[wind_var]] >= 0, "RightBank", "LeftBank"),
                           levels = c("LeftBank", "RightBank"))
   } else if (all(c("UpEstuary", "DownEstuary") %in% wind_levels)) {
      df$WindDir <- factor(ifelse(df[[wind_var]] >= 0, "UpEstuary", "DownEstuary"),
                           levels = c("DownEstuary", "UpEstuary"))
   } else {
      stop("Unknown WindDir levels.")
   }
   df
}

stacked_train <- add_wind_dir(stacked_train, gam_obj) %>% filter(h <= H_MAX)
stacked_hold  <- add_wind_dir(stacked_hold,  gam_obj) %>% filter(h <= H_MAX)

# Identify model columns from the fitted formula
model_vars <- all.vars(gam_unified$formula)
model_vars <- setdiff(model_vars, "Response")

# =============================================================================
# PREDICT ON HOLDOUT AND TRAINING DATA
# =============================================================================

stacked_hold$Predicted  <- as.numeric(predict(gam_obj, newdata = stacked_hold,  type = "response"))
stacked_hold$Residual   <- stacked_hold$Salinity_h - stacked_hold$Predicted

stacked_train$Predicted <- as.numeric(predict(gam_obj, newdata = stacked_train, type = "response"))
stacked_train$Residual  <- stacked_train$Salinity_h - stacked_train$Predicted

# =============================================================================
# PERFORMANCE METRICS BY LEAD TIME (holdout)
# =============================================================================

compute_metrics <- function(df, threshold = HIGH_SALINITY_THRESHOLD) {
   df %>%
      filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
      group_by(h) %>%
      summarise(
         RMSE      = sqrt(mean((Salinity_h - Predicted)^2)),
         MAE       = mean(abs(Salinity_h - Predicted)),
         Bias      = mean(Predicted - Salinity_h),
         R2        = cor(Salinity_h, Predicted)^2,
         NSE       = 1 - sum((Salinity_h - Predicted)^2) /
            sum((Salinity_h - mean(Salinity_h))^2),
         RMSE_High = {
            high <- Salinity_h > threshold
            if (sum(high) > 1) sqrt(mean((Salinity_h[high] - Predicted[high])^2))
            else NA_real_
         },
         N         = n(),
         .groups   = "drop"
      ) %>%
      rename(LeadTime = h)
}

perf_hold <- compute_metrics(stacked_hold)

# ---- RMSE overall + high-salinity by lead time ----
p_rmse <- perf_hold %>%
   select(LeadTime, RMSE, RMSE_High) %>%
   pivot_longer(-LeadTime, names_to = "Metric", values_to = "Value") %>%
   mutate(Metric = recode(Metric,
                          "RMSE"      = "Overall RMSE",
                          "RMSE_High" = "High-Salinity RMSE")) %>%
   ggplot(aes(x = LeadTime, y = Value, color = Metric, group = Metric)) +
   geom_line(linewidth = 1.2) +
   geom_point(size = 3) +
   scale_color_manual(values = c("Overall RMSE"      = gam_colors$secondary,
                                 "High-Salinity RMSE" = gam_colors$primary),
                      name = NULL) +
   scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
   labs(title = "RMSE by Lead Time — Holdout",
        x     = "Lead Time (days)",
        y     = "RMSE (psu)") +
   theme_eval() +
   theme(legend.position = "bottom")

save_plot(p_rmse, "RMSE_ByLeadTime", w = 10, h = 6)

# ---- MAE overall + high-salinity by lead time ----
p_mae <- perf_hold %>%
   select(LeadTime, MAE) %>%
   ggplot(aes(x = LeadTime, y = MAE)) +
   geom_line(linewidth = 1.2, color = gam_colors$secondary) +
   geom_point(size = 3, color = gam_colors$secondary) +
   scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
   labs(title = "MAE by Lead Time — Holdout",
        x     = "Lead Time (days)",
        y     = "MAE (psu)") +
   theme_eval()

save_plot(p_mae, "MAE_ByLeadTime", w = 10, h = 6)

# ---- R2 and NSE ----
p_skill <- perf_hold %>%
   select(LeadTime, R2, NSE) %>%
   pivot_longer(-LeadTime, names_to = "Metric", values_to = "Value") %>%
   ggplot(aes(x = LeadTime, y = Value, color = Metric, group = Metric)) +
   geom_line(linewidth = 1.2) +
   geom_point(size = 3) +
   scale_color_manual(values = c("R2"  = gam_colors$secondary,
                                 "NSE" = gam_colors$primary),
                      name = NULL) +
   scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
   labs(title = "Skill Scores by Lead Time — Holdout",
        x     = "Lead Time (days)",
        y     = "Score") +
   theme_eval() +
   theme(legend.position = "bottom")

save_plot(p_skill, "Skill_ByLeadTime", w = 10, h = 6)

# ---- Bias ----
p_bias <- perf_hold %>%
   ggplot(aes(x = LeadTime, y = Bias)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = gam_colors$dark, linewidth = 0.6) +
   geom_line(linewidth = 1.2, color = gam_colors$primary) +
   geom_point(size = 3, color = gam_colors$primary) +
   scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
   labs(title = "Bias by Lead Time — Holdout",
        x     = "Lead Time (days)",
        y     = "Bias (psu)") +
   theme_eval()

save_plot(p_bias, "Bias_ByLeadTime", w = 10, h = 6)

# ---- Combined panel ----
p_combined <- (p_rmse | p_mae) / (p_skill | p_bias) +
   plot_annotation(title = "Forecast Performance by Lead Time — Holdout",
                   theme = theme(plot.title = element_text(
                      size = 16, face = "bold", color = gam_colors$dark)))

ggsave(file.path(base_dir, "Performance_Combined.png"),
       plot = p_combined, width = 14, height = 10, dpi = 600)
ggsave(file.path(base_dir, "Performance_Combined.svg"),
       plot = p_combined, width = 14, height = 10)

# =============================================================================
# RESIDUAL DIAGNOSTICS
# =============================================================================

h_breaks <- c(0, 5, 10, 15, 20, 25, 30)
h_labels <- c("h = 1\u20135", "h = 6\u201310", "h = 11\u201315",
              "h = 16\u201320", "h = 21\u201325", "h = 26\u201330")

resid_df <- stacked_hold %>%
   filter(!is.na(Residual)) %>%
   mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels))

# Observed vs predicted — orange = high salinity, blue = normal
p_obs_pred <- stacked_hold %>%
   filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
   mutate(HighSal = Salinity_h > HIGH_SALINITY_THRESHOLD) %>%
   ggplot(aes(x = Predicted, y = Salinity_h, color = HighSal)) +
   geom_point(alpha = 0.3, size = 0.8) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = gam_colors$dark) +
   scale_color_manual(values = c("FALSE" = gam_colors$secondary,
                                 "TRUE"  = gam_colors$primary),
                      labels = c("Normal", "High Salinity"), name = NULL) +
   labs(title = "Observed vs Predicted — Holdout",
        x     = "Predicted (psu)",
        y     = "Observed (psu)") +
   theme_eval() +
   theme(legend.position = "bottom")

save_plot(p_obs_pred, "ObsVsPred", w = 8, h = 7)

# Obs vs pred by h-bin
p_obs_pred_h <- stacked_hold %>%
   filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
   mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels)) %>%
   ggplot(aes(x = Predicted, y = Salinity_h)) +
   geom_point(alpha = 0.25, size = 0.7, color = gam_colors$secondary) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = gam_colors$dark) +
   facet_wrap(~ HBin) +
   labs(title = "Observed vs Predicted by Lead Time Bin — Holdout",
        x     = "Predicted (psu)",
        y     = "Observed (psu)") +
   theme_eval()

save_plot(p_obs_pred_h, "ObsVsPred_ByHBin", w = 10, h = 8)

# QQ
p_qq <- ggplot(resid_df, aes(sample = Residual)) +
   stat_qq(size = 0.6, alpha = 0.4, color = gam_colors$secondary) +
   stat_qq_line(color = gam_colors$dark, linetype = "dashed") +
   facet_wrap(~ HBin) +
   labs(title = "Q-Q Plot of Residuals by Lead Time Bin",
        x     = "Theoretical Quantiles",
        y     = "Sample Quantiles") +
   theme_eval()

save_plot(p_qq, "QQ_ByHBin", w = 10, h = 8)

# Residual histogram
p_resid_hist <- ggplot(resid_df, aes(x = Residual)) +
   geom_histogram(bins = 50, fill = gam_colors$secondary, alpha = 0.85,
                  color = gam_colors$dark, linewidth = 0.2) +
   facet_wrap(~ HBin, scales = "free_y") +
   labs(title = "Residual Distribution by Lead Time Bin",
        x     = "Residual (psu)",
        y     = "Count") +
   theme_eval()

save_plot(p_resid_hist, "ResidHist_ByHBin", w = 10, h = 8)

# Residuals vs fitted — loess trend in orange
p_resid_fitted <- ggplot(resid_df, aes(x = Predicted, y = Residual)) +
   geom_point(alpha = 0.25, size = 0.7, color = gam_colors$secondary) +
   geom_hline(yintercept = 0, linetype = "dashed", color = gam_colors$dark) +
   geom_smooth(method = "loess", se = FALSE, color = gam_colors$primary,
               linewidth = 0.9, span = 0.4) +
   facet_wrap(~ HBin) +
   labs(title = "Residuals vs Fitted by Lead Time Bin",
        x     = "Fitted (psu)",
        y     = "Residual (psu)") +
   theme_eval()

save_plot(p_resid_fitted, "ResidVsFitted_ByHBin", w = 10, h = 8)

# =============================================================================
# CALIBRATION
# =============================================================================

cal_df <- stacked_hold %>%
   filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
   mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels)) %>%
   group_by(HBin) %>%
   mutate(PredBin = cut(Predicted,
                        breaks = quantile(Predicted,
                                          probs = seq(0, 1, 1 / N_CAL_BINS),
                                          na.rm = TRUE),
                        include.lowest = TRUE, labels = FALSE)) %>%
   group_by(HBin, PredBin) %>%
   summarise(MeanPredicted = mean(Predicted,  na.rm = TRUE),
             MeanObserved  = mean(Salinity_h, na.rm = TRUE),
             N             = n(),
             .groups       = "drop")

cal_range <- range(c(cal_df$MeanPredicted, cal_df$MeanObserved), na.rm = TRUE)

p_cal <- ggplot(cal_df, aes(x = MeanPredicted, y = MeanObserved, size = N)) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = gam_colors$dark) +
   geom_point(color = gam_colors$secondary, alpha = 0.85) +
   facet_wrap(~ HBin) +
   scale_size_continuous(range = c(2, 6), name = "N") +
   coord_fixed(xlim = cal_range, ylim = cal_range) +
   labs(title    = "Calibration by Lead Time Bin — Holdout",
        subtitle = "Points on dashed line = perfect calibration",
        x        = "Mean Predicted (psu)",
        y        = "Mean Observed (psu)") +
   theme_eval()

save_plot(p_cal, "Calibration_ByHBin", w = 10, h = 8)

# =============================================================================
# ACF / PACF BY H-SLICE
# =============================================================================

ci_line <- qnorm(0.975) / sqrt(nrow(stacked_hold) / H_MAX)

acf_records  <- list()
pacf_records <- list()

for (hval in 1:H_MAX) {
   resid_h <- stacked_hold %>%
      filter(h == hval, !is.na(Residual)) %>%
      arrange(DateTime) %>%
      pull(Residual)
   
   if (length(resid_h) < 20) next
   
   acf_obj  <- acf(resid_h,  plot = FALSE, lag.max = 30)
   pacf_obj <- pacf(resid_h, plot = FALSE, lag.max = 30)
   
   acf_records[[hval]]  <- data.frame(h    = hval,
                                      Lag  = as.numeric(acf_obj$lag[-1]),
                                      ACF  = as.numeric(acf_obj$acf[-1]))
   pacf_records[[hval]] <- data.frame(h    = hval,
                                      Lag  = as.numeric(pacf_obj$lag),
                                      PACF = as.numeric(pacf_obj$acf))
}

acf_df  <- do.call(rbind, acf_records)
pacf_df <- do.call(rbind, pacf_records)

p_acf <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
   geom_col(fill = gam_colors$secondary, alpha = 0.85, width = 0.7) +
   geom_hline(yintercept = c(-ci_line, ci_line),
              linetype = "dashed", color = gam_colors$dark) +
   geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.3) +
   facet_wrap(~ paste0("h = ", h), ncol = 5) +
   labs(title = "Residual ACF by Lead Time",
        x     = "Lag",
        y     = "ACF") +
   theme_eval() +
   theme(axis.text = element_text(size = 7))

ggsave(file.path(acf_dir, "ACF_AllH.png"),
       plot = p_acf, width = 16, height = 14, dpi = 600)

p_pacf <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
   geom_col(fill = gam_colors$primary, alpha = 0.85, width = 0.7) +
   geom_hline(yintercept = c(-ci_line, ci_line),
              linetype = "dashed", color = gam_colors$dark) +
   geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.3) +
   facet_wrap(~ paste0("h = ", h), ncol = 5) +
   labs(title = "Residual PACF by Lead Time",
        x     = "Lag",
        y     = "PACF") +
   theme_eval() +
   theme(axis.text = element_text(size = 7))

ggsave(file.path(acf_dir, "PACF_AllH.png"),
       plot = p_pacf, width = 16, height = 14, dpi = 600)

# =============================================================================
# HELPER: build a median-filled prediction grid for a given predictor
# All variables other than h and pred_var are held at their training median
# or reference factor level.
# =============================================================================

make_pred_grid <- function(pred_var, h_seq, pred_seq, train_df, model_vars, gam_obj) {
   
   grid <- expand.grid(h = h_seq, .x = pred_seq)
   names(grid)[2] <- pred_var
   
   for (v in model_vars) {
      if (v %in% names(grid)) next
      if (v == "WindDir") {
         grid$WindDir <- factor(levels(train_df$WindDir)[1],
                                levels = levels(train_df$WindDir))
      } else if (v %in% names(train_df)) {
         if (is.factor(train_df[[v]])) {
            grid[[v]] <- factor(levels(train_df[[v]])[1],
                                levels = levels(train_df[[v]]))
         } else {
            grid[[v]] <- median(train_df[[v]], na.rm = TRUE)
         }
      }
   }
   grid
}

# =============================================================================
# 1D MARGINAL SMOOTH PLOTS  (s() terms only)
# Partial effect + 95% CI ribbon, rug of training observations
# =============================================================================

cat("\nPlotting 1D marginal smooths...\n")

for (s_label in s_labels) {
   
   # Parse variable name from label, e.g. "s(RollingDischarge30)" or
   # "s(RollingWindCross7):WindDirRightBank"
   inner    <- gsub("^s\\(|\\).*$", "", s_label)
   var_name <- trimws(strsplit(inner, ",")[[1]])[1]
   
   # Identify by-level if present
   by_level <- NULL
   if (grepl(":", s_label)) {
      by_level <- sub(".*:", "", s_label)
      # Strip factor variable name prefix from level string
      wind_smooth <- Filter(function(s) s$label == s_label, gam_obj$smooth)
      if (length(wind_smooth) > 0) by_level <- wind_smooth[[1]]$by.level
   }
   
   if (!var_name %in% names(stacked_train)) next
   
   # Build newdata
   x_range <- range(stacked_train[[var_name]], na.rm = TRUE)
   newdata  <- data.frame(x = seq(x_range[1], x_range[2], length.out = 200))
   names(newdata) <- var_name
   
   for (v in model_vars) {
      if (v %in% names(newdata)) next
      if (v == "h") {
         newdata$h <- median(stacked_train$h, na.rm = TRUE)
      } else if (v == "WindDir") {
         if (!is.null(by_level)) {
            newdata$WindDir <- factor(by_level, levels = levels(stacked_train$WindDir))
         } else {
            newdata$WindDir <- factor(levels(stacked_train$WindDir)[1],
                                      levels = levels(stacked_train$WindDir))
         }
      } else if (v %in% names(stacked_train)) {
         newdata[[v]] <- median(stacked_train[[v]], na.rm = TRUE)
      }
   }
   
   preds <- tryCatch(
      predict(gam_obj, newdata = newdata, se.fit = TRUE, type = "terms"),
      error = function(e) { cat("  Skipping", s_label, ":", e$message, "\n"); NULL }
   )
   if (is.null(preds)) next
   
   col_match <- which(colnames(preds$fit) == s_label)
   if (length(col_match) == 0) next
   
   pred_df <- data.frame(
      x     = newdata[[var_name]],
      fit   = preds$fit[, col_match],
      se    = preds$se.fit[, col_match]
   ) %>% mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
   
   rug_data <- stacked_train[[var_name]]
   
   plot_title <- if (!is.null(by_level)) {
      paste0("s(", var_name, ") \u2014 ", by_level)
   } else {
      paste0("s(", var_name, ")")
   }
   
   p_smooth <- ggplot(pred_df, aes(x = x, y = fit)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = gam_colors$dark, linewidth = 0.5) +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = gam_colors$secondary, alpha = 0.25) +
      geom_line(color = gam_colors$primary, linewidth = 1.1) +
      geom_rug(data = data.frame(x = rug_data), aes(x = x),
               inherit.aes = FALSE, sides = "b",
               alpha = 0.2, color = gam_colors$dark) +
      labs(title = plot_title,
           x     = var_name,
           y     = "Partial Effect (psu)") +
      theme_eval()
   
   safe_name <- gsub("[^A-Za-z0-9_]", "", s_label)
   save_plot_dir(p_smooth, smooth_dir, paste0("Smooth_", safe_name), w = 8, h = 5)
   cat("  Saved smooth:", s_label, "\n")
}

# =============================================================================
# TENSOR SURFACE PLOTS
#
# For each ti(h, predictor) term two surfaces are produced:
#
# (A) FULL CONDITIONAL SURFACE
#     Predict over grid of (h, predictor), all other vars at training median.
#     Subtract the prediction at median predictor value to get effect relative
#     to median conditions in PSU. This is the headline physical interpretation.
#     Sequential colorscale: blue (below median) -> orange (above median).
#
# (B) PARTIAL INTERACTION SURFACE
#     Extract the ti() column from predict(..., type='terms').
#     Shows how horizon modulates the predictor's marginal effect.
#     Diverging colorscale: blue -> white -> orange, centered at zero.
#     Values correctly small — this is the interaction residual after main
#     effects are absorbed.
# =============================================================================

cat("\nPlotting tensor surfaces...\n")

for (ti_label in ti_labels) {
   
   inner    <- gsub("ti\\(|\\)", "", ti_label)
   vars_in  <- trimws(strsplit(inner, ",")[[1]])
   pred_var <- vars_in[vars_in != "h"]
   
   if (length(pred_var) != 1) next
   if (!pred_var %in% names(stacked_train)) next
   
   h_seq    <- 1:H_MAX
   pred_seq <- seq(quantile(stacked_train[[pred_var]], 0.02, na.rm = TRUE),
                   quantile(stacked_train[[pred_var]], 0.98, na.rm = TRUE),
                   length.out = 60)
   
   grid <- make_pred_grid(pred_var, h_seq, pred_seq,
                          stacked_train, model_vars, gam_obj)
   
   # --- (A) FULL CONDITIONAL SURFACE ---
   
   # Grid at varying (h, pred_var)
   pred_full <- tryCatch(
      predict(gam_obj, newdata = grid, type = "response"),
      error = function(e) { cat("  Skipping full conditional for", pred_var, "\n"); NULL }
   )
   
   if (!is.null(pred_full)) {
      
      # Reference grid: same h, predictor fixed at its median
      grid_ref <- grid
      grid_ref[[pred_var]] <- median(stacked_train[[pred_var]], na.rm = TRUE)
      pred_ref  <- predict(gam_obj, newdata = grid_ref, type = "response")
      
      grid$CondEffect <- as.numeric(pred_full) - as.numeric(pred_ref)
      
      # Symmetric color limit so zero is centered
      clim <- max(abs(grid$CondEffect), na.rm = TRUE)
      
      p_cond <- ggplot(grid, aes(x = h, y = .data[[pred_var]], fill = CondEffect)) +
         geom_tile() +
         scale_fill_gradient2(low      = gam_colors$secondary,
                              mid      = "white",
                              high     = gam_colors$primary,
                              midpoint = 0,
                              limits   = c(-clim, clim),
                              name     = "Effect\nrelative to\nmedian (psu)") +
         scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
         labs(title    = paste0(pred_var, " \u2014 Full Conditional Effect"),
              subtitle = "Predicted salinity relative to median predictor value; all other vars at training median",
              x        = "Lead Time (days)",
              y        = pred_var) +
         theme_eval()
      
      safe_name <- gsub("[^A-Za-z0-9_]", "", pred_var)
      save_plot_dir(p_cond, tensor_dir,
                    paste0("TensorFull_", safe_name), w = 10, h = 6)
      cat("  Saved full conditional surface for", pred_var, "\n")
   }
   
   # --- (B) PARTIAL INTERACTION SURFACE ---
   
   term_matrix <- tryCatch(
      predict(gam_obj, newdata = grid, type = "terms"),
      error = function(e) { cat("  Skipping partial for", pred_var, "\n"); NULL }
   )
   
   if (!is.null(term_matrix)) {
      
      col_matches  <- grep(pred_var, colnames(term_matrix), value = TRUE)
      ti_col_match <- col_matches[grepl("^ti", col_matches)][1]
      
      if (!is.na(ti_col_match)) {
         
         grid$PartialEffect <- as.numeric(term_matrix[, ti_col_match])
         plim <- max(abs(grid$PartialEffect), na.rm = TRUE)
         
         p_partial <- ggplot(grid, aes(x = h, y = .data[[pred_var]], fill = PartialEffect)) +
            geom_tile() +
            scale_fill_gradient2(low      = gam_colors$secondary,
                                 mid      = "white",
                                 high     = gam_colors$primary,
                                 midpoint = 0,
                                 limits   = c(-plim, plim),
                                 name     = "Partial\nInteraction\nEffect (psu)") +
            scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
            labs(title    = paste0("ti(h, ", pred_var, ") \u2014 Partial Interaction Effect"),
                 subtitle = "Horizon-modulated deviation from marginal average; correctly small by construction",
                 x        = "Lead Time (days)",
                 y        = pred_var) +
            theme_eval()
         
         safe_name <- gsub("[^A-Za-z0-9_]", "", pred_var)
         save_plot_dir(p_partial, tensor_dir,
                       paste0("TensorPartial_", safe_name), w = 10, h = 6)
         cat("  Saved partial interaction surface for", pred_var, "\n")
      }
   }
}

# =============================================================================
# OCTOBER 2016 EVENT
# =============================================================================

obs_daily <- stacked_train %>%
   filter(h == 1,
          as.Date(DateTime) >= as.Date(EVENT_DATE_RANGE[1]),
          as.Date(DateTime) <= as.Date(EVENT_DATE_RANGE[2])) %>%
   select(DateTime, Salinity_h) %>%
   rename(Observed = Salinity_h) %>%
   mutate(DateTime = as.Date(DateTime))

horizons_to_plot <- c(1, 3, 7, 10, 14, 20)

event_plot_df <- stacked_train %>%
   filter(h %in% horizons_to_plot,
          as.Date(DateTime) >= as.Date(EVENT_DATE_RANGE[1]),
          as.Date(DateTime) <= as.Date(EVENT_DATE_RANGE[2]),
          !is.na(Predicted)) %>%
   mutate(DateTime = as.Date(DateTime))

# Blue-to-orange ramp for increasing lead time
horizon_colors <- colorRampPalette(c(gam_colors$secondary, gam_colors$primary))(length(horizons_to_plot))

p_event <- ggplot() +
   geom_hline(yintercept = HIGH_SALINITY_THRESHOLD, linetype = "dashed",
              color = gam_colors$dark, linewidth = 0.6) +
   geom_line(data  = obs_daily,
             aes(x = DateTime, y = Observed),
             color = gam_colors$dark, linewidth = 1.1) +
   geom_line(data  = event_plot_df,
             aes(x = DateTime, y = Predicted,
                 color = factor(h), group = factor(h)),
             linewidth = 0.85, alpha = 0.9) +
   scale_color_manual(values = setNames(horizon_colors, as.character(horizons_to_plot)),
                      labels = paste0("h = ", horizons_to_plot, " days"),
                      name   = "Lead Time") +
   scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
   labs(title    = "October 2016 Salinity Intrusion Event",
        subtitle = "Observed (dark) vs model forecasts at multiple lead times; dashed = threshold",
        x        = NULL,
        y        = "Salinity (psu)") +
   theme_eval() +
   theme(legend.position = "right",
         axis.text.x     = element_text(angle = 45, hjust = 1))

save_plot(p_event, "Oct2016Event", w = 12, h = 7)

# =============================================================================
# WRITE PERFORMANCE TABLE
# =============================================================================

write_qs_files(
   list(perf_hold),
   'Outputs/Experiments/Models/UnifiedGAM',
   list('HoldoutPerformanceByH')
)

cat("\nScript 05 complete. Plots saved to:", base_dir, "\n")

rm(list = ls())