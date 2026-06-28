# =============================================================================
# GamEvaluationPlots.R
# Project:   Chapter1
# Author:    Ethan Heidtman
# Purpose:   All helper functions and plotting functions for unified GAM
#            evaluation. Sourced by 05_EvaluateUnifiedGAM.R; no side effects
#            on load. Requires gam_colors and base_dir to be defined in the
#            calling script before source() is called.
#
# Functions:
#   theme_eval()
#   save_plot(p, name, w, h)
#   save_plot_dir(p, dir, name, w, h)
#   add_wind_dir(df, gam_obj)
#   resolve_predictor_color(var_name, predictor_colors, fallback)
#   make_pred_grid(pred_var, h_seq, pred_seq, train_df, model_vars, gam_obj,
#                  wind_var_name, reference_wind_level, reference_wind_value)
#   extract_ti_column(term_matrix, ti_label)
#   plot_performance_metrics(perf_hold, H_MAX, gam_colors, base_dir)
#   plot_residual_diagnostics(stacked_hold, H_MAX, HIGH_SALINITY_THRESHOLD,
#                              gam_colors, base_dir)
#   plot_calibration(stacked_hold, N_CAL_BINS, gam_colors, base_dir)
#   plot_acf_pacf(stacked_hold, H_MAX, gam_colors, acf_dir)
#   plot_1d_smooths(gam_obj, s_labels, stacked_train, model_vars,
#                   wind_var_name, reference_wind_level, reference_wind_value,
#                   predictor_colors, smooth_dir)
#   plot_tensor_partial(gam_obj, ti_labels, stacked_train, model_vars,
#                       wind_var_name, reference_wind_level, reference_wind_value,
#                       predictor_colors, tensor_negative_color, H_MAX,
#                       tensor_partial_dir)
#   plot_tensor_full(gam_obj, ti_labels, stacked_train, model_vars,
#                    wind_var_name, reference_wind_level, reference_wind_value,
#                    predictor_colors, tensor_negative_color, H_MAX,
#                    tensor_full_dir)
#   plot_tensor_slices(gam_obj, ti_labels, stacked_train, model_vars,
#                      wind_var_name, reference_wind_level, reference_wind_value,
#                      predictor_colors, H_MAX, n_slices, slice_dir)
#   plot_tensor_derivatives(gam_obj, ti_labels, stacked_train, model_vars,
#                           wind_var_name, reference_wind_level,
#                           reference_wind_value, predictor_colors,
#                           H_MAX, n_slices, deriv_dir)
#   plot_salinity_forecast_panels(data, date_range, year, horizons,
#                                  epa_line, threshold, title)
# =============================================================================


# =============================================================================
# SHARED THEME AND SAVE HELPERS
# theme_eval() reads gam_colors from the calling environment (defined in
# script 05 before source() is called).
# =============================================================================

theme_eval <- function() {
   theme_bw() +
      theme(
         plot.title        = element_text(size = 14, face = "bold",  color = gam_colors$dark),
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
# HELPER: add WindDir consistent with fit_gam convention
# =============================================================================

add_wind_dir <- function(df, gam_obj) {
   wind_var <- setdiff(all.vars(formula(gam_obj)),
                       c("Response", "h", "LagSalinity", "RollingDischarge30",
                         "MaxDischarge10", "TideMean30", "WindDir"))
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


# =============================================================================
# HELPER: resolve predictor color by prefix match
# Exact name matching breaks when window suffixes change (MaxDischarge9 ->
# MaxDischarge10, RollingWindCross7 -> RollingWindCross12). Matches on the
# longest key in predictor_colors that is a prefix of var_name.
# =============================================================================

resolve_predictor_color <- function(var_name, predictor_colors, fallback = "#888888") {
   keys       <- names(predictor_colors)
   match_mask <- startsWith(var_name, keys)
   if (!any(match_mask)) return(fallback)
   best_key   <- keys[match_mask][which.max(nchar(keys[match_mask]))]
   predictor_colors[[best_key]]
}


# =============================================================================
# HELPER: build a median-filled prediction grid
# See detailed WindDir handling notes in script 05 header.
# =============================================================================

make_pred_grid <- function(pred_var, h_seq, pred_seq, train_df, model_vars,
                           gam_obj, wind_var_name,
                           reference_wind_level, reference_wind_value) {
   
   grid           <- expand.grid(h = h_seq, .x = pred_seq)
   names(grid)[2] <- pred_var
   is_wind_grid   <- (pred_var == wind_var_name)
   wind_levels    <- levels(train_df$WindDir)
   
   for (v in model_vars) {
      if (v %in% names(grid)) next
      if (v == "WindDir") {
         if (is_wind_grid) {
            grid$WindDir <- factor(
               ifelse(grid[[pred_var]] >= 0, wind_levels[2], wind_levels[1]),
               levels = wind_levels
            )
         } else {
            grid$WindDir <- factor(reference_wind_level, levels = wind_levels)
         }
      } else if (v == wind_var_name && !is_wind_grid) {
         grid[[v]] <- reference_wind_value
      } else if (v %in% names(train_df)) {
         if (is.factor(train_df[[v]])) {
            grid[[v]] <- factor(levels(train_df[[v]])[1], levels = levels(train_df[[v]]))
         } else {
            grid[[v]] <- median(train_df[[v]], na.rm = TRUE)
         }
      }
   }
   grid
}


# =============================================================================
# HELPER: extract partial ti() column from a terms matrix by exact label match.
# grep(pred_var, ...) can match s() columns or other predictors sharing a
# substring — exact match on the full ti_label is required.
# =============================================================================

extract_ti_column <- function(term_matrix, ti_label) {
   col_idx <- which(colnames(term_matrix) == ti_label)
   if (length(col_idx) == 0) {
      warning("No exact column match for '", ti_label, "' in term matrix. ",
              "Available: ", paste(colnames(term_matrix), collapse = ", "))
      return(NULL)
   }
   as.numeric(term_matrix[, col_idx[1]])
}


# =============================================================================
# PERFORMANCE METRICS PLOT
# Single function producing: combined error plot (RMSE + MAE, dashed =
# high-salinity subset), standalone R2 plot, bias plot, and a combined
# patchwork panel saved to base_dir.
# NSE is retained in perf_hold (passed in from script 05) but not plotted.
# =============================================================================

plot_performance_metrics <- function(perf_hold, H_MAX, gam_colors, base_dir) {
   
   # ---- Combined error: RMSE + MAE, solid = overall, dashed = high-salinity ----
   p_error <- perf_hold %>%
      select(LeadTime, RMSE, MAE, RMSE_High, MAE_High) %>%
      pivot_longer(-LeadTime, names_to = "Stat", values_to = "Value") %>%
      mutate(
         Metric = ifelse(grepl("^RMSE", Stat), "RMSE", "MAE"),
         Subset = ifelse(grepl("_High$", Stat), "High Salinity", "Overall")
      ) %>%
      ggplot(aes(x = LeadTime, y = Value, color = Metric,
                 linetype = Subset, group = Stat)) +
      geom_line(linewidth = 1.2) +
      geom_point(data = ~ filter(.x, Subset == "Overall"), size = 3) +
      scale_color_manual(values = c("RMSE" = gam_colors$secondary,
                                    "MAE"  = gam_colors$primary),
                         name = "Metric") +
      scale_linetype_manual(values = c("Overall" = "solid", "High Salinity" = "dashed"),
                            name = "Subset") +
      scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
      labs(title = "Forecast Error by Lead Time \u2014 Holdout",
           x     = "Lead Time (days)",
           y     = "Error (ppt)") +
      theme_eval() +
      theme(legend.position = "bottom")
   
   # ---- R2 ----
   p_r2 <- perf_hold %>%
      ggplot(aes(x = LeadTime, y = R2)) +
      geom_line(linewidth = 1.2, color = gam_colors$secondary) +
      geom_point(size = 3, color = gam_colors$secondary) +
      scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
      labs(title = "R\u00b2 by Lead Time \u2014 Holdout",
           x     = "Lead Time (days)",
           y     = expression(R^2)) +
      theme_eval()
   
   # ---- Bias ----
   p_bias <- perf_hold %>%
      ggplot(aes(x = LeadTime, y = Bias)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = gam_colors$dark, linewidth = 0.6) +
      geom_line(linewidth = 1.2, color = gam_colors$primary) +
      geom_point(size = 3, color = gam_colors$primary) +
      scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
      labs(title = "Bias by Lead Time \u2014 Holdout",
           x     = "Lead Time (days)",
           y     = "Bias (ppt)") +
      theme_eval()
   
   # ---- Combined panel ----
   p_combined <- (p_error | p_r2) / p_bias +
      plot_annotation(
         title = "Forecast Performance by Lead Time \u2014 Holdout",
         theme = theme(plot.title = element_text(size = 16, face = "bold",
                                                 color = gam_colors$dark))
      )
   
   save_plot(p_error,    "Error_ByLeadTime", w = 10, h = 6)
   save_plot(p_r2,       "R2_ByLeadTime",    w = 10, h = 6)
   save_plot(p_bias,     "Bias_ByLeadTime",  w = 10, h = 6)
   ggsave(file.path(base_dir, "Performance_Combined.png"),
          plot = p_combined, width = 14, height = 10, dpi = 600)
   ggsave(file.path(base_dir, "Performance_Combined.svg"),
          plot = p_combined, width = 14, height = 10)
   
   invisible(list(error = p_error, r2 = p_r2, bias = p_bias, combined = p_combined))
}


# =============================================================================
# RESIDUAL DIAGNOSTICS
# Observed vs predicted (overall and by h-bin), QQ plots, residual
# histogram, residuals vs fitted — all faceted by lead-time bin.
# =============================================================================

plot_residual_diagnostics <- function(stacked_hold, H_MAX,
                                      HIGH_SALINITY_THRESHOLD,
                                      gam_colors, base_dir) {
   
   h_breaks <- c(0, 5, 10, 15, 20, 25, 30)
   h_labels <- c("h = 1\u20135", "h = 6\u201310", "h = 11\u201315",
                 "h = 16\u201320", "h = 21\u201325", "h = 26\u201330")
   
   resid_df <- stacked_hold %>%
      filter(!is.na(Residual)) %>%
      mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels))
   
   p_obs_pred <- stacked_hold %>%
      filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
      mutate(HighSal = Salinity_h > HIGH_SALINITY_THRESHOLD) %>%
      ggplot(aes(x = Predicted, y = Salinity_h, color = HighSal)) +
      geom_point(alpha = 0.3, size = 0.8) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = gam_colors$dark) +
      scale_color_manual(values = c("FALSE" = gam_colors$secondary,
                                    "TRUE"  = gam_colors$primary),
                         labels = c("Normal", "High Salinity"), name = NULL) +
      labs(title = "Observed vs Predicted \u2014 Holdout",
           x     = "Predicted (ppt)",
           y     = "Observed (ppt)") +
      theme_eval() +
      theme(legend.position = "bottom")
   
   p_obs_pred_h <- stacked_hold %>%
      filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
      mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels)) %>%
      ggplot(aes(x = Predicted, y = Salinity_h)) +
      geom_point(alpha = 0.25, size = 0.7, color = gam_colors$secondary) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = gam_colors$dark) +
      facet_wrap(~ HBin) +
      labs(title = "Observed vs Predicted by Lead Time Bin \u2014 Holdout",
           x     = "Predicted (ppt)",
           y     = "Observed (ppt)") +
      theme_eval()
   
   p_qq <- ggplot(resid_df, aes(sample = Residual)) +
      stat_qq(size = 0.6, alpha = 0.4, color = gam_colors$secondary) +
      stat_qq_line(color = gam_colors$dark, linetype = "dashed") +
      facet_wrap(~ HBin) +
      labs(title = "Q-Q Plot of Residuals by Lead Time Bin",
           x     = "Theoretical Quantiles",
           y     = "Sample Quantiles") +
      theme_eval()
   
   p_resid_hist <- ggplot(resid_df, aes(x = Residual)) +
      geom_histogram(bins = 50, fill = gam_colors$secondary, alpha = 0.85,
                     color = gam_colors$dark, linewidth = 0.2) +
      facet_wrap(~ HBin, scales = "free_y") +
      labs(title = "Residual Distribution by Lead Time Bin",
           x     = "Residual (ppt)",
           y     = "Count") +
      theme_eval()
   
   p_resid_fitted <- ggplot(resid_df, aes(x = Predicted, y = Residual)) +
      geom_point(alpha = 0.25, size = 0.7, color = gam_colors$secondary) +
      geom_hline(yintercept = 0, linetype = "dashed", color = gam_colors$dark) +
      geom_smooth(method = "loess", se = FALSE, color = gam_colors$primary,
                  linewidth = 0.9, span = 0.4) +
      facet_wrap(~ HBin) +
      labs(title = "Residuals vs Fitted by Lead Time Bin",
           x     = "Fitted (ppt)",
           y     = "Residual (ppt)") +
      theme_eval()
   
   save_plot(p_obs_pred,      "ObsVsPred",             w = 8,  h = 7)
   save_plot(p_obs_pred_h,    "ObsVsPred_ByHBin",      w = 10, h = 8)
   save_plot(p_qq,            "QQ_ByHBin",             w = 10, h = 8)
   save_plot(p_resid_hist,    "ResidHist_ByHBin",      w = 10, h = 8)
   save_plot(p_resid_fitted,  "ResidVsFitted_ByHBin",  w = 10, h = 8)
   
   invisible(NULL)
}


# =============================================================================
# CALIBRATION PLOT
# =============================================================================

plot_calibration <- function(stacked_hold, N_CAL_BINS, gam_colors, base_dir) {
   
   h_breaks <- c(0, 5, 10, 15, 20, 25, 30)
   h_labels <- c("h = 1\u20135", "h = 6\u201310", "h = 11\u201315",
                 "h = 16\u201320", "h = 21\u201325", "h = 26\u201330")
   
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
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = gam_colors$dark) +
      geom_point(color = gam_colors$secondary, alpha = 0.85) +
      facet_wrap(~ HBin) +
      scale_size_continuous(range = c(2, 6), name = "N") +
      coord_fixed(xlim = cal_range, ylim = cal_range) +
      labs(title = "Calibration by Lead Time Bin \u2014 Holdout",
           x     = "Mean Predicted (ppt)",
           y     = "Mean Observed (ppt)") +
      theme_eval()
   
   save_plot(p_cal, "Calibration_ByHBin", w = 10, h = 8)
   invisible(p_cal)
}


# =============================================================================
# ACF / PACF BY H-SLICE
# =============================================================================

plot_acf_pacf <- function(stacked_hold, H_MAX, gam_colors, acf_dir) {
   
   ci_line      <- qnorm(0.975) / sqrt(nrow(stacked_hold) / H_MAX)
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
      
      acf_records[[hval]]  <- data.frame(h   = hval,
                                         Lag = as.numeric(acf_obj$lag[-1]),
                                         ACF = as.numeric(acf_obj$acf[-1]))
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
      labs(title = "Residual ACF by Lead Time", x = "Lag", y = "ACF") +
      theme_eval() +
      theme(axis.text = element_text(size = 7))
   
   p_pacf <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
      geom_col(fill = gam_colors$primary, alpha = 0.85, width = 0.7) +
      geom_hline(yintercept = c(-ci_line, ci_line),
                 linetype = "dashed", color = gam_colors$dark) +
      geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.3) +
      facet_wrap(~ paste0("h = ", h), ncol = 5) +
      labs(title = "Residual PACF by Lead Time", x = "Lag", y = "PACF") +
      theme_eval() +
      theme(axis.text = element_text(size = 7))
   
   ggsave(file.path(acf_dir, "ACF_AllH.png"),
          plot = p_acf,  width = 16, height = 14, dpi = 600)
   ggsave(file.path(acf_dir, "PACF_AllH.png"),
          plot = p_pacf, width = 16, height = 14, dpi = 600)
   
   invisible(NULL)
}


# =============================================================================
# 1D MARGINAL SMOOTH PLOTS
# Wind smooths: single combined figure, each by-level curve restricted to its
# own valid sign range. Both curves in wind predictor color (purple); level
# distinguished by x-range and inline label — caption will clarify fitting.
# All other s() terms: one figure each, colored by predictor via prefix match.
# s(h): dark (not a predictor color).
# =============================================================================

plot_1d_smooths <- function(gam_obj, s_labels, stacked_train, model_vars,
                            wind_var_name, reference_wind_level,
                            reference_wind_value, predictor_colors,
                            smooth_dir,
                            h_fallback_color = "#002030") {
   
   wind_s_labels  <- s_labels[grepl(paste0("^s\\(", wind_var_name, "\\)"), s_labels)]
   other_s_labels <- setdiff(s_labels, wind_s_labels)
   wind_levels    <- levels(stacked_train$WindDir)
   wind_color     <- resolve_predictor_color(wind_var_name, predictor_colors)
   
   predict_terms_safe <- function(newdata, label) {
      tryCatch(
         predict(gam_obj, newdata = newdata, se.fit = TRUE, type = "terms"),
         error = function(e) { message("  Skipping smooth ", label, ": ", e$message); NULL }
      )
   }
   
   # ---- (A) Combined wind smooth ----
   if (length(wind_s_labels) > 0) {
      
      wind_curves <- lapply(wind_s_labels, function(s_label) {
         obj <- Filter(function(s) s$label == s_label, gam_obj$smooth)
         if (length(obj) == 0) return(NULL)
         by_level    <- obj[[1]]$by.level
         level_range <- range(stacked_train[[wind_var_name]][stacked_train$WindDir == by_level],
                              na.rm = TRUE)
         x_seq   <- seq(level_range[1], level_range[2], length.out = 200)
         newdata <- setNames(data.frame(x_seq), wind_var_name)
         newdata$h       <- median(stacked_train$h, na.rm = TRUE)
         newdata$WindDir <- factor(by_level, levels = wind_levels)
         for (v in model_vars) {
            if (v %in% names(newdata) || v == wind_var_name) next
            if (v %in% names(stacked_train) && !is.factor(stacked_train[[v]]))
               newdata[[v]] <- median(stacked_train[[v]], na.rm = TRUE)
         }
         preds     <- predict_terms_safe(newdata, s_label)
         if (is.null(preds)) return(NULL)
         col_match <- which(colnames(preds$fit) == s_label)
         if (length(col_match) == 0) return(NULL)
         data.frame(x = newdata[[wind_var_name]],
                    fit = preds$fit[, col_match],
                    se  = preds$se.fit[, col_match],
                    Level = by_level)
      })
      
      wind_df <- do.call(rbind, Filter(Negate(is.null), wind_curves))
      
      if (!is.null(wind_df) && nrow(wind_df) > 0) {
         wind_df <- wind_df %>%
            mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
         
         label_df <- wind_df %>%
            group_by(Level) %>%
            slice_max(abs(x), n = 1) %>%
            ungroup()
         
         p_wind <- ggplot(wind_df, aes(x = x, y = fit, group = Level)) +
            geom_hline(yintercept = 0, linetype = "dashed",
                       color = "#002030", linewidth = 0.5) +
            geom_vline(xintercept = 0, linetype = "dotted",
                       color = "#002030", linewidth = 0.4) +
            geom_ribbon(aes(ymin = lower, ymax = upper),
                        fill = wind_color, alpha = 0.18, color = NA) +
            geom_line(color = wind_color, linewidth = 1.1) +
            geom_text(data = label_df,
                      aes(label = Level),
                      hjust = 0,
                      nudge_x = 0.02 * diff(range(wind_df$x)),
                      color = wind_color, size = 3.5, fontface = "italic") +
            geom_rug(data = data.frame(x = stacked_train[[wind_var_name]]),
                     aes(x = x), inherit.aes = FALSE,
                     sides = "b", alpha = 0.12, color = "#002030") +
            labs(title = paste0("s(", wind_var_name, ") by WindDir"),
                 x     = wind_var_name,
                 y     = "Partial Effect (ppt)") +
            theme_eval() +
            theme(legend.position = "none")
         
         save_plot_dir(p_wind, smooth_dir,
                       paste0("Smooth_", wind_var_name, "_Combined"), w = 8, h = 5)
         message("  Saved combined wind smooth")
      }
   }
   
   # ---- (B) All other 1D smooths ----
   for (s_label in other_s_labels) {
      
      inner    <- gsub("^s\\(|\\).*$", "", s_label)
      var_name <- trimws(strsplit(inner, ",")[[1]])[1]
      if (!var_name %in% names(stacked_train)) next
      
      line_color <- if (var_name == "h") {
         h_fallback_color
      } else {
         resolve_predictor_color(var_name, predictor_colors, fallback = "#888888")
      }
      
      x_range <- range(stacked_train[[var_name]], na.rm = TRUE)
      newdata  <- setNames(
         data.frame(seq(x_range[1], x_range[2], length.out = 200)),
         var_name
      )
      for (v in model_vars) {
         if (v %in% names(newdata)) next
         if (v == "h")           { newdata$h <- median(stacked_train$h, na.rm = TRUE); next }
         if (v == "WindDir")     { newdata$WindDir <- factor(reference_wind_level,
                                                             levels = levels(stacked_train$WindDir)); next }
         if (v == wind_var_name) { newdata[[v]] <- reference_wind_value; next }
         if (v %in% names(stacked_train))
            newdata[[v]] <- median(stacked_train[[v]], na.rm = TRUE)
      }
      
      preds     <- predict_terms_safe(newdata, s_label)
      if (is.null(preds)) next
      col_match <- which(colnames(preds$fit) == s_label)
      if (length(col_match) == 0) next
      
      pred_df <- data.frame(
         x   = newdata[[var_name]],
         fit = preds$fit[, col_match],
         se  = preds$se.fit[, col_match]
      ) %>% mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
      
      p_smooth <- ggplot(pred_df, aes(x = x, y = fit)) +
         geom_hline(yintercept = 0, linetype = "dashed",
                    color = "#002030", linewidth = 0.5) +
         geom_ribbon(aes(ymin = lower, ymax = upper),
                     fill = line_color, alpha = 0.2) +
         geom_line(color = line_color, linewidth = 1.1) +
         geom_rug(data = data.frame(x = stacked_train[[var_name]]),
                  aes(x = x), inherit.aes = FALSE,
                  sides = "b", alpha = 0.2, color = "#002030") +
         labs(title = paste0("s(", var_name, ")"),
              x     = var_name,
              y     = "Partial Effect (ppt)") +
         theme_eval()
      
      safe_name <- gsub("[^A-Za-z0-9_]", "", s_label)
      save_plot_dir(p_smooth, smooth_dir, paste0("Smooth_", safe_name), w = 8, h = 5)
      message("  Saved smooth: ", s_label)
   }
}


# =============================================================================
# TENSOR PARTIAL SURFACE  [ti() term only]
# =============================================================================

plot_tensor_partial <- function(gam_obj, ti_labels, stacked_train, model_vars,
                                wind_var_name, reference_wind_level,
                                reference_wind_value, predictor_colors,
                                tensor_negative_color = "#002030",
                                H_MAX, tensor_partial_dir) {
   
   for (ti_label in ti_labels) {
      inner    <- gsub("ti\\(|\\)", "", ti_label)
      vars_in  <- trimws(strsplit(inner, ",")[[1]])
      pred_var <- vars_in[vars_in != "h"]
      if (length(pred_var) != 1 || !pred_var %in% names(stacked_train)) next
      
      pos_color <- resolve_predictor_color(pred_var, predictor_colors)
      h_seq     <- 1:H_MAX
      pred_seq  <- seq(quantile(stacked_train[[pred_var]], 0.02, na.rm = TRUE),
                       quantile(stacked_train[[pred_var]], 0.98, na.rm = TRUE),
                       length.out = 60)
      
      grid <- make_pred_grid(pred_var, h_seq, pred_seq, stacked_train,
                             model_vars, gam_obj, wind_var_name,
                             reference_wind_level, reference_wind_value)
      
      term_matrix <- tryCatch(
         predict(gam_obj, newdata = grid, type = "terms"),
         error = function(e) { message("  Skipping partial for ", pred_var); NULL }
      )
      if (is.null(term_matrix)) next
      
      partial_vals <- extract_ti_column(term_matrix, ti_label)
      if (is.null(partial_vals)) next
      
      grid$PartialEffect <- partial_vals
      plim <- max(abs(grid$PartialEffect), na.rm = TRUE)
      
      p <- ggplot(grid, aes(x = h, y = .data[[pred_var]], fill = PartialEffect)) +
         geom_tile() +
         scale_fill_gradient2(low = tensor_negative_color, mid = "white",
                              high = pos_color, midpoint = 0,
                              limits = c(-plim, plim),
                              name = "Partial\nInteraction\n(ppt)") +
         scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
         labs(title = paste0("ti(h, ", pred_var, ") \u2014 Partial Interaction Effect"),
              x = "Lead Time (days)", y = pred_var) +
         theme_eval()
      
      safe_name <- gsub("[^A-Za-z0-9_]", "", pred_var)
      save_plot_dir(p, tensor_partial_dir, paste0("TensorPartial_", safe_name), w = 10, h = 6)
      message("  Partial surface: ", pred_var, " | max |effect| = ", round(plim, 4), " ppt")
   }
}


# =============================================================================
# TENSOR FULL CONDITIONAL SURFACE
# =============================================================================

plot_tensor_full <- function(gam_obj, ti_labels, stacked_train, model_vars,
                             wind_var_name, reference_wind_level,
                             reference_wind_value, predictor_colors,
                             tensor_negative_color = "#002030",
                             H_MAX, tensor_full_dir) {
   
   for (ti_label in ti_labels) {
      inner    <- gsub("ti\\(|\\)", "", ti_label)
      vars_in  <- trimws(strsplit(inner, ",")[[1]])
      pred_var <- vars_in[vars_in != "h"]
      if (length(pred_var) != 1 || !pred_var %in% names(stacked_train)) next
      
      pos_color <- resolve_predictor_color(pred_var, predictor_colors)
      h_seq     <- 1:H_MAX
      pred_seq  <- seq(quantile(stacked_train[[pred_var]], 0.02, na.rm = TRUE),
                       quantile(stacked_train[[pred_var]], 0.98, na.rm = TRUE),
                       length.out = 60)
      
      grid <- make_pred_grid(pred_var, h_seq, pred_seq, stacked_train,
                             model_vars, gam_obj, wind_var_name,
                             reference_wind_level, reference_wind_value)
      
      pred_full <- tryCatch(
         predict(gam_obj, newdata = grid, type = "response"),
         error = function(e) { message("  Skipping full conditional for ", pred_var); NULL }
      )
      if (is.null(pred_full)) next
      
      grid_ref             <- grid
      grid_ref[[pred_var]] <- median(stacked_train[[pred_var]], na.rm = TRUE)
      pred_ref             <- predict(gam_obj, newdata = grid_ref, type = "response")
      grid$CondEffect      <- as.numeric(pred_full) - as.numeric(pred_ref)
      clim                 <- max(abs(grid$CondEffect), na.rm = TRUE)
      
      p <- ggplot(grid, aes(x = h, y = .data[[pred_var]], fill = CondEffect)) +
         geom_tile() +
         scale_fill_gradient2(low = tensor_negative_color, mid = "white",
                              high = pos_color, midpoint = 0,
                              limits = c(-clim, clim),
                              name = "Effect vs.\nmedian (ppt)") +
         scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
         labs(title = paste0(pred_var, " \u2014 Full Conditional Effect"),
              x = "Lead Time (days)", y = pred_var) +
         theme_eval()
      
      safe_name <- gsub("[^A-Za-z0-9_]", "", pred_var)
      save_plot_dir(p, tensor_full_dir, paste0("TensorFull_", safe_name), w = 10, h = 6)
      message("  Full conditional: ", pred_var, " | max |effect| = ", round(clim, 4), " ppt")
   }
}


# =============================================================================
# TENSOR SLICE PLOTS
# =============================================================================

plot_tensor_slices <- function(gam_obj, ti_labels, stacked_train, model_vars,
                               wind_var_name, reference_wind_level,
                               reference_wind_value, predictor_colors,
                               H_MAX, n_slices = 3, slice_dir) {
   
   h_seq      <- 1:H_MAX
   alphas     <- seq(0.45, 1.0, length.out = n_slices)
   slice_prob <- seq(0.02, 0.98, length.out = n_slices)
   
   build_slice_df <- function(pred_var, pred_vals, wind_level = NULL) {
      slices <- lapply(names(pred_vals), function(lbl) {
         pv   <- pred_vals[[lbl]]
         grid <- make_pred_grid(pred_var, h_seq, pv, stacked_train,
                                model_vars, gam_obj, wind_var_name,
                                reference_wind_level, reference_wind_value)
         if (!is.null(wind_level)) {
            grid$WindDir          <- factor(wind_level, levels = levels(stacked_train$WindDir))
            grid[[wind_var_name]] <- pv
         }
         tm <- tryCatch(predict(gam_obj, newdata = grid, type = "terms"), error = function(e) NULL)
         if (is.null(tm)) return(NULL)
         ti_lbl <- paste0("ti(h,", pred_var, ")")
         pv_col <- extract_ti_column(tm, ti_lbl)
         if (is.null(pv_col)) return(NULL)
         data.frame(h = h_seq, Effect = pv_col, Slice = lbl,
                    SliceVal = pv, stringsAsFactors = FALSE)
      })
      do.call(rbind, Filter(Negate(is.null), slices))
   }
   
   make_slice_plot <- function(slice_df, pred_var, pred_color, title_suffix = "") {
      slice_df     <- slice_df %>% mutate(Slice = factor(Slice, levels = unique(Slice)))
      n_curves     <- nlevels(slice_df$Slice)
      curve_alphas <- setNames(alphas[1:n_curves], levels(slice_df$Slice))
      ggplot(slice_df, aes(x = h, y = Effect,
                           color = Slice, alpha = Slice, group = Slice)) +
         geom_hline(yintercept = 0, linetype = "dashed",
                    color = "#002030", linewidth = 0.5) +
         geom_line(linewidth = 1.1) +
         scale_color_manual(values = setNames(rep(pred_color, n_curves),
                                              levels(slice_df$Slice)),
                            name = pred_var) +
         scale_alpha_manual(values = curve_alphas, name = pred_var) +
         scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
         labs(title = paste0("ti(h, ", pred_var, ") Slices", title_suffix),
              x = "Lead Time (days)",
              y = "Partial Interaction Effect (ppt)") +
         theme_eval() +
         theme(legend.position = "bottom")
   }
   
   for (ti_label in ti_labels) {
      inner    <- gsub("ti\\(|\\)", "", ti_label)
      vars_in  <- trimws(strsplit(inner, ",")[[1]])
      pred_var <- vars_in[vars_in != "h"]
      if (length(pred_var) != 1 || !pred_var %in% names(stacked_train)) next
      
      pred_color <- resolve_predictor_color(pred_var, predictor_colors)
      safe_name  <- gsub("[^A-Za-z0-9_]", "", pred_var)
      
      if (pred_var == wind_var_name) {
         for (wlevel in levels(stacked_train$WindDir)) {
            sign_data  <- stacked_train[[pred_var]][stacked_train$WindDir == wlevel]
            slice_vals <- setNames(
               quantile(sign_data, probs = slice_prob, na.rm = TRUE),
               paste0(wlevel, "_", c("Low", "Mid", "High"))
            )
            sdf <- build_slice_df(pred_var, as.list(slice_vals), wind_level = wlevel)
            if (is.null(sdf) || nrow(sdf) == 0) next
            sdf$Slice <- sub(paste0(wlevel, "_"), "", sdf$Slice)
            p <- make_slice_plot(sdf, pred_var, pred_color,
                                 title_suffix = paste0(" \u2014 ", wlevel))
            save_plot_dir(p, slice_dir,
                          paste0("Slices_", safe_name, "_", wlevel), w = 8, h = 5)
            message("  Slice plot: ", pred_var, " / ", wlevel)
         }
      } else {
         slice_vals <- setNames(
            as.list(quantile(stacked_train[[pred_var]], probs = slice_prob, na.rm = TRUE)),
            c("Low", "Mid", "High")
         )
         sdf <- build_slice_df(pred_var, slice_vals)
         if (is.null(sdf) || nrow(sdf) == 0) next
         p <- make_slice_plot(sdf, pred_var, pred_color)
         save_plot_dir(p, slice_dir, paste0("Slices_", safe_name), w = 8, h = 5)
         message("  Slice plot: ", pred_var)
      }
   }
}


# =============================================================================
# TENSOR DERIVATIVE PLOTS  [d(ti_partial)/dh by central finite difference]
# =============================================================================

plot_tensor_derivatives <- function(gam_obj, ti_labels, stacked_train, model_vars,
                                    wind_var_name, reference_wind_level,
                                    reference_wind_value, predictor_colors,
                                    H_MAX, n_slices = 3, deriv_dir) {
   
   h_seq_ext  <- 1:(H_MAX + 1)
   slice_prob <- seq(0.02, 0.98, length.out = n_slices)
   alphas     <- seq(0.45, 1.0, length.out = n_slices)
   
   compute_deriv_slice <- function(pred_var, pred_val, wind_level = NULL) {
      grid <- make_pred_grid(pred_var, h_seq_ext, pred_val, stacked_train,
                             model_vars, gam_obj, wind_var_name,
                             reference_wind_level, reference_wind_value)
      if (!is.null(wind_level)) {
         grid$WindDir          <- factor(wind_level, levels = levels(stacked_train$WindDir))
         grid[[wind_var_name]] <- pred_val
      }
      tm <- tryCatch(predict(gam_obj, newdata = grid, type = "terms"), error = function(e) NULL)
      if (is.null(tm)) return(NULL)
      ti_lbl <- paste0("ti(h,", pred_var, ")")
      pv     <- extract_ti_column(tm, ti_lbl)
      if (is.null(pv)) return(NULL)
      n   <- length(pv)
      drv <- numeric(n)
      drv[1]       <- pv[2] - pv[1]
      drv[n]       <- pv[n] - pv[n - 1]
      drv[2:(n-1)] <- (pv[3:n] - pv[1:(n-2)]) / 2
      data.frame(h = h_seq_ext, Deriv = drv)
   }
   
   make_deriv_plot <- function(deriv_df, pred_var, pred_color, title_suffix = "") {
      deriv_df     <- deriv_df %>% mutate(Slice = factor(Slice, levels = unique(Slice)))
      n_curves     <- nlevels(deriv_df$Slice)
      curve_alphas <- setNames(alphas[1:n_curves], levels(deriv_df$Slice))
      ggplot(deriv_df, aes(x = h, y = Deriv,
                           color = Slice, alpha = Slice, group = Slice)) +
         geom_hline(yintercept = 0, linetype = "dashed",
                    color = "#002030", linewidth = 0.5) +
         geom_line(linewidth = 1.1) +
         scale_color_manual(values = setNames(rep(pred_color, n_curves),
                                              levels(deriv_df$Slice)),
                            name = pred_var) +
         scale_alpha_manual(values = curve_alphas, name = pred_var) +
         scale_x_continuous(breaks = seq(2, H_MAX, 2), limits = c(1, H_MAX)) +
         labs(title = paste0("\u2202/\u2202h ti(h, ", pred_var, ")", title_suffix),
              x = "Lead Time (days)",
              y = "d(Partial Effect)/dh (ppt / day)") +
         theme_eval() +
         theme(legend.position = "bottom")
   }
   
   for (ti_label in ti_labels) {
      inner    <- gsub("ti\\(|\\)", "", ti_label)
      vars_in  <- trimws(strsplit(inner, ",")[[1]])
      pred_var <- vars_in[vars_in != "h"]
      if (length(pred_var) != 1 || !pred_var %in% names(stacked_train)) next
      
      pred_color <- resolve_predictor_color(pred_var, predictor_colors)
      safe_name  <- gsub("[^A-Za-z0-9_]", "", pred_var)
      slice_lbls <- c("Low", "Mid", "High")
      
      if (pred_var == wind_var_name) {
         for (wlevel in levels(stacked_train$WindDir)) {
            sign_data  <- stacked_train[[pred_var]][stacked_train$WindDir == wlevel]
            slice_vals <- quantile(sign_data, probs = slice_prob, na.rm = TRUE)
            deriv_rows <- lapply(seq_along(slice_vals), function(i) {
               df <- compute_deriv_slice(pred_var, slice_vals[i], wind_level = wlevel)
               if (is.null(df)) return(NULL)
               df$Slice <- slice_lbls[i]; df$SliceVal <- slice_vals[i]; df
            })
            ddf <- do.call(rbind, Filter(Negate(is.null), deriv_rows))
            if (is.null(ddf) || nrow(ddf) == 0) next
            p <- make_deriv_plot(ddf, pred_var, pred_color,
                                 title_suffix = paste0(" \u2014 ", wlevel))
            save_plot_dir(p, deriv_dir,
                          paste0("Deriv_", safe_name, "_", wlevel), w = 8, h = 5)
            message("  Derivative plot: ", pred_var, " / ", wlevel)
         }
      } else {
         slice_vals <- quantile(stacked_train[[pred_var]], probs = slice_prob, na.rm = TRUE)
         deriv_rows <- lapply(seq_along(slice_vals), function(i) {
            df <- compute_deriv_slice(pred_var, slice_vals[i])
            if (is.null(df)) return(NULL)
            df$Slice <- slice_lbls[i]; df$SliceVal <- slice_vals[i]; df
         })
         ddf <- do.call(rbind, Filter(Negate(is.null), deriv_rows))
         if (is.null(ddf) || nrow(ddf) == 0) next
         p <- make_deriv_plot(ddf, pred_var, pred_color)
         save_plot_dir(p, deriv_dir, paste0("Deriv_", safe_name), w = 8, h = 5)
         message("  Derivative plot: ", pred_var)
      }
   }
}


# =============================================================================
# FORECAST PANEL PLOTS
# =============================================================================

plot_salinity_forecast_panels <- function(data,
                                          date_range = NULL,
                                          year       = NULL,
                                          horizons   = NULL,
                                          epa_line   = TRUE,
                                          threshold  = 0.5,
                                          title      = NULL) {
   
   observed_linewidth <- 0.9
   model_linewidth    <- 1.3
   observed_alpha     <- 0.8
   model_alpha        <- 1.0
   observed_color     <- "#f58220"
   model_palette      <- c("#3b7ea1", "#6a994e", "#8338ec", "#bc4b51",
                           "#fb5607", "#ffbe0b", "#06ffa5", "#c4820e")
   
   if (is.null(horizons)) stop("horizons must be specified.")
   
   data <- data %>%
      dplyr::mutate(TargetDate = DateTime + lubridate::days(h)) %>%
      dplyr::filter(h %in% horizons)
   
   if (!is.null(year)) {
      base_data <- data %>% dplyr::filter(lubridate::year(TargetDate) == year)
   } else if (!is.null(date_range)) {
      base_data <- data %>%
         dplyr::filter(TargetDate >= as_datetime(date_range[1]) &
                          TargetDate <= as_datetime(date_range[2]))
   } else {
      base_data <- data
   }
   
   add_segments <- function(df) {
      df %>%
         dplyr::arrange(TargetDate) %>%
         dplyr::mutate(
            dt      = as.numeric(difftime(TargetDate, dplyr::lag(TargetDate), units = "secs")),
            base_dt = median(dt, na.rm = TRUE),
            segment = cumsum(is.na(dt) | dt > 1.5 * base_dt)
         )
   }
   
   make_panel <- function(h_val, model_color,
                          show_x_axis = FALSE, show_y_axis = TRUE,
                          show_y_label = FALSE, panel_title = NULL) {
      
      h_data       <- base_data %>% dplyr::filter(h == h_val)
      series_label <- paste0("h = ", h_val, " days")
      
      obs_df <- h_data %>%
         dplyr::select(TargetDate, Value = Salinity_h) %>%
         dplyr::mutate(Series = "Observed") %>%
         add_segments()
      
      mod_df <- h_data %>%
         dplyr::select(TargetDate, Value = Predicted) %>%
         dplyr::mutate(Series = series_label) %>%
         add_segments()
      
      plot_long <- dplyr::bind_rows(obs_df, mod_df) %>%
         dplyr::mutate(Series = factor(Series, levels = c("Observed", series_label)),
                       .draw_order = ifelse(Series == "Observed", 2, 1)) %>%
         dplyr::arrange(.draw_order)
      
      label_row <- mod_df %>%
         dplyr::filter(!is.na(Value)) %>%
         dplyr::slice_max(TargetDate, n = 1)
      
      p <- ggplot(plot_long,
                  aes(x = TargetDate, y = Value,
                      color = Series, size = Series, alpha = Series,
                      group = interaction(Series, segment))) +
         geom_line()
      
      if (epa_line) {
         p <- p +
            geom_hline(yintercept = threshold, color = "#002030", linetype = 2) +
            annotate("text",
                     x = min(base_data$TargetDate), y = threshold + 0.02,
                     label = "EPA Secondary Drinking Water Standard for TDS",
                     hjust = 0, vjust = 0, size = 4, colour = "#002030")
      }
      
      p +
         annotate("text",
                  x = label_row$TargetDate, y = label_row$Value,
                  label = series_label, hjust = 1.05, vjust = -0.5,
                  size = 5, fontface = "bold", colour = model_color) +
         scale_color_manual(values = c("Observed" = observed_color,
                                       setNames(model_color, series_label))) +
         scale_size_manual( values = c("Observed" = observed_linewidth,
                                       setNames(model_linewidth, series_label))) +
         scale_alpha_manual(values = c("Observed" = observed_alpha,
                                       setNames(model_alpha, series_label))) +
         scale_y_continuous(name = if (show_y_label) "Salinity (ppt)" else NULL) +
         labs(x = if (show_x_axis) "Date" else NULL, title = panel_title) +
         theme_bw() +
         theme(
            plot.title         = element_text(size = 18, face = "bold", color = "#002030"),
            axis.title.x       = element_text(size = 16, face = "bold", color = "#002030"),
            axis.title.y.left  = element_text(size = 16, face = "bold", colour = "#f58220"),
            axis.text.y.left   = element_text(colour = "#f58220", size = 13),
            axis.text.x        = if (show_x_axis) element_text(size = 13) else element_blank(),
            axis.ticks.x       = if (show_x_axis) element_line() else element_blank(),
            panel.border       = element_blank(),
            axis.line.x.bottom = element_line(colour = "#002030", linewidth = 0.6),
            axis.line.y.left   = element_line(colour = "#002030", linewidth = 0.6),
            legend.position    = "none"
         ) +
         { if (!show_y_axis) theme(axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank()) }
   }
   
   n_panels     <- length(horizons)
   panel_colors <- model_palette[(seq_along(horizons) - 1) %% length(model_palette) + 1]
   
   panels <- purrr::pmap(
      list(
         h_val        = horizons,
         model_color  = panel_colors,
         show_x_axis  = (seq_along(horizons) == n_panels),
         show_y_axis  = rep(TRUE, n_panels),
         show_y_label = (seq_along(horizons) == ceiling(n_panels / 2)),
         panel_title  = c(list(title), rep(list(NULL), max(n_panels - 1, 0)))
      ),
      make_panel
   )
   
   patchwork::wrap_plots(panels, ncol = 1) &
      theme(plot.margin = margin(2, 10, 2, 10))
}