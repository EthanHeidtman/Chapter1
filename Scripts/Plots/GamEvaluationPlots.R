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
#   get_cluster_robust_vcov(model, data, cluster_var_name)
#   plot_performance_metrics(perf_hold, H_MAX, gam_colors, base_dir)
#   plot_residual_diagnostics(stacked_hold, H_MAX, HIGH_SALINITY_THRESHOLD,
#                              gam_colors, base_dir)
#   plot_calibration(stacked_hold, N_CAL_BINS, gam_colors, base_dir)
#   plot_acf_pacf(stacked_hold, H_MAX, gam_colors, acf_dir)
#   plot_1d_smooths(gam_obj, s_labels, stacked_train, model_vars,
#                   wind_var_name, reference_wind_level, reference_wind_value,
#                   predictor_colors, smooth_dir)
#   plot_tensor_full(gam_obj, ti_labels, stacked_train, model_vars,
#                    wind_var_name, reference_wind_level, reference_wind_value,
#                    predictor_colors, tensor_negative_color, H_MAX,
#                    tensor_full_dir)
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
# Matches on the longest key in predictor_colors that is a prefix of var_name.
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
# COVARIANCE ADJUSTMENT FUNCTION
# =============================================================================

get_cluster_robust_vcov <- function(model, data, cluster_var_name) {
   cat("\nAligning data and checking for omitted NA rows...\n")
   
   vars_in_model <- all.vars(formula(model))
   if ("Response" %in% vars_in_model && !("Response" %in% names(data))) {
      if ("Salinity_h" %in% names(data)) {
         vars_in_model <- c(setdiff(vars_in_model, "Response"), "Salinity_h")
      }
   }
   vars_to_check <- intersect(vars_in_model, names(data))
   
   complete_idx <- complete.cases(data[, vars_to_check, drop = FALSE])
   clean_data <- data[complete_idx, ]
   
   # Bread: penalized Bayesian covariance matrix
   bread <- vcov(model, unconditional = FALSE)
   p_raw <- ncol(bread)          # raw basis dimension (all coefficients)
   p_edf <- sum(model$edf)       # effective degrees of freedom
   
   cat(sprintf("Raw basis dimension (ncol(bread)): %d\n", p_raw))
   cat(sprintf("Effective df (sum(model$edf)):      %.2f\n", p_edf))
   cat("Using p = sum(model$edf) for sig2 fallback and df correction.\n")
   
   # Force exact (non-discretized) prediction path.
   # model$dinfo is non-NULL here (fit with bam(discrete=TRUE)); the default
   # predict.bam path bins covariates and evaluates the basis once per bin,
   # which was verified to diverge materially from the exact evaluation for
   # this model. discrete = FALSE forces per-observation exact evaluation.
   cat("Extracting exact (non-discretized) design matrix...\n")
   X <- predict(model, newdata = clean_data, type = "lpmatrix", discrete = FALSE)
   
   y_hat <- as.numeric(predict(model, newdata = clean_data, type = "response",
                               discrete = FALSE))
   response_var_name <- intersect(c("Response", "Salinity_h"), names(clean_data))[1]
   y <- clean_data[[response_var_name]]
   res <- y - y_hat
   
   # Scale parameter (sigma^2) -- use p_edf consistently
   sig2 <- model$sig2
   if (is.null(sig2)) sig2 <- sum(res^2) / (nrow(X) - p_edf)
   
   cluster_var <- as.factor(clean_data[[cluster_var_name]])
   cluster_levels <- levels(cluster_var)
   G <- length(cluster_levels)
   N <- nrow(X)
   
   cat(sprintf("Processing %d clusters over %d observations...\n", G, N))
   
   meat <- matrix(0, nrow = p_raw, ncol = p_raw)
   
   for (g in cluster_levels) {
      idx <- which(cluster_var == g)
      X_g   <- X[idx, , drop = FALSE]
      res_g <- res[idx]
      score_g <- t(X_g) %*% res_g
      meat <- meat + (score_g %*% t(score_g))
   }
   
   meat <- meat / (sig2^2)
   
   # Small-sample cluster correction using effective df, not raw basis size
   df_correction <- (G / (G - 1)) * ((N - 1) / (N - p_edf))
   meat <- meat * df_correction
   
   robust_vcov <- bread %*% meat %*% bread
   colnames(robust_vcov) <- rownames(robust_vcov) <- colnames(bread)
   
   cat("Cluster-robust covariance adjustment complete.\n\n")
   return(robust_vcov)
}

# =============================================================================
# PERFORMANCE METRICS — distilled to a single two-panel figure:
# A) RMSE/MAE by lead time, B) R2 by lead time.
# =============================================================================

plot_performance_metrics <- function(perf_hold, H_MAX, gam_colors, base_dir) {
   
   p_error <- perf_hold %>%
      select(LeadTime, RMSE, MAE, RMSE_High, MAE_High) %>%
      pivot_longer(-LeadTime, names_to = "Stat", values_to = "Value") %>%
      mutate(
         Metric = ifelse(grepl("^RMSE", Stat), "RMSE", "MAE"),
         Subset = ifelse(grepl("_High$", Stat), "High Salinity", "Overall")
      ) %>%
      ggplot(aes(x = LeadTime, y = Value, color = Metric,
                 linetype = Subset, shape = Subset, group = Stat)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = c("RMSE" = gam_colors$secondary,
                                    "MAE"  = gam_colors$primary),
                         name = "Metric") +
      scale_linetype_manual(values = c("Overall" = "solid", "High Salinity" = "dashed"),
                            name = "Subset") +
      scale_shape_manual(values = c("Overall" = 16, "High Salinity" = 16),
                         name = "Subset") +
      scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
      labs(title = "A)",
           x     = "Forecast Horizon (days)",
           y     = "Error (ppt)") +
      theme_eval() +
      theme(legend.position = "bottom", legend.box = "horizontal",
            legend.key.width = unit(1.5, "cm"),
            plot.title = element_text(face = "bold", size = 16, hjust = 0,
                                      margin = margin(b = 2)))
   
   # R2 uses tertiary color
   p_r2 <- perf_hold %>%
      ggplot(aes(x = LeadTime, y = R2)) +
      geom_line(linewidth = 1.2, color = gam_colors$tertiary) +
      geom_point(size = 3, color = gam_colors$tertiary) +
      scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
      labs(title = "B)",
           x     = "Forecast Horizon (days)",
           y     = expression(R^2)) +
      theme_eval() +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0,
                                      margin = margin(b = 2)))
   
   p_combined <- p_error | p_r2
   
   save_plot(p_error, "Error_ByLeadTime", w = 10, h = 6)
   save_plot(p_r2,    "R2_ByLeadTime",    w = 10, h = 6)
   ggsave(file.path(base_dir, "Performance_Combined.png"), plot = p_combined, width = 14, height = 6, dpi = 600)
   ggsave(file.path(base_dir, "Performance_Combined.svg"), plot = p_combined, width = 14, height = 6)
   
   invisible(list(error = p_error, r2 = p_r2, combined = p_combined))
}

# =============================================================================
# RESIDUAL DIAGNOSTICS — observed-vs-predicted scatter plots dropped (both
# the overall and by-horizon-bin versions). QQ, residual histogram, and
# residuals-vs-fitted retained.
# =============================================================================

plot_residual_diagnostics <- function(stacked_hold, H_MAX,
                                      HIGH_SALINITY_THRESHOLD,
                                      gam_colors, base_dir) {
   
   h_breaks <- c(0, 5, 10, 15, 20)
   h_labels <- c("h = 1\u20135", "h = 6\u201310", "h = 11\u201315", "h = 16\u201320")
   
   resid_df <- stacked_hold %>%
      filter(!is.na(Residual)) %>%
      mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels)) %>%
      filter(!is.na(HBin)) # Drop any dangling horizons outside bounds
   
   p_qq <- ggplot(resid_df, aes(sample = Residual)) +
      stat_qq(size = 0.6, alpha = 0.4, color = gam_colors$secondary) +
      stat_qq_line(color = gam_colors$dark, linetype = "dashed") +
      facet_wrap(~ HBin) +
      labs(title = "Q-Q Plot of Residuals by Forecast Horizon Bin",
           x     = "Theoretical Quantiles",
           y     = "Sample Quantiles") +
      theme_eval()
   
   p_resid_hist <- ggplot(resid_df, aes(x = Residual)) +
      geom_histogram(bins = 50, fill = gam_colors$secondary, alpha = 0.85,
                     color = gam_colors$dark, linewidth = 0.2) +
      facet_wrap(~ HBin, scales = "free_y") +
      labs(title = "Residual Distribution by Forecast Horizon Bin",
           x     = "Residual (ppt)",
           y     = "Count") +
      theme_eval()
   
   p_resid_fitted <- ggplot(resid_df, aes(x = Predicted, y = Residual)) +
      geom_point(alpha = 0.25, size = 0.7, color = gam_colors$secondary) +
      geom_hline(yintercept = 0, linetype = "dashed", color = gam_colors$dark) +
      geom_smooth(method = "loess", se = TRUE, color = gam_colors$primary,
                  fill = gam_colors$primary, alpha = 0.15,
                  linewidth = 0.9, span = 0.4) +
      facet_wrap(~ HBin) +
      labs(title = "Residuals vs Fitted by Forecast Horizon Bin",
           x     = "Fitted (ppt)",
           y     = "Residual (ppt)") +
      theme_eval()
   
   save_plot(p_qq,           "QQ_ByHBin",             w = 10, h = 8)
   save_plot(p_resid_hist,   "ResidHist_ByHBin",      w = 10, h = 8)
   save_plot(p_resid_fitted, "ResidVsFitted_ByHBin",  w = 10, h = 8)
   
   invisible(list(qq = p_qq, hist = p_resid_hist, resid_fitted = p_resid_fitted))
}

# =============================================================================
# CALIBRATION — vertical error bars on the observed dimension removed (there
# was no principled reason for them here: N-based SE on the observed mean
# within a predicted-quantile bin doesn't represent calibration uncertainty
# the way the horizontal predicted-dimension bars do). Horizontal bars on
# the predicted dimension retained.
# =============================================================================

plot_calibration <- function(stacked_hold, N_CAL_BINS, gam_colors, base_dir) {
   library(dplyr)
   library(ggplot2)
   
   h_breaks <- c(0, 5, 10, 15, 20)
   h_labels <- c("h = 1\u20135", "h = 6\u201310", "h = 11\u201315", "h = 16\u201320")
   
   cal_df <- stacked_hold %>%
      filter(!is.na(Salinity_h), !is.na(Predicted)) %>%
      mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels)) %>%
      filter(!is.na(HBin)) %>%
      group_by(HBin) %>%
      mutate(PredBin = cut(Predicted,
                           breaks = quantile(Predicted, probs = seq(0, 1, 1 / N_CAL_BINS), na.rm = TRUE),
                           include.lowest = TRUE, labels = FALSE)) %>%
      group_by(HBin, PredBin) %>%
      summarise(MeanPredicted = mean(Predicted,  na.rm = TRUE),
                MeanObserved  = mean(Salinity_h, na.rm = TRUE),
                SE_Predicted  = sd(Predicted, na.rm = TRUE) / sqrt(n()),
                N             = n(),
                .groups       = "drop") %>%
      mutate(
         Pred_Lower = MeanPredicted - 1.96 * SE_Predicted,
         Pred_Upper = MeanPredicted + 1.96 * SE_Predicted
      )
   
   cal_range <- range(c(cal_df$MeanPredicted, cal_df$MeanObserved,
                        cal_df$Pred_Lower, cal_df$Pred_Upper), na.rm = TRUE)
   
   p_cal <- ggplot(cal_df, aes(x = MeanPredicted, y = MeanObserved)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = gam_colors$dark) +
      geom_errorbarh(aes(xmin = Pred_Lower, xmax = Pred_Upper),
                     color = gam_colors$secondary, height = 0, alpha = 0.5) +
      geom_point(aes(size = N), color = gam_colors$secondary, alpha = 0.85) +
      facet_wrap(~ HBin) +
      scale_size_continuous(range = c(2, 6), name = "N") +
      coord_fixed(xlim = cal_range, ylim = cal_range, clip = "off") +
      labs(title = "Calibration by Forecast Horizon Bin \u2014 Holdout",
           x     = "Mean Predicted (ppt)",
           y     = "Mean Observed (ppt)") +
      theme_bw() +
      theme(
         strip.background = element_rect(fill = "grey95", color = "black"),
         strip.text       = element_text(face = "bold", size = 12, color = "#002030"),
         plot.title       = element_text(face = "bold", size = 15, color = "#002030"),
         axis.title       = element_text(face = "bold", size = 13, color = "#002030"),
         axis.text        = element_text(color = "black")
      )
   
   ggsave(file.path(base_dir, "Model_Calibration.png"), plot = p_cal, width = 10, height = 8, dpi = 600)
   ggsave(file.path(base_dir, "Model_Calibration.svg"), plot = p_cal, width = 10, height = 8)
   
   invisible(p_cal)
}

# =============================================================================
# ACF / PACF BY H-SLICE
# =============================================================================
plot_acf_pacf <- function(stacked_hold, H_MAX, gam_colors, acf_dir) {
   
   acf_records  <- list()
   pacf_records <- list()
   
   for (hval in 1:H_MAX) {
      resid_h <- stacked_hold %>%
         filter(h == hval, !is.na(Residual)) %>%
         arrange(DateTime) %>%
         pull(Residual)
      if (length(resid_h) < 20) next
      
      # Dynamic, clean slice length for robust CI bounds
      n_obs <- length(resid_h)
      ci    <- qnorm(0.975) / sqrt(n_obs)
      
      acf_obj  <- acf(resid_h,  plot = FALSE, lag.max = 30)
      pacf_obj <- pacf(resid_h, plot = FALSE, lag.max = 30)
      
      acf_records[[hval]]  <- data.frame(h    = hval,
                                         Lag  = as.numeric(acf_obj$lag[-1]),
                                         ACF  = as.numeric(acf_obj$acf[-1]),
                                         CI   = ci)
      pacf_records[[hval]] <- data.frame(h    = hval,
                                         Lag  = as.numeric(pacf_obj$lag),
                                         PACF = as.numeric(pacf_obj$acf),
                                         CI   = ci)
   }
   
   acf_df  <- do.call(rbind, acf_records)
   pacf_df <- do.call(rbind, pacf_records)
   
   # Explicit numeric-ordered factor for facet labels, built once from the
   # actual h values present (so it still works if some horizons were
   # dropped above for having <20 residuals).
   h_levels_numeric <- sort(unique(c(acf_df$h, pacf_df$h)))
   h_level_labels   <- paste0("h = ", h_levels_numeric)
   
   acf_df$HLabel  <- factor(paste0("h = ", acf_df$h),  levels = h_level_labels)
   pacf_df$HLabel <- factor(paste0("h = ", pacf_df$h), levels = h_level_labels)
   
   p_acf <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
      geom_col(fill = gam_colors$secondary, alpha = 0.85, width = 0.7) +
      geom_hline(aes(yintercept = CI), linetype = "dashed", color = gam_colors$dark, linewidth = 0.5) +
      geom_hline(aes(yintercept = -CI), linetype = "dashed", color = gam_colors$dark, linewidth = 0.5) +
      geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.3) +
      facet_wrap(~ HLabel, ncol = 4) +
      labs(title = "Residual Autocorrelation Function (ACF) by Forecast Horizon",
           x = "Lag (days)", y = "ACF") +
      theme_eval() +
      theme(axis.text = element_text(size = 7), strip.text = element_text(size = 8, face = "bold"))
   
   p_pacf <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
      geom_col(fill = gam_colors$primary, alpha = 0.85, width = 0.7) +
      geom_hline(aes(yintercept = CI), linetype = "dashed", color = gam_colors$dark, linewidth = 0.5) +
      geom_hline(aes(yintercept = -CI), linetype = "dashed", color = gam_colors$dark, linewidth = 0.5) +
      geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.3) +
      facet_wrap(~ HLabel, ncol = 4) +
      labs(title = "Residual Partial Autocorrelation Function (PACF) by Forecast Horizon",
           x = "Lag (days)", y = "PACF") +
      theme_eval() +
      theme(axis.text = element_text(size = 7), strip.text = element_text(size = 8, face = "bold"))
   
   ggsave(file.path(acf_dir, "ACF_AllH.png"),   plot = p_acf,  width = 14, height = 14, dpi = 600)
   ggsave(file.path(acf_dir, "PACF_AllH.png"),  plot = p_pacf, width = 14, height = 14, dpi = 600)
   
   invisible(list(acf = p_acf, pacf = p_pacf))
}

# =============================================================================
# 1D MARGINAL SMOOTH PLOTS — Returns the plot objects (not just saves
# them) so build_paired_grid() can assemble them downstream.
#
# Requires: get_units() to be sourced first.
# =============================================================================

plot_1d_smooths <- function(gam_obj, s_labels, stacked_train, model_vars,
                            wind_var_name, reference_wind_level, reference_wind_value,
                            predictor_colors, smooth_dir, h_fallback_color = "#002030") {
   
   wind_s_labels  <- s_labels[grepl(paste0("^s\\(", wind_var_name, "\\)"), s_labels)]
   other_s_labels <- setdiff(s_labels, wind_s_labels)
   wind_levels    <- levels(stacked_train$WindDir)
   wind_color     <- resolve_predictor_color(wind_var_name, predictor_colors)
   
   plots <- list()  # named by variable, for downstream pairing
   
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
         level_range <- range(stacked_train[[wind_var_name]][stacked_train$WindDir == by_level], na.rm = TRUE)
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
         data.frame(x = newdata[[wind_var_name]], fit = preds$fit[, col_match],
                    se  = preds$se.fit[, col_match], Level = by_level)
      })
      
      wind_df <- do.call(rbind, Filter(Negate(is.null), wind_curves))
      if (!is.null(wind_df) && nrow(wind_df) > 0) {
         wind_df <- wind_df %>% mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
         
         label_df <- wind_df %>% group_by(Level) %>% slice_min(abs(x - mean(range(x))), n = 1, with_ties = FALSE) %>%
            ungroup() %>% mutate(LabelText = case_when(Level == "LeftBank" ~ "Easterly", Level == "RightBank" ~ "Westerly", TRUE ~ as.character(Level)))
         
         wind_units_str <- get_units(wind_var_name)
         wind_y_label    <- if (wind_units_str != "") paste(wind_var_name, wind_units_str) else wind_var_name
         
         p_wind <- ggplot(wind_df, aes(x = fit, y = x, group = Level)) +
            geom_vline(xintercept = 0, linetype = "dashed", color = "#002030", linewidth = 0.5) +
            geom_hline(yintercept = 0, linetype = "dotted", color = "#002030", linewidth = 0.4) +
            geom_ribbon(aes(xmin = lower, xmax = upper), fill = wind_color, alpha = 0.18, color = NA) +
            geom_path(color = wind_color, linewidth = 1.1) +
            geom_text(data = label_df, aes(label = LabelText), vjust = 0.3, hjust = -0.2, color = wind_color, size = 3.5, fontface = "italic") +
            geom_rug(data = data.frame(x = stacked_train[[wind_var_name]]), aes(y = x), inherit.aes = FALSE, sides = "l", alpha = 0.12, color = "#002030") +
            labs(title = paste0("Partial Dependence Plot: s(", wind_var_name, ") by Wind Direction"),
                 y     = wind_y_label, x     = "Partial Effect (ppt)") +
            theme_eval() + theme(legend.position = "none")
         
         save_plot_dir(p_wind, smooth_dir, paste0("Smooth_", wind_var_name, "_Combined"), w = 8, h = 5)
         plots[[wind_var_name]] <- p_wind
      }
   }
   
   # ---- (B) All other 1D smooths ----
   for (s_label in other_s_labels) {
      inner    <- gsub("^s\\(|\\).*$", "", s_label)
      var_name <- trimws(strsplit(inner, ",")[[1]])[1]
      if (!var_name %in% names(stacked_train)) next
      
      line_color <- if (var_name == "h") h_fallback_color else resolve_predictor_color(var_name, predictor_colors, fallback = "#888888")
      x_range <- range(stacked_train[[var_name]], na.rm = TRUE)
      newdata <- setNames(data.frame(seq(x_range[1], x_range[2], length.out = 200)), var_name)
      
      for (v in model_vars) {
         if (v %in% names(newdata)) next
         if (v == "h")           { newdata$h <- median(stacked_train$h, na.rm = TRUE); next }
         if (v == "WindDir")     { newdata$WindDir <- factor(reference_wind_level, levels = levels(stacked_train$WindDir)); next }
         if (v == wind_var_name) { newdata[[v]] <- reference_wind_value; next }
         if (v %in% names(stacked_train)) newdata[[v]] <- median(stacked_train[[v]], na.rm = TRUE)
      }
      
      preds <- predict_terms_safe(newdata, s_label)
      if (is.null(preds)) next
      col_match <- which(colnames(preds$fit) == s_label)
      if (length(col_match) == 0) next
      
      pred_df <- data.frame(x = newdata[[var_name]], fit = preds$fit[, col_match], se = preds$se.fit[, col_match]) %>%
         mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)
      
      units_str <- get_units(var_name)
      y_label   <- if (units_str != "") paste(var_name, units_str) else var_name
      
      p_smooth <- ggplot(pred_df, aes(x = fit, y = x)) +
         geom_vline(xintercept = 0, linetype = "dashed", color = "#002030", linewidth = 0.5) +
         geom_ribbon(aes(xmin = lower, xmax = upper), fill = line_color, alpha = 0.2) +
         geom_path(color = line_color, linewidth = 1.1) +
         geom_rug(data = data.frame(x = stacked_train[[var_name]]), aes(y = x), inherit.aes = FALSE, sides = "l", alpha = 0.2, color = "#002030") +
         labs(title = paste0("Partial Dependence Plot: s(", var_name, ")"),
              y     = y_label, x     = "Partial Effect (ppt)") +
         theme_eval()
      
      safe_name <- gsub("[^A-Za-z0-9_]", "", s_label)
      save_plot_dir(p_smooth, smooth_dir, paste0("Smooth_", safe_name), w = 8, h = 5)
      plots[[var_name]] <- p_smooth
   }
   
   invisible(plots)
}

# =============================================================================
# LagSalinity has no marginal s() term — it enters the model only through
# ti(h, LagSalinity). This produces a rug/density panel so its row in the
# paired grid stays structurally consistent with the others, rather than
# leaving a blank cell that could be mistaken for a missing plot.
#
# Requires: get_units() to be sourced first.
# =============================================================================

plot_lag_salinity_rug <- function(stacked_train, predictor_colors,
                                  var_name = "LagSalinity") {
   color_val <- resolve_predictor_color(var_name, predictor_colors, fallback = "#888888")
   units_str <- get_units(var_name)
   y_label   <- if (units_str != "") paste(var_name, units_str) else var_name
   
   df <- data.frame(x = stacked_train[[var_name]])
   
   ggplot(df, aes(y = x)) +
      geom_rug(aes(x = 0), sides = "l", color = "#002030", alpha = 0.25) +
      geom_density(aes(x = after_stat(density)), fill = color_val, alpha = 0.25,
                   color = color_val, orientation = "y") +
      labs(title = paste0(var_name, ": No Marginal Smooth"),
           subtitle = "Enters the model only through ti(h, LagSalinity) \u2014\nno additive s() term exists to plot",
           y = y_label, x = "Density") +
      theme_eval()
}

get_units <- function(var) {
   if (grepl("Salinity", var))  return("(ppt)")
   if (grepl("Discharge", var)) return("(m\u00b3/s)") # m^3/s
   if (grepl("Tide", var))      return("(m)")
   if (grepl("Wind", var))      return("(m/s)")
   return("")
}

plot_robust_tensor_surfaces <- function(gam_obj, ti_labels, stacked_train, model_vars,
                                        wind_var_name, reference_wind_level, reference_wind_value,
                                        predictor_colors, H_MAX, output_dir,
                                        sig_z = 1.96,        # 95% two-sided z critical value for the baseline-difference test
                                        nonsig_alpha = 0.30, # opacity for cells not significantly different from baseline
                                        grid_n_pred = 150) { # predictor-axis grid resolution
   library(mgcv)
   library(dplyr)
   library(ggplot2)
   library(lubridate)
   
   # 1. Define Seasonal Regimes
   dry_data <- stacked_train %>% filter(month(DateTime) %in% c(8, 9, 10))
   wet_data <- stacked_train %>% filter(month(DateTime) %in% c(3, 4, 5))
   
   regimes <- list(
      "DrySeason" = dry_data,
      "WetSeason" = wet_data
   )
   
   for (regime_name in names(regimes)) {
      dir.create(file.path(output_dir, regime_name), recursive = TRUE, showWarnings = FALSE)
   }
   
   # Return structure: plots_out[[regime_name]][[pred_var]] -> ggplot object.
   # If a term has a 'by' variable, plots_out[[regime_name]][[pred_var]] is
   # itself a named list keyed by level.
   plots_out <- setNames(vector("list", length(regimes)), names(regimes))
   
   # 2. Iterate through tensor terms
   for (ti_label in ti_labels) {
      inner    <- gsub("ti\\(|\\)", "", ti_label)
      vars_in  <- trimws(strsplit(inner, ",")[[1]])
      
      # Extract 'by' variable if present (e.g., "by = WindDir")
      by_var   <- NULL
      by_part  <- vars_in[grepl("^by\\s*=", vars_in)]
      if (length(by_part) > 0) {
         by_var  <- trimws(sub("^by\\s*=\\s*", "", by_part))
         vars_in <- vars_in[!grepl("^by\\s*=", vars_in)] # strip 'by' part out
      }
      
      pred_var <- vars_in[vars_in != "h"]
      if (length(pred_var) != 1 || !pred_var %in% names(stacked_train)) next
      
      pos_color  <- resolve_predictor_color(pred_var, predictor_colors)
      units_str  <- get_units(pred_var)
      y_label    <- if (units_str != "") paste(pred_var, units_str) else pred_var
      
      h_seq     <- 1:H_MAX
      # Full observed range, matching the smooth-plot rug distribution 
      pred_range <- range(stacked_train[[pred_var]], na.rm = TRUE)
      pred_seq   <- seq(pred_range[1], pred_range[2], length.out = grid_n_pred)
      
      for (regime_name in names(regimes)) {
         regime_data <- regimes[[regime_name]]
         
         # --- Precompute baseline values ONCE per regime ---
         regime_baselines <- list()
         for (v in model_vars) {
            if (v %in% names(regime_data)) {
               if (is.factor(regime_data[[v]])) {
                  tbl <- table(regime_data[[v]])
                  regime_baselines[[v]] <- factor(names(tbl)[which.max(tbl)], levels = levels(regime_data[[v]]))
               } else {
                  regime_baselines[[v]] <- mean(regime_data[[v]], na.rm = TRUE)
               }
            }
         }
         
         by_levels <- if (!is.null(by_var)) levels(stacked_train[[by_var]]) else list(NULL)
         
         for (by_lev in by_levels) {
            
            # --- Build the Surface Grid ---
            base_grid <- expand.grid(h = h_seq, pred_target = pred_seq)
            names(base_grid)[2] <- pred_var
            
            for (v in model_vars) {
               if (v == "h" || v == pred_var) next
               if (v %in% names(regime_baselines)) {
                  if (!is.null(by_var) && v == by_var) {
                     base_grid[[v]] <- factor(by_lev, levels = levels(regime_data[[v]]))
                  } else {
                     base_grid[[v]] <- regime_baselines[[v]]
                  }
               }
            }
            
            # Single-row seasonal baseline condition (mean of every
            # variable, held at the regime's typical value) used as the
            # reference point for the significance test below: is the
            # prediction at each grid cell meaningfully different from the
            # "typical day" for this season?
            baseline_row <- list()
            for (v in model_vars) {
               if (v %in% names(regime_baselines)) baseline_row[[v]] <- regime_baselines[[v]]
            }
            baseline_row[["h"]] <- mean(regime_data$h, na.rm = TRUE)
            if (!is.null(by_var)) baseline_row[[by_var]] <- factor(by_lev, levels = levels(regime_data[[by_var]]))
            baseline_df <- as.data.frame(baseline_row)
            
            # --- Predict Full Conditional Response ---
            # type = "response" pulls the whole linear predictor (all terms,
            # including intercept), so this is the actual predicted salinity
            # under the seasonal-mean baseline at each grid cell, not a
            # partial effect.
            resp_predictions <- tryCatch(
               predict(gam_obj, newdata = base_grid, type = "response"),
               error = function(e) NULL
            )
            if (is.null(resp_predictions)) next
            base_grid$PredictedSalinity <- pmax(0, as.numeric(resp_predictions))
            
            # --- Significance vs. the seasonal baseline (delta method) ---
            # Tests whether each grid cell's prediction is significantly
            # different from the "typical day" baseline for this season/
            # level, at the model's identity-link response scale. Uses the
            # lpmatrix rather than separately summing SEs, since the grid
            # prediction and the baseline prediction are correlated (they
            # share most fitted terms) — the delta-method SE of the
            # difference correctly accounts for that covariance via the
            # already-verified cluster-robust Vp.
            sig_result <- tryCatch({
               X_grid <- predict(gam_obj, newdata = base_grid, type = "lpmatrix")
               X_base <- predict(gam_obj, newdata = baseline_df, type = "lpmatrix")
               Xdiff  <- sweep(X_grid, 2, as.numeric(X_base[1, ]), "-")
               diff_fit <- as.numeric(Xdiff %*% coef(gam_obj))
               diff_se  <- sqrt(rowSums((Xdiff %*% gam_obj$Vp) * Xdiff))
               list(z = diff_fit / diff_se)
            }, error = function(e) { message("    Significance test failed: ", e$message); NULL })
            
            if (!is.null(sig_result)) {
               base_grid$Significant95 <- abs(sig_result$z) >= sig_z
            } else {
               base_grid$Significant95 <- TRUE  # fail open: show full opacity rather than silently blank
            }
            
            # Per-panel max color ceiling — a shared fixed ceiling (e.g. 2.0
            # ppt, the observed event peak) washed out every predictor except
            # LagSalinity, since most conditions never approach that value.
            panel_max <- max(base_grid$PredictedSalinity, na.rm = TRUE)
            if (panel_max <= 0) panel_max <- 1  # guard against a degenerate all-zero panel
            
            # --- Plotting: fill = predicted salinity, alpha = significance vs. baseline ---
            season_title  <- ifelse(regime_name == "DrySeason", "Dry Season (Aug-Oct)", "Wet Season (Mar-May)")
            title_suffix  <- if (!is.null(by_var)) paste0(" (", by_lev, ")") else ""
            
            p <- ggplot(base_grid, aes(x = h, y = .data[[pred_var]])) +
               geom_tile(aes(fill = PredictedSalinity, alpha = Significant95)) +
               scale_fill_gradient(low = "white", high = pos_color,
                                   limits = c(0, panel_max),
                                   name = "Predicted Salinity (ppt)") +
               scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = nonsig_alpha), guide = "none") +
               scale_x_continuous(breaks = seq(2, H_MAX, 2)) +
               labs(title = paste0("Predicted Salinity Surface: ", pred_var, " \u00d7 Horizon", title_suffix),
                    # subtitle = paste0("Conditional on baseline covariates for ", season_title,
                    #                   " | faded = not significantly different from seasonal baseline (p<0.05)"),
                    x = "Forecast Horizon (days)",
                    y = y_label) +
               theme_eval()
            
            safe_name <- gsub("[^A-Za-z0-9_]", "", pred_var)
            if (!is.null(by_var)) {
               safe_name <- paste0(safe_name, "_", gsub("[^A-Za-z0-9_]", "", by_lev))
            }
            
            out_path <- file.path(output_dir, regime_name)
            save_plot_dir(p, out_path, paste0("ResponseSurface_", safe_name), w = 10, h = 6)
            
            if (is.null(by_var)) {
               plots_out[[regime_name]][[pred_var]] <- p
            } else {
               if (is.null(plots_out[[regime_name]][[pred_var]])) plots_out[[regime_name]][[pred_var]] <- list()
               plots_out[[regime_name]][[pred_var]][[by_lev]] <- p
            }
         }
      }
      message("  Saved seasonal response surfaces for: ", pred_var, if (!is.null(by_var)) paste0(" (by ", by_var, ")") else "")
   }
   
   invisible(plots_out)
}

# =============================================================================
# PAIRED GRID — smooth (left column) x tensor surface (right column),
# one row per predictor, 2 x 5 layout. Call once per season, since each
# tensor surface is season-specific (tensor_plots is the already-subsetted
# per-season list, e.g. tensor_output$DrySeason from
# plot_robust_tensor_surfaces()).
#
# Layout choices:
# - Every individual panel gets its own letter, reading left-to-right then
#   top-to-bottom (A = row1-left, B = row1-right, C = row2-left, ...), tucked
#   close above its panel rather than floating with default ggplot spacing.
# - Y-axis shows units only (e.g. "(ppt)"), not "Variable (ppt)" — the row
#   position/color already identifies which predictor a row belongs to, so
#   repeating the name on every axis was redundant.
# - Right column mirrors the left column's y-range (verified upstream — same
#   full observed predictor range feeds both), so its left-side axis text is
#   dropped (ticks kept) and the numbers are mirrored onto a secondary axis
#   on the right instead, avoiding a redundant duplicate label in the middle
#   of the figure.
# - Each tensor panel keeps its own horizontal, top-positioned legend
#   (not a single collected legend), positioned close to its panel.
#
# Requires: library(patchwork). Requires get_units() to be sourced.
# =============================================================================

build_paired_grid <- function(smooth_plots, tensor_plots,
                              row_order = c("LagSalinity", "RollingDischarge50",
                                            "RollingWindCross12", "MaxDischarge10",
                                            "TideRange60"),
                              output_dir, season_name,
                              legend_key_width_cm = 1.3) {
   library(patchwork)
   
   panels <- list()
   letters_used <- LETTERS[seq_len(2 * length(row_order))]
   letter_i <- 0
   
   for (i in seq_along(row_order)) {
      var   <- row_order[i]
      left  <- smooth_plots[[var]]
      right <- tensor_plots[[var]]
      if (is.null(left) || is.null(right)) {
         message("  Skipping row for ", var, " \u2014 missing left or right panel")
         next
      }
      
      units_str <- get_units(var)
      unit_only_label <- if (units_str != "") units_str else NULL
      
      letter_i <- letter_i + 1
      left <- left +
         labs(title = paste0(letters_used[letter_i], ") ", var), subtitle = NULL,
              y = unit_only_label) +
         theme(plot.title   = element_text(face = "bold", hjust = 0, size = 16,
                                           margin = margin(b = 1)),
               plot.margin  = margin(t = 0, r = 5, b = 5, l = 5))
      
      letter_i <- letter_i + 1
      right <- right +
         labs(title = paste0(letters_used[letter_i], ")"), subtitle = NULL,
              y = unit_only_label) +
         scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
         theme(
            plot.title        = element_text(face = "bold", hjust = 0, size = 16,
                                             margin = margin(b = 1)),
            axis.text.y.left  = element_blank(),
            axis.title.y.left = element_blank(),
            axis.ticks.y.left = element_line(),
            legend.position   = "top",
            legend.direction  = "horizontal",
            legend.title      = element_text(size = 9, face = "bold"),
            legend.text       = element_text(size = 8),
            legend.key.width  = unit(legend_key_width_cm, "cm"),
            legend.key.height = unit(0.35, "cm"),
            legend.box.spacing = unit(0.05, "cm"),
            plot.margin       = margin(t = 0, r = 5, b = 5, l = 2)
         )
      
      panels[[length(panels) + 1]] <- left
      panels[[length(panels) + 1]] <- right
   }
   
   combined <- wrap_plots(panels, ncol = 2, nrow = length(row_order))
   
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
   ggsave(file.path(output_dir, paste0("PairedGrid_", season_name, ".png")),
          combined, width = 12, height = 22, dpi = 600)
   ggsave(file.path(output_dir, paste0("PairedGrid_", season_name, ".svg")),
          combined, width = 12, height = 22, dpi = 600)
   
   invisible(combined)
}


# =============================================================================
# FORECAST PANELS
# =============================================================================

plot_salinity_forecast_panels <- function(data,
                                          date_range = NULL,
                                          year       = NULL,
                                          horizons   = NULL,
                                          epa_line   = TRUE,
                                          threshold  = 0.5,
                                          title      = NULL,
                                          NCOL       = 2) {
   
   observed_linewidth <- 0.9
   model_linewidth    <- 1.3
   observed_alpha     <- 0.8
   model_alpha        <- 1.0
   observed_color     <- "#f58220"
   axis_dark          <- "#002030"
   model_palette      <- c("#3b7ea1", "#6a994e", "#8338ec", "#bc4b51",
                           "#fb5607", "#ffbe0b", "#06ffa5", "#c4820e")
   
   if (is.null(horizons)) stop("horizons must be specified.")
   
   if (!"Predicted_SE" %in% names(data)) {
      data$Predicted_SE <- 0
   }
   
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
                          show_x_axis  = FALSE,
                          show_y_title = FALSE,
                          mirror_axis  = FALSE,
                          panel_title  = NULL) {
      
      h_data       <- base_data %>% dplyr::filter(h == h_val)
      series_label <- paste0("h = ", h_val, " days")
      
      obs_df <- h_data %>%
         dplyr::select(TargetDate, Value = Salinity_h) %>%
         dplyr::mutate(Series = "Observed", Lower = NA_real_, Upper = NA_real_) %>%
         add_segments()
      
      mod_df <- h_data %>%
         dplyr::select(TargetDate, Value = Predicted, SE = Predicted_SE) %>%
         dplyr::mutate(
            Series = series_label,
            Lower  = Value - (1.96 * SE),
            Upper  = Value + (1.96 * SE)
         ) %>%
         dplyr::select(-SE) %>%
         add_segments()
      
      plot_long <- dplyr::bind_rows(obs_df, mod_df) %>%
         dplyr::mutate(Series = factor(Series, levels = c("Observed", series_label)),
                       .draw_order = ifelse(Series == "Observed", 2, 1)) %>%
         dplyr::arrange(.draw_order)
      
      p <- ggplot(plot_long, aes(x = TargetDate, y = Value, group = interaction(Series, segment)))
      p <- p + geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Series),
                           alpha = 0.15, color = NA, na.rm = TRUE)
      p <- p + geom_line(aes(color = Series, size = Series, alpha = Series))
      
      if (epa_line) {
         p <- p +
            geom_hline(yintercept = threshold, color = axis_dark, linetype = 2, linewidth = 0.5) +
            annotate("text",
                     x = max(base_data$TargetDate), y = threshold,
                     label = paste0("EPA Standard (", threshold, " ppt)"),
                     hjust = 1, vjust = -0.5, size = 3, colour = axis_dark)
      }
      
      p <- p +
         scale_color_manual(values = c("Observed" = observed_color,
                                       setNames(model_color, series_label))) +
         scale_fill_manual(values = c("Observed" = NA,
                                      setNames(model_color, series_label))) +
         scale_size_manual( values = c("Observed" = observed_linewidth,
                                       setNames(model_linewidth, series_label))) +
         scale_alpha_manual(values = c("Observed" = observed_alpha,
                                       setNames(model_alpha, series_label))) +
         labs(x = if (show_x_axis) "Date" else NULL,
              y = if (show_y_title) "Daily Maximum Salinity (ppt)" else NULL,
              title = panel_title) +
         theme_bw() +
         theme(
            plot.title         = element_text(size = 16, face = "bold", color = axis_dark,
                                              margin = margin(b = 4)),
            axis.title.x       = element_text(size = 14, face = "bold", color = axis_dark),
            axis.title.y.left  = element_text(size = 14, face = "bold", colour = axis_dark),
            axis.text.y.left   = element_text(colour = axis_dark, size = 12),
            axis.text.x        = if (show_x_axis) element_text(size = 12) else element_blank(),
            axis.ticks.x       = if (show_x_axis) element_line() else element_blank(),
            panel.border       = element_blank(),
            axis.line.x.bottom = element_line(colour = axis_dark, linewidth = 0.6),
            axis.line.y.left   = element_line(colour = axis_dark, linewidth = 0.6),
            legend.position    = "none"
         )
      
      if (mirror_axis) {
         # Right-column panels: ticks stay on the left (for visual
         # continuity with the left column), numbers move to a secondary
         # axis on the right, and no left-side title (the left column
         # already carries it).
         p <- p +
            scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
            theme(
               axis.text.y.left  = element_blank(),
               axis.title.y.left = element_blank(),
               axis.ticks.y.left = element_line(),
               axis.text.y.right = element_text(colour = axis_dark, size = 12)
            )
      }
      
      p
   }
   
   n_panels     <- length(horizons)
   n_rows       <- ceiling(n_panels / NCOL)
   panel_colors <- model_palette[(seq_along(horizons) - 1) %% length(model_palette) + 1]
   letters_used <- LETTERS[seq_along(horizons)]
   
   row_idx <- ceiling(seq_along(horizons) / NCOL)
   col_idx <- ((seq_along(horizons) - 1) %% NCOL) + 1
   
   panels <- purrr::pmap(
      list(
         h_val        = horizons,
         model_color  = panel_colors,
         show_x_axis  = (row_idx == n_rows),
         show_y_title = (col_idx == 1),
         mirror_axis  = (col_idx != 1),
         panel_title  = paste0(letters_used, ") ", horizons, "-Day Forecast")
      ),
      make_panel
   )
   
   combined <- patchwork::wrap_plots(panels, ncol = NCOL) &
      theme(plot.margin = margin(2, 10, 2, 10))
   
   if (!is.null(title)) {
      combined <- combined + patchwork::plot_annotation(title = title)
   }
   
   combined
}