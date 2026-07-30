# =============================================================================
# GamEvaluationPlots.R
# Project:   Chapter1
# Author:    Ethan Heidtman
# Purpose:   All helper functions and plotting functions for unified GAM
#            evaluation. Sourced by 05_EvaluateUnifiedGAM.R; no side effects
#            on load. Requires gam_colors and base_dir to be defined in the
#            calling script before source() is called.
#
# GENERALIZATION NOTES (read before modifying):
#   - Predictor -> physical-group classification is done by classify_predictor()
#     using the naming-convention rules confirmed with the user:
#       LagSalinity itself         -> "LagSalinity"
#       contains "RollingDischarge" -> "SustainedDischarge"
#       contains "Max" or "ExceedFlux" -> "FlushingDischarge"
#       contains "Tide"             -> "Tide"
#       contains "RollingWind"      -> "Wind"
#     This assumes those conventions hold for any future candidate model. If
#     they stop holding, classify_predictor() is the only place to update.
#   - predictor_colors and get_units() are now keyed by GROUP name (the five
#     strings above), not by literal variable name, so any candidate model's
#     specific variable (e.g. RollingDischarge35 vs RollingDischarge50) gets
#     the right color/units automatically.
#   - GROUP_ORDER fixes the canonical row/plotting order (LagSalinity,
#     SustainedDischarge, FlushingDischarge, Tide, Wind). The pipeline
#     currently assumes exactly these 5 groups are present in every final
#     model; discover_predictor_groups() errors loudly if that's violated.
#   - WindDir sign convention (which raw wind sign maps to which factor
#     level) is NOT derivable from names or from stacked_train alone, since
#     WindDir doesn't exist in stacked_train until add_wind_dir() creates it.
#     This is a modeling decision from Script 04. get_wind_convention() reads
#     it from gam_unified$wind_dir_convention if present (recommended: have
#     Script 04 attach this), and falls back to "2nd factor level in the
#     model's smooth by.levels = positive" with a warning if absent.
# =============================================================================


# =============================================================================
# PREDICTOR CLASSIFICATION / GROUPING
# =============================================================================

GROUP_ORDER <- c("LagSalinity", "SustainedDischarge", "FlushingDischarge", "Tide", "Wind")

classify_predictor <- function(var_name) {
   if (identical(var_name, "LagSalinity"))        return("LagSalinity")
   if (grepl("RollingDischarge", var_name))       return("SustainedDischarge")
   if (grepl("Max", var_name) || grepl("ExceedFlux", var_name)) return("FlushingDischarge")
   if (grepl("Tide", var_name))                   return("Tide")
   if (grepl("RollingWind", var_name))            return("Wind")
   NA_character_
}

# Given the model's predictor variable names (excluding Response, h, WindDir),
# returns a named vector: names = GROUP_ORDER, values = the actual variable
# name in THIS model belonging to that group. Errors if the model doesn't
# have exactly one variable per group in GROUP_ORDER.
discover_predictor_groups <- function(model_vars) {
   candidates <- setdiff(model_vars, "WindDir")
   grp        <- vapply(candidates, classify_predictor, character(1))
   named      <- setNames(candidates, grp)
   named      <- named[!is.na(names(named))]
   
   missing_groups <- setdiff(GROUP_ORDER, names(named))
   dup_groups     <- names(named)[duplicated(names(named))]
   
   if (length(missing_groups) > 0) {
      stop("Could not identify a predictor for group(s): ",
           paste(missing_groups, collapse = ", "),
           ". Model variables were: ", paste(candidates, collapse = ", "))
   }
   if (length(dup_groups) > 0) {
      stop("More than one model variable classified into group(s): ",
           paste(unique(dup_groups), collapse = ", "),
           ". classify_predictor() rules are ambiguous for this model's variables: ",
           paste(candidates, collapse = ", "))
   }
   
   named[GROUP_ORDER]
}

resolve_predictor_color <- function(var_name, predictor_colors, fallback = "#888888") {
   grp <- classify_predictor(var_name)
   if (is.na(grp) || is.null(predictor_colors[[grp]])) return(fallback)
   predictor_colors[[grp]]
}

get_units <- function(var_name) {
   grp <- classify_predictor(var_name)
   if (is.na(grp)) return("")
   switch(grp,
          LagSalinity         = "(ppt)",
          SustainedDischarge  = "(m\u00b3/s)",
          FlushingDischarge   = "(m\u00b3/s)",
          Tide                = "(m)",
          Wind                = "(m/s)",
          "")
}


# =============================================================================
# WIND SIGN CONVENTION
# =============================================================================

# Returns list(levels = c(neg_level, pos_level), positive_level = pos_level).
# Prefers metadata stored on the model object; falls back to "2nd level in
# the training factor = positive" (matching old hardcoded behavior) with a
# warning, since that convention cannot otherwise be recovered.
get_wind_convention <- function(gam_unified, train_df, wind_dir_var = "WindDir") {
   if (!is.null(gam_unified$wind_dir_convention)) {
      conv <- gam_unified$wind_dir_convention
      if (!is.null(conv$levels) && !is.null(conv$positive_level)) return(conv)
   }
   lv <- levels(train_df[[wind_dir_var]])
   if (length(lv) != 2) stop("Expected exactly 2 ", wind_dir_var, " levels; found ", length(lv))
   warning(
      "No wind_dir_convention metadata found on the model object. Falling back to ",
      "'2nd factor level (", lv[2], ") = positive wind', matching legacy behavior. ",
      "Verify this is correct, and consider having Script 04 attach ",
      "gam_unified$wind_dir_convention going forward."
   )
   list(levels = lv, positive_level = lv[2])
}


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
# HELPER: add WindDir consistent with the model's training convention.
# See get_wind_convention() above for how the sign->level mapping is sourced.
# =============================================================================

add_wind_dir <- function(df, gam_obj, wind_var_name, wind_convention) {
   df[[wind_convention$levels[1]]] # no-op reference to trigger clean errors if malformed
   pos_level <- wind_convention$positive_level
   neg_level <- setdiff(wind_convention$levels, pos_level)
   df$WindDir <- factor(
      ifelse(df[[wind_var_name]] >= 0, pos_level, neg_level),
      levels = wind_convention$levels
   )
   df
}


# =============================================================================
# HELPER: build a median-filled prediction grid
# =============================================================================

make_pred_grid <- function(pred_var, h_seq, pred_seq, train_df, model_vars,
                           gam_obj, wind_var_name, wind_convention,
                           reference_wind_level, reference_wind_value) {
   
   grid           <- expand.grid(h = h_seq, .x = pred_seq)
   names(grid)[2] <- pred_var
   is_wind_grid   <- (pred_var == wind_var_name)
   wind_levels    <- wind_convention$levels
   pos_level      <- wind_convention$positive_level
   neg_level      <- setdiff(wind_levels, pos_level)
   
   for (v in model_vars) {
      if (v %in% names(grid)) next
      if (v == "WindDir") {
         if (is_wind_grid) {
            grid$WindDir <- factor(
               ifelse(grid[[pred_var]] >= 0, pos_level, neg_level),
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
   
   bread <- vcov(model, unconditional = FALSE)
   p_raw <- ncol(bread)
   p_edf <- sum(model$edf)
   
   cat(sprintf("Raw basis dimension (ncol(bread)): %d\n", p_raw))
   cat(sprintf("Effective df (sum(model$edf)):      %.2f\n", p_edf))
   cat("Using p = sum(model$edf) for sig2 fallback and df correction.\n")
   
   cat("Extracting exact (non-discretized) design matrix...\n")
   X <- predict(model, newdata = clean_data, type = "lpmatrix", discrete = FALSE)
   
   y_hat <- as.numeric(predict(model, newdata = clean_data, type = "response",
                               discrete = FALSE))
   response_var_name <- intersect(c("Response", "Salinity_h"), names(clean_data))[1]
   y <- clean_data[[response_var_name]]
   res <- y - y_hat
   
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
   
   df_correction <- (G / (G - 1)) * ((N - 1) / (N - p_edf))
   meat <- meat * df_correction
   
   robust_vcov <- bread %*% meat %*% bread
   colnames(robust_vcov) <- rownames(robust_vcov) <- colnames(bread)
   
   cat("Cluster-robust covariance adjustment complete.\n\n")
   return(robust_vcov)
}

# =============================================================================
# PERFORMANCE METRICS
# =============================================================================

plot_performance_metrics <- function(perf_hold, H_MAX, gam_colors, dir) {
   
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
   
   save_plot_dir(p_error, dir, "Error_ByLeadTime", w = 10, h = 6)
   save_plot_dir(p_r2,  dir,  "R2_ByLeadTime",    w = 10, h = 6)
   ggsave(file.path(dir, "Performance_Combined.png"), plot = p_combined, width = 14, height = 6, dpi = 600)
   ggsave(file.path(dir, "Performance_Combined.svg"), plot = p_combined, width = 14, height = 6)
   
   invisible(list(error = p_error, r2 = p_r2, combined = p_combined))
}

# =============================================================================
# RESIDUAL DIAGNOSTICS
# H_MAX-derived bin breaks: 4 bins spanning 1..H_MAX regardless of H_MAX value.
# =============================================================================

plot_residual_diagnostics <- function(stacked_hold, H_MAX,
                                      HIGH_SALINITY_THRESHOLD,
                                      gam_colors, dir) {
   
   h_breaks <- unique(round(seq(0, H_MAX, length.out = 5)))
   h_labels <- sapply(seq_len(length(h_breaks) - 1), function(i) {
      lo <- h_breaks[i] + 1
      hi <- h_breaks[i + 1]
      if (lo == hi) paste0("h = ", lo) else paste0("h = ", lo, "\u2013", hi)
   })
   
   resid_df <- stacked_hold %>%
      filter(!is.na(Residual)) %>%
      mutate(HBin = cut(h, breaks = h_breaks, labels = h_labels)) %>%
      filter(!is.na(HBin))
   
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
   
   save_plot_dir(p_qq,   dir,        "QQ_ByHBin",             w = 10, h = 8)
   save_plot_dir(p_resid_hist,  dir,  "ResidHist_ByHBin",      w = 10, h = 8)
   save_plot_dir(p_resid_fitted, dir,  "ResidVsFitted_ByHBin",  w = 10, h = 8)
   
   invisible(list(qq = p_qq, hist = p_resid_hist, resid_fitted = p_resid_fitted))
}

# =============================================================================
# CALIBRATION
# =============================================================================

plot_calibration <- function(stacked_hold, H_MAX, N_CAL_BINS, gam_colors, dir) {
   library(dplyr)
   library(ggplot2)
   
   h_breaks <- unique(round(seq(0, H_MAX, length.out = 5)))
   h_labels <- sapply(seq_len(length(h_breaks) - 1), function(i) {
      lo <- h_breaks[i] + 1
      hi <- h_breaks[i + 1]
      if (lo == hi) paste0("h = ", lo) else paste0("h = ", lo, "\u2013", hi)
   })
   
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
   
   ggsave(file.path(dir, "Model_Calibration.png"), plot = p_cal, width = 10, height = 8, dpi = 600)
   ggsave(file.path(dir, "Model_Calibration.svg"), plot = p_cal, width = 10, height = 8)
   
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
# 1D MARGINAL SMOOTH PLOTS
#
# Two-pass design: (1) compute all curve data frames (wind-combined and all
# other 1D smooths) WITHOUT plotting, tracking the global min/max of the
# partial-effect scale across every curve; (2) plot each with a shared xlim
# derived from that global range, so all panels share an axis regardless of
# what range this particular model's fitted effects happen to span.
#
# Requires: get_units() to be sourced first.
# =============================================================================

plot_1d_smooths <- function(gam_obj, s_labels, stacked_train, model_vars,
                            wind_var_name, wind_convention,
                            reference_wind_level, reference_wind_value,
                            predictor_colors, smooth_dir, h_fallback_color = "#002030",
                            wind_level_labels = NULL, xlim_pad = 0.05) {
   
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
   
   curve_data <- list()  # named by variable -> data.frame(s) of fit/lower/upper
   
   # ---- (A) Combined wind smooth: compute curve data ----
   wind_df <- NULL
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
         curve_data[[wind_var_name]] <- wind_df
      }
   }
   
   # ---- (B) All other 1D smooths: compute curve data ----
   other_curves <- list()
   for (s_label in other_s_labels) {
      inner    <- gsub("^s\\(|\\).*$", "", s_label)
      var_name <- trimws(strsplit(inner, ",")[[1]])[1]
      if (!var_name %in% names(stacked_train)) next
      
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
      
      other_curves[[var_name]] <- list(s_label = s_label, df = pred_df)
      curve_data[[var_name]] <- pred_df
   }
   
   # ---- Shared xlim across every curve computed above ----
   all_bounds <- unlist(lapply(curve_data, function(d) c(d$lower, d$upper)))
   if (length(all_bounds) == 0 || all(is.na(all_bounds))) {
      shared_xlim <- c(-1, 1)  # degenerate fallback; shouldn't occur in practice
   } else {
      rng  <- range(all_bounds, na.rm = TRUE)
      pad  <- diff(rng) * xlim_pad
      if (pad == 0) pad <- 0.1 * max(abs(rng), 1)
      shared_xlim <- c(rng[1] - pad, rng[2] + pad)
   }
   
   plots <- list()
   
   # ---- (A) Plot combined wind smooth ----
   if (!is.null(wind_df) && nrow(wind_df) > 0) {
      label_df <- wind_df %>%
         group_by(Level) %>%
         slice_min(abs(x - mean(range(x))), n = 1, with_ties = FALSE) %>%
         ungroup() %>%
         mutate(LabelText = if (!is.null(wind_level_labels) && all(Level %in% names(wind_level_labels))) {
            unname(wind_level_labels[Level])
         } else {
            as.character(Level)
         })
      
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
         theme_eval() + theme(legend.position = "none") +
         xlim(shared_xlim)
      
      save_plot_dir(p_wind, smooth_dir, paste0("Smooth_", wind_var_name, "_Combined"), w = 8, h = 5)
      plots[[wind_var_name]] <- p_wind
   }
   
   # ---- (B) Plot all other 1D smooths ----
   for (var_name in names(other_curves)) {
      s_label <- other_curves[[var_name]]$s_label
      pred_df <- other_curves[[var_name]]$df
      
      line_color <- if (var_name == "h") h_fallback_color else resolve_predictor_color(var_name, predictor_colors, fallback = "#888888")
      units_str  <- get_units(var_name)
      y_label    <- if (units_str != "") paste(var_name, units_str) else var_name
      
      p_smooth <- ggplot(pred_df, aes(x = fit, y = x)) +
         geom_vline(xintercept = 0, linetype = "dashed", color = "#002030", linewidth = 0.5) +
         geom_ribbon(aes(xmin = lower, xmax = upper), fill = line_color, alpha = 0.2) +
         geom_path(color = line_color, linewidth = 1.1) +
         geom_rug(data = data.frame(x = stacked_train[[var_name]]), aes(y = x), inherit.aes = FALSE, sides = "l", alpha = 0.2, color = "#002030") +
         labs(title = paste0("Partial Dependence Plot: s(", var_name, ")"),
              y     = y_label, x     = "Partial Effect (ppt)") +
         theme_eval() +
         xlim(shared_xlim)
      
      safe_name <- gsub("[^A-Za-z0-9_]", "", s_label)
      save_plot_dir(p_smooth, smooth_dir, paste0("Smooth_", safe_name), w = 8, h = 5)
      plots[[var_name]] <- p_smooth
   }
   
   invisible(plots)
}

# =============================================================================
# LagSalinity (or whichever group has no marginal s() term) rug/density panel.
# Requires: get_units() to be sourced first.
# =============================================================================

plot_lag_salinity_rug <- function(stacked_train, predictor_colors, var_name) {
   color_val <- resolve_predictor_color(var_name, predictor_colors, fallback = "#888888")
   units_str <- get_units(var_name)
   y_label   <- if (units_str != "") paste(var_name, units_str) else var_name
   
   df <- data.frame(x = stacked_train[[var_name]])
   
   ggplot(df, aes(y = x)) +
      geom_rug(aes(x = 0), sides = "l", color = "#002030", alpha = 0.25) +
      geom_density(aes(x = after_stat(density)), fill = color_val, alpha = 0.25,
                   color = color_val, orientation = "y") +
      labs(title = paste0(var_name, ": No Marginal Smooth"),
           subtitle = paste0(var_name, " enters the model only through a tensor interaction \u2014\n",
                             "no additive s() term exists to plot"),
           y = y_label, x = "Density") +
      theme_eval()
}

# =============================================================================
# ROBUST TENSOR SURFACES
# =============================================================================

plot_robust_tensor_surfaces <- function(gam_obj, ti_labels, stacked_train, model_vars,
                                        wind_var_name, wind_convention,
                                        reference_wind_level, reference_wind_value,
                                        predictor_colors, H_MAX, output_dir,
                                        sig_z = 1.96,
                                        nonsig_alpha = 0.30,
                                        grid_n_pred = 150) {
   library(mgcv)
   library(dplyr)
   library(ggplot2)
   library(lubridate)
   
   dry_data <- stacked_train %>% filter(month(DateTime) %in% c(8, 9, 10))
   wet_data <- stacked_train %>% filter(month(DateTime) %in% c(3, 4, 5))
   
   regimes <- list(
      "DrySeason" = dry_data,
      "WetSeason" = wet_data
   )
   
   for (regime_name in names(regimes)) {
      dir.create(file.path(output_dir, regime_name), recursive = TRUE, showWarnings = FALSE)
   }
   
   plots_out <- setNames(vector("list", length(regimes)), names(regimes))
   
   # ---------------------------------------------------------------------
   # Group ti() smooth objects by their underlying term set, collapsing
   # by-level duplicates (e.g. a factor by= producing WindDirLeftBank /
   # WindDirRightBank as separate smooth objects) into one group. This
   # reads structure off gam_obj$smooth directly rather than parsing
   # label strings, since mgcv's label format for a factor `by=` term
   # (e.g. "ti(h,RollingWindCross14):WindDirLeftBank") does not contain
   # a parseable "by=" substring the way a continuous `by=` term does.
   # ---------------------------------------------------------------------
   ti_smooth_objs <- Filter(function(s) grepl("^ti\\(", s$label), gam_obj$smooth)
   ti_keys        <- sapply(ti_smooth_objs, function(s) paste(s$term, collapse = ","))
   ti_groups      <- split(ti_smooth_objs, ti_keys)
   
   for (ti_key in names(ti_groups)) {
      group      <- ti_groups[[ti_key]]
      ref_smooth <- group[[1]]
      
      vars_in <- ref_smooth$term
      by_var  <- if (!is.null(ref_smooth$by) && !identical(ref_smooth$by, "NA")) ref_smooth$by else NULL
      
      pred_var <- vars_in[vars_in != "h"]
      if (length(pred_var) != 1 || !pred_var %in% names(stacked_train)) next
      
      pos_color  <- resolve_predictor_color(pred_var, predictor_colors)
      units_str  <- get_units(pred_var)
      y_label    <- if (units_str != "") paste(pred_var, units_str) else pred_var
      
      h_seq     <- 1:H_MAX
      pred_range <- range(stacked_train[[pred_var]], na.rm = TRUE)
      pred_seq   <- seq(pred_range[1], pred_range[2], length.out = grid_n_pred)
      
      for (regime_name in names(regimes)) {
         regime_data <- regimes[[regime_name]]
         
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
         
         is_wind_grid <- (pred_var == wind_var_name)
         wind_levels  <- wind_convention$levels
         pos_level    <- wind_convention$positive_level
         neg_level    <- setdiff(wind_levels, pos_level)
         
         # Detect when the ti()'s by= term IS the wind sign-split variable itself.
         # In this case we stitch one surface from both bases rather than looping levels.
         stitch_wind_by <- is_wind_grid && !is.null(by_var) && by_var == "WindDir"
         
         by_levels <- if (!is.null(by_var) && !stitch_wind_by) levels(stacked_train[[by_var]]) else list(NULL)
         
         for (by_lev in by_levels) {
            
            base_grid <- expand.grid(h = h_seq, pred_target = pred_seq)
            names(base_grid)[2] <- pred_var
            
            for (v in model_vars) {
               if (v == "h" || v == pred_var) next
               if (v == "WindDir" && is_wind_grid) {
                  base_grid$WindDir <- factor(
                     ifelse(base_grid[[pred_var]] >= 0, pos_level, neg_level),
                     levels = wind_levels
                  )
               } else if (v %in% names(regime_baselines)) {
                  if (!is.null(by_var) && v == by_var && !stitch_wind_by) {
                     base_grid[[v]] <- factor(by_lev, levels = levels(regime_data[[v]]))
                  } else {
                     base_grid[[v]] <- regime_baselines[[v]]
                  }
               }
            }
            
            baseline_row <- list()
            for (v in model_vars) {
               if (v %in% names(regime_baselines)) baseline_row[[v]] <- regime_baselines[[v]]
            }
            baseline_row[["h"]] <- mean(regime_data$h, na.rm = TRUE)
            if (!is.null(by_var) && !stitch_wind_by) {
               baseline_row[[by_var]] <- factor(by_lev, levels = levels(regime_data[[by_var]]))
            } else if (stitch_wind_by) {
               baseline_row[["WindDir"]]     <- factor(reference_wind_level, levels = wind_levels)
               baseline_row[[wind_var_name]] <- reference_wind_value
            }
            baseline_df <- as.data.frame(baseline_row)
            
            resp_predictions <- tryCatch(
               predict(gam_obj, newdata = base_grid, type = "response"),
               error = function(e) NULL
            )
            if (is.null(resp_predictions)) next
            base_grid$PredictedSalinity <- pmax(0, as.numeric(resp_predictions))
            
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
               base_grid$Significant95 <- TRUE
            }
            
            panel_max <- max(base_grid$PredictedSalinity, na.rm = TRUE)
            if (panel_max <= 0) panel_max <- 1
            
            title_suffix <- if (!is.null(by_var) && !stitch_wind_by) paste0(" (", by_lev, ")") else ""
            
            p <- ggplot(base_grid, aes(x = h, y = .data[[pred_var]])) +
               geom_tile(aes(fill = PredictedSalinity, alpha = Significant95)) +
               scale_fill_gradient(low = "white", high = pos_color,
                                   limits = c(0, panel_max),
                                   name = "Predicted Salinity (ppt)") +
               scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = nonsig_alpha), guide = "none") +
               scale_x_continuous(breaks = seq(2, H_MAX, 2), expand = c(0, 0)) +
               scale_y_continuous(expand = c(0, 0)) +
               labs(title = paste0("Predicted Salinity Surface: ", pred_var, " \u00d7 Horizon", title_suffix),
                    x = "Forecast Horizon (days)",
                    y = y_label) +
               theme_eval() +
               theme(
                  panel.grid       = element_blank(),
                  panel.background = element_rect(fill = "grey92", color = NA)
               )
            
            safe_name <- gsub("[^A-Za-z0-9_]", "", pred_var)
            if (!is.null(by_var) && !stitch_wind_by) {
               safe_name <- paste0(safe_name, "_", gsub("[^A-Za-z0-9_]", "", by_lev))
            }
            
            out_path <- file.path(output_dir, regime_name)
            save_plot_dir(p, out_path, paste0("ResponseSurface_", safe_name), w = 10, h = 6)
            
            if (is.null(by_var) || stitch_wind_by) {
               plots_out[[regime_name]][[pred_var]] <- p
            } else {
               if (is.null(plots_out[[regime_name]][[pred_var]])) plots_out[[regime_name]][[pred_var]] <- list()
               plots_out[[regime_name]][[pred_var]][[by_lev]] <- p
            }
         }
      }
      message("  Saved seasonal response surfaces for: ", pred_var, if (!is.null(by_var) && !stitch_wind_by) paste0(" (by ", by_var, ")") else "")
   }
   
   invisible(plots_out)
}

# =============================================================================
# PAIRED GRID — row_order and H_MAX are now required arguments (no hardcoded
# defaults), derived by the caller from the model's discovered predictor
# groups. See Script 05.
# =============================================================================

build_paired_grid <- function(smooth_plots, tensor_plots, row_order,
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

build_smooth_grid <- function(smooth_plots, var_order,
                              output_dir, ncol = 2, nrow = 2) {
   library(patchwork)
   
   letters_used <- LETTERS[seq_along(var_order)]
   panels <- list()
   
   n_panels <- length(var_order)
   n_rows   <- ceiling(n_panels / ncol)
   row_idx  <- ceiling(seq_along(var_order) / ncol)
   col_idx  <- ((seq_along(var_order) - 1) %% ncol) + 1
   
   for (i in seq_along(var_order)) {
      var <- var_order[i]
      p   <- smooth_plots[[var]]
      if (is.null(p)) {
         message("  Skipping smooth panel for ", var, " \u2014 not found")
         next
      }
      
      units_str       <- get_units(var)
      unit_only_label <- if (units_str != "") units_str else NULL
      
      show_x_axis  <- (row_idx[i] == n_rows)
      is_right_col <- (col_idx[i] != 1)
      
      p <- p +
         labs(title = paste0(letters_used[i], ") ", var), subtitle = NULL,
              y = if (is_right_col) NULL else unit_only_label) +
         theme(plot.title   = element_text(face = "bold", hjust = 0, size = 16,
                                           margin = margin(b = 1)),
               plot.margin  = margin(t = 0, r = 5, b = 5, l = 5),
               axis.text.x  = if (show_x_axis) element_text() else element_blank(),
               axis.ticks.x = if (show_x_axis) element_line() else element_blank())
      
      if (is_right_col) {
         p <- p +
            scale_y_continuous(sec.axis = dup_axis(name = unit_only_label)) +
            theme(
               axis.text.y.left   = element_blank(),
               axis.title.y.left  = element_blank(),
               axis.ticks.y.left  = element_line(),
               axis.text.y.right  = element_text(),
               axis.title.y.right = element_text(size = 14, face = "bold")
            )
      }
      
      panels[[length(panels) + 1]] <- p
   }
   
   combined <- wrap_plots(panels, ncol = ncol, nrow = nrow)
   
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
   ggsave(file.path(output_dir, "SmoothGrid_Supplemental.png"),
          combined, width = 5 * ncol, height = 5 * nrow, dpi = 600)
   ggsave(file.path(output_dir, "SmoothGrid_Supplemental.svg"),
          combined, width = 5 * ncol, height = 5 * nrow, dpi = 600)
   
   invisible(combined)
}

build_tensor_grid <- function(tensor_plots, var_order, H_MAX,
                              output_dir, season_name, ncol = 3, nrow = 2) {
   library(patchwork)
   library(ggh4x)
   
   letters_used <- LETTERS[seq_along(var_order)]
   panels <- list()
   
   n_panels <- length(var_order)
   n_rows   <- ceiling(n_panels / ncol)
   row_idx  <- ceiling(seq_along(var_order) / ncol)
   col_idx  <- ((seq_along(var_order) - 1) %% ncol) + 1
   
   for (i in seq_along(var_order)) {
      var <- var_order[i]
      p   <- tensor_plots[[var]]
      if (is.null(p)) {
         message("  Skipping tensor panel for ", var, " \u2014 not found")
         next
      }
      
      is_right_col <- (col_idx[i] != 1)
      is_bottom    <- (row_idx[i] == n_rows)
      col_has_panel_below <- any(col_idx == col_idx[i] & row_idx > row_idx[i])
      show_x_labels <- is_bottom || !col_has_panel_below
      
      p <- p +
         labs(title = paste0(letters_used[i], ") ", var), subtitle = NULL) +
         scale_x_continuous(
            expand       = c(0, 0),
            breaks       = seq(2, H_MAX, 2),
            minor_breaks = seq(1, H_MAX, 1),
            guide        = guide_axis_minor()
         ) +
         scale_y_continuous(
            expand    = c(0, 0),
            guide     = guide_axis_minor(),
            sec.axis  = dup_axis(name = if (is_right_col) derive() else NULL,
                                 labels = if (is_right_col) waiver() else NULL)
         ) +
         theme(
            plot.title          = element_text(face = "bold", hjust = 0, size = 16,
                                               margin = margin(b = 1)),
            legend.position     = "top",
            legend.direction    = "horizontal",
            legend.title        = element_text(size = 9, face = "bold"),
            legend.text         = element_text(size = 8),
            legend.key.width    = unit(1.3, "cm"),
            legend.key.height   = unit(0.35, "cm"),
            legend.box.spacing  = unit(0.05, "cm"),
            plot.margin         = margin(t = 0, r = 5, b = 5, l = 5),
            axis.ticks.y.left   = element_line(),
            axis.ticks.y.right  = element_line(),
            axis.text.y.right   = if (is_right_col) element_text() else element_blank(),
            axis.text.y.left    = if (is_right_col) element_blank() else element_text(),
            axis.title.y.left   = if (is_right_col) element_blank() else element_text(),
            axis.title.y.right  = if (is_right_col) element_text() else element_blank(),
            ggh4x.axis.ticks.length.minor = rel(0.5),
            axis.ticks.x        = element_line(),
            axis.text.x         = if (show_x_labels) element_text() else element_blank(),
            axis.title.x        = if (show_x_labels) element_text() else element_blank()
         )
      
      panels[[length(panels) + 1]] <- p
   }
   
   combined <- wrap_plots(panels, ncol = ncol, nrow = nrow)
   
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
   ggsave(file.path(output_dir, paste0("TensorGrid_", season_name, ".png")),
          combined, width = 6 * ncol, height = 6 * nrow, dpi = 600)
   ggsave(file.path(output_dir, paste0("TensorGrid_", season_name, ".svg")),
          combined, width = 6 * ncol, height = 6 * nrow, dpi = 600)
   
   invisible(combined)
}

# =============================================================================
# FORECAST PANELS — unchanged in substance; no predictor-name hardcoding
# existed here to begin with (it operates on Salinity_h/Predicted/h/
# DateTime, which are fixed pipeline column names, not candidate-specific).
# =============================================================================

plot_salinity_forecast_panels <- function(data,
                                          date_range = NULL,
                                          year       = NULL,
                                          horizons   = NULL,
                                          epa_line   = TRUE,
                                          threshold  = 0.5,
                                          title      = NULL,
                                          NCOL       = 2,
                                          y_expand   = c(0.05, 0.05)) {
   
   observed_linewidth <- 0.9
   model_linewidth    <- 1.3
   observed_alpha     <- 0.8
   model_alpha        <- 1.0
   observed_color     <- "#f58220"
   axis_dark          <- "#002030"
   model_color        <- "#009bba"
   
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
   
   shared_y_range <- base_data %>%
      dplyr::mutate(
         Lower = Predicted - (1.96 * Predicted_SE),
         Upper = Predicted + (1.96 * Predicted_SE)
      ) %>%
      dplyr::summarise(
         lo = min(c(Salinity_h, Lower, if (epa_line) threshold else NA), na.rm = TRUE),
         hi = max(c(Salinity_h, Upper, if (epa_line) threshold else NA), na.rm = TRUE)
      )
   shared_y_limits <- c(shared_y_range$lo, shared_y_range$hi)
   
   add_segments <- function(df) {
      df %>%
         dplyr::arrange(TargetDate) %>%
         dplyr::mutate(
            dt      = as.numeric(difftime(TargetDate, dplyr::lag(TargetDate), units = "secs")),
            base_dt = median(dt, na.rm = TRUE),
            segment = cumsum(is.na(dt) | dt > 1.5 * base_dt)
         )
   }
   
   make_panel <- function(h_val,
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
         scale_x_datetime(expand = c(0, 0)) +
         scale_y_continuous(limits = shared_y_limits, expand = expansion(mult = y_expand)) +
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
         p <- p +
            scale_y_continuous(limits = shared_y_limits, expand = expansion(mult = y_expand),
                               sec.axis = dup_axis(name = NULL)) +
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
   letters_used <- LETTERS[seq_along(horizons)]
   
   row_idx <- ceiling(seq_along(horizons) / NCOL)
   col_idx <- ((seq_along(horizons) - 1) %% NCOL) + 1
   
   panels <- purrr::pmap(
      list(
         h_val        = horizons,
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