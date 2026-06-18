# =============================================================================
# GAM Discharge Scenario Sensitivity — Two-Experiment Framework
#
# Experiment 1 (Counterfactual): Full-season chain, tests what would have
#   happened under a different hydrological reality. Pre-release window runs
#   from PRERELEASE_START to EVENT_START. No operational constraint.
#
# Experiment 2 (Forecast-Operations): Chain initialized INIT_DAYS_BEFORE_EVENT
#   days before event onset. Pre-release scenarios are sustained ramps from
#   that initialization date. Represents an operator responding to a forecast.
#
# Outputs per experiment:
#   - Individual trajectory plots per lead time
#   - Summary sensitivity plot (mitigation vs lead time, by scenario)
# Final output:
#   - Cross-experiment comparison panel
# =============================================================================

library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tidyr)
library(mgcv)
library(svglite)
library(ggplot2)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Plots/SimpleModels/ModelEvaluationPlots.R')
source('Scripts/Plots/MultiPanelModelPlot.R')
source('Scripts/Utilities/ComputeGamPerformance.R')
source('Scripts/Plots/GamEvalPlots.R')

# =============================================================================
# USER INPUTS
# =============================================================================

lead_times           <- seq(1, 30, 1)
YEAR                 <- 2016
EVENT_START          <- as.Date("2016-10-09")
EVENT_END            <- as.Date("2016-10-24")
PLOT_START           <- as.Date("2016-09-01")
PLOT_END             <- as.Date("2016-11-30")

# Experiment 1 pre-release window
EXP1_PRERELEASE_START <- as.Date("2016-08-01")
EXP1_PRERELEASE_END   <- EVENT_START

# Experiment 2: how many days before event to initialize chain
INIT_DAYS_BEFORE_EVENT <- 15
EXP2_SIM_START        <- EVENT_START - INIT_DAYS_BEFORE_EVENT   # 2016-09-24
EXP2_PRERELEASE_START <- EXP2_SIM_START
EXP2_PRERELEASE_END   <- EVENT_START

OUT_BASE <- "Outputs/Plots/SensitivitySimulations/Discharge"
OUT_EXP1 <- file.path(OUT_BASE, "CounterfactualExperiment")
OUT_EXP2 <- file.path(OUT_BASE, "ForecastOperationExperiment")
OUT_COMP <- file.path(OUT_BASE, "Comparison")
for (d in c(OUT_EXP1, OUT_EXP2, OUT_COMP)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD DATA AND MODELS
# =============================================================================

raw_model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/FinalModelData.qs'))
FLUSH_THRESHOLD <- quantile(
   raw_model_data$MaxDischarge[month(as.Date(raw_model_data$DateTime)) %in% c(8, 9, 10, 11)],
   0.90, na.rm = TRUE
)

screened_data <- list()
rf_results    <- list()
models        <- list()

for (k in lead_times) {
   lag_name <- paste0("lag", k)
   screened_data[[lag_name]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   rf_results[[lag_name]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
   models[[paste0('Lag', k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyGAM/Gam_', k, '.qs')
   )
}

# Climatological discharge baseline (used for RollingAnomaly predictor)
full_data <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/FinalModelData.qs'))
clim_discharge <- full_data %>%
   mutate(DayOfYear = as.numeric(format(as.Date(DateTime), "%j"))) %>%
   group_by(DayOfYear) %>%
   summarise(ClimDischarge = mean(Discharge, na.rm = TRUE), .groups = 'drop') %>%
   mutate(ClimDischarge = zoo::rollmean(ClimDischarge, 15, fill = "extend", align = "center"))
rm(full_data)

# Year-filtered raw data
raw_df <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/FinalModelData.qs')) %>%
   filter(Year == YEAR) %>%
   arrange(DateTime) %>%
   mutate(DayOfYear = as.numeric(format(as.Date(DateTime), "%j"))) %>%
   left_join(clim_discharge, by = 'DayOfYear')

raw_q <- raw_df$Discharge

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

extract_window <- function(nm) {
   as.integer(regmatches(nm, regexpr("[0-9]+$", nm)))
}

compute_discharge_preds <- function(q, var_names, clim_vals) {
   out <- list()
   for (nm in var_names) {
      w <- extract_window(nm)
      if      (grepl("RollingDischarge", nm)) out[[nm]] <- zoo::rollmean(q, w, fill = NA, align = "right")
      else if (grepl("RollingAnomaly",   nm)) out[[nm]] <- zoo::rollmean(q / clim_vals, w, fill = NA, align = "right")
      else if (grepl("ExceedFlux",       nm)) out[[nm]] <- zoo::rollsum(pmax(0, q - FLUSH_THRESHOLD), w, fill = NA, align = "right")
      else if (grepl("MaxDischarge",     nm)) out[[nm]] <- zoo::rollapply(q, w, max, fill = NA, align = "right")
   }
   as.data.frame(out)
}

# =============================================================================
# SCENARIO DEFINITIONS
# Scenarios are defined as functions of (q, dates, prerelease_start,
# prerelease_end, event_start, event_end) so they work for both experiments.
# =============================================================================

in_window <- function(dates, start, end) as.Date(dates) >= start & as.Date(dates) < end
in_event  <- function(dates, start, end) as.Date(dates) >= start & as.Date(dates) <= end

make_scenarios <- function(prerelease_start, prerelease_end, event_start, event_end, q, dates) {
   
   event_mean <- mean(q[in_event(dates, event_start, event_end)], na.rm = TRUE)
   
   list(
      list(label = "Observed",          group = "Observed",
           modifier = function(q, d) q),
      
      list(label = "Pre-release \u00d71.5", group = "Pre-release",
           modifier = function(q, d) { q_new <- q; q_new[in_window(d, prerelease_start, prerelease_end)] <- q_new[in_window(d, prerelease_start, prerelease_end)] * 1.5; q_new }),
      list(label = "Pre-release \u00d72",   group = "Pre-release",
           modifier = function(q, d) { q_new <- q; q_new[in_window(d, prerelease_start, prerelease_end)] <- q_new[in_window(d, prerelease_start, prerelease_end)] * 2.0; q_new }),
      list(label = "Pre-release \u00d73",   group = "Pre-release",
           modifier = function(q, d) { q_new <- q; q_new[in_window(d, prerelease_start, prerelease_end)] <- q_new[in_window(d, prerelease_start, prerelease_end)] * 3.0; q_new }),
      
      list(label = "Event pulse \u00d72",   group = "Pulse",
           modifier = function(q, d) { q_new <- q; q_new[in_event(d, event_start, event_end)] <- event_mean * 2; q_new }),
      list(label = "Event pulse \u00d73",   group = "Pulse",
           modifier = function(q, d) { q_new <- q; q_new[in_event(d, event_start, event_end)] <- event_mean * 3; q_new }),
      list(label = "Event pulse \u00d75",   group = "Pulse",
           modifier = function(q, d) { q_new <- q; q_new[in_event(d, event_start, event_end)] <- event_mean * 5; q_new }),
      
      list(label = "Pre \u00d72 + Pulse \u00d73", group = "Combined",
           modifier = function(q, d) {
              q_new <- q
              q_new[in_window(d, prerelease_start, prerelease_end)] <- q_new[in_window(d, prerelease_start, prerelease_end)] * 2.0
              q_new[in_event(d, event_start, event_end)]            <- event_mean * 3
              q_new }),
      list(label = "Pre \u00d72 + Pulse \u00d75", group = "Combined",
           modifier = function(q, d) {
              q_new <- q
              q_new[in_window(d, prerelease_start, prerelease_end)] <- q_new[in_window(d, prerelease_start, prerelease_end)] * 2.0
              q_new[in_event(d, event_start, event_end)]            <- event_mean * 5
              q_new })
   )
}

# Plot styling — shared across experiments
scenario_colors <- c(
   "Simulated (obs. Q)"       = "#002030",
   "Pre-release \u00d71.5"    = "#9ecae1",
   "Pre-release \u00d72"      = "#3182bd",
   "Pre-release \u00d73"      = "#08519c",
   "Event pulse \u00d72"      = "#a1d99b",
   "Event pulse \u00d73"      = "#41ab5d",
   "Event pulse \u00d75"      = "#006d2c",
   "Pre \u00d72 + Pulse \u00d73" = "#d4a0e0",
   "Pre \u00d72 + Pulse \u00d75" = "#8338ec"
)
scenario_lwd <- c(
   "Simulated (obs. Q)" = 1.0,
   "Pre-release \u00d71.5" = 0.6, "Pre-release \u00d72" = 0.6, "Pre-release \u00d73" = 0.6,
   "Event pulse \u00d72"   = 0.6, "Event pulse \u00d73"  = 0.6, "Event pulse \u00d75"  = 0.6,
   "Pre \u00d72 + Pulse \u00d73" = 0.8, "Pre \u00d72 + Pulse \u00d75" = 0.8
)
scenario_lty <- c(
   "Simulated (obs. Q)" = "dashed",
   "Pre-release \u00d71.5" = "solid", "Pre-release \u00d72" = "solid", "Pre-release \u00d73" = "solid",
   "Event pulse \u00d72"   = "solid", "Event pulse \u00d73"  = "solid", "Event pulse \u00d75"  = "solid",
   "Pre \u00d72 + Pulse \u00d73" = "solid", "Pre \u00d72 + Pulse \u00d75" = "solid"
)
group_colors <- c("Pre-release" = "#3182bd", "Pulse" = "#41ab5d", "Combined" = "#8338ec")

# =============================================================================
# CORE SIMULATION FUNCTION
# Handles both experiments via sim_start parameter.
# If sim_start is NULL, uses all available rows (Experiment 1 behaviour).
# If sim_start is a Date, rows before that date use observed salinity as
# the AR input and discharge modifications are ignored — the chain only
# runs from sim_start onward.
# =============================================================================

run_experiment <- function(experiment_label,
                           sim_start,        # Date or NULL
                           prerelease_start,
                           prerelease_end,
                           event_start,
                           event_end,
                           out_dir) {
   
   cat(sprintf("\n====== %s ======\n", experiment_label))
   
   scenarios    <- make_scenarios(prerelease_start, prerelease_end,
                                  event_start, event_end, raw_q, raw_df$DateTime)
   all_labels   <- sapply(scenarios, `[[`, "label")
   summary_rows <- list()
   
   for (k in lead_times) {
      
      lag_name <- paste0("lag", k)
      gam_obj  <- models[[paste0("Lag", k)]]$gam_object
      cat(sprintf("  Lag%d\n", k))
      
      # ---- Identify predictor structure ------------------------------------
      gam_cols      <- names(gam_obj$model)
      pred_cols_k   <- gam_cols[-1]
      base_names    <- sub(paste0("_", k, "$"), "", pred_cols_k)
      
      is_discharge  <- grepl("RollingDischarge|RollingAnomaly|ExceedFlux|MaxDischarge", base_names)
      is_lag_sal    <- grepl("LagSalinity|Salinity", base_names)
      is_winddir    <- base_names == "WindDir"
      
      discharge_base <- base_names[is_discharge]
      other_base     <- base_names[!is_discharge & !is_lag_sal & !is_winddir]
      wind_base_var  <- other_base[grepl("RollingWindAlong|RollingWindCross", other_base)]
      
      lag_sal_col <- paste0("LagSalinity_", k)
      
      # ---- Build predictor dataframe for a given discharge vector ----------
      build_pred_df <- function(q_modified) {
         disc_df  <- compute_discharge_preds(q_modified, discharge_base,
                                             clim_vals = raw_df$ClimDischarge)
         base_df  <- bind_cols(
            raw_df %>% dplyr::select(DateTime, Salinity, all_of(other_base)),
            disc_df
         )
         all_base <- c(other_base, discharge_base)
         shifted  <- base_df %>%
            mutate(across(all_of(all_base), ~ dplyr::lag(.x, k),
                          .names = paste0("{.col}_", k)))
         shifted[[lag_sal_col]] <- dplyr::lag(base_df$Salinity, k)
         
         if (length(wind_base_var) >= 1) {
            shifted_wind_col <- paste0(wind_base_var[1], "_", k)
            wind_levels      <- levels(gam_obj$model$WindDir)
            if (grepl("Along", wind_base_var[1])) {
               shifted <- shifted %>% mutate(WindDir = factor(
                  ifelse(.data[[shifted_wind_col]] >= 0, "UpEstuary", "DownEstuary"),
                  levels = wind_levels))
            } else {
               shifted <- shifted %>% mutate(WindDir = factor(
                  ifelse(.data[[shifted_wind_col]] >= 0, "RightBank", "LeftBank"),
                  levels = wind_levels))
            }
         }
         shifted
      }
      
      # ---- Chained forward simulation --------------------------------------
      # For Experiment 2: rows before sim_start always use observed lagged
      # salinity (pred_history stays NA), so the chain effectively starts
      # from sim_start. Discharge modifications applied to the full q vector
      # but only affect the chain once active rows are reached.
      run_chain <- function(pred_df) {
         n            <- nrow(pred_df)
         preds        <- rep(NA_real_, n)
         pred_history <- rep(NA_real_, n)
         
         # Determine which rows are within the active simulation window
         active <- if (!is.null(sim_start)) {
            as.Date(pred_df$DateTime) >= sim_start
         } else {
            rep(TRUE, n)
         }
         
         for (i in seq_len(n)) {
            row <- pred_df[i, , drop = FALSE]
            
            # Replace lagged salinity with model output once:
            #   (a) within active window, AND
            #   (b) a prediction exists k steps back
            if (active[i] && i > k && !is.na(pred_history[i - k]))
               row[[lag_sal_col]] <- pred_history[i - k]
            
            gam_pred_cols <- pred_cols_k[pred_cols_k %in% names(row)]
            if (any(is.na(row[, gam_pred_cols]))) { pred_history[i] <- NA_real_; next }
            
            pred            <- predict(gam_obj, newdata = row, type = "response")
            pred_history[i] <- pred
            preds[i]        <- pred
         }
         preds
      }
      
      # ---- Run all scenarios -----------------------------------------------
      results <- raw_df %>% dplyr::select(DateTime, Salinity)
      for (sc in scenarios) {
         q_mod            <- sc$modifier(raw_q, raw_df$DateTime)
         pred_df          <- build_pred_df(q_mod)
         results[[sc$label]] <- run_chain(pred_df)
      }
      
      # ---- Sensitivity summary (event window max) --------------------------
      event_rows <- in_event(results$DateTime, event_start, event_end)
      obs_max    <- max(results$Observed[event_rows], na.rm = TRUE)
      
      for (sc in scenarios[-1]) {
         sc_max <- max(results[[sc$label]][event_rows], na.rm = TRUE)
         summary_rows[[length(summary_rows) + 1]] <- data.frame(
            LeadTime  = k,
            Scenario  = sc$label,
            Group     = sc$group,
            Obs_Max   = obs_max,
            Scen_Max  = sc_max,
            Reduction = obs_max - sc_max,
            Experiment = experiment_label
         )
      }
      
      # ---- Individual trajectory plot -------------------------------------
      plot_labels  <- c("Simulated (obs. Q)", all_labels[-1])
      results_plot <- results %>% rename(`Simulated (obs. Q)` = Observed)
      
      observed_long <- raw_df %>%
         dplyr::select(DateTime, Salinity) %>%
         filter(as.Date(DateTime) >= PLOT_START, as.Date(DateTime) <= PLOT_END)
      
      results_long <- results_plot %>%
         pivot_longer(-DateTime, names_to = "Scenario", values_to = "Salinity") %>%
         mutate(Scenario = factor(Scenario, levels = plot_labels)) %>%
         filter(as.Date(DateTime) >= PLOT_START, as.Date(DateTime) <= PLOT_END)
      
      subtitle_str <- if (!is.null(sim_start)) {
         sprintf("Chain initialized %s  |  Pre-release from %s",
                 format(sim_start, "%b %d"), format(prerelease_start, "%b %d"))
      } else {
         sprintf("Full-season chain  |  Pre-release from %s",
                 format(prerelease_start, "%b %d"))
      }
      
      p <- ggplot() +
         geom_vline(xintercept = as.POSIXct(event_start),
                    linetype = "dotted", color = "#888888", linewidth = 0.5) +
         { if (!is.null(sim_start))
            geom_vline(xintercept = as.POSIXct(sim_start),
                       linetype = "dashed", color = "#cc6600", linewidth = 0.6)
         } +
         geom_line(data = results_long,
                   aes(x = DateTime, y = Salinity,
                       color = Scenario, linewidth = Scenario, linetype = Scenario)) +
         geom_line(data = observed_long, aes(x = DateTime, y = Salinity),
                   color = "#f58220", linewidth = 1.1, alpha = 0.9) +
         geom_hline(yintercept = 0.5, color = "#002030",
                    linetype = "dashed", linewidth = 0.5) +
         annotate("text", x = as.POSIXct(PLOT_START), y = 0.52,
                  label = "EPA Secondary Standard (0.5 psu)",
                  hjust = 0, vjust = 0, size = 3.2, colour = "#002030") +
         scale_color_manual(values = scenario_colors, breaks = plot_labels) +
         scale_linewidth_manual(values = scenario_lwd, breaks = plot_labels, guide = "none") +
         scale_linetype_manual(values = scenario_lty, breaks = plot_labels, guide = "none") +
         guides(color = guide_legend(
            override.aes = list(linewidth = unname(scenario_lwd[plot_labels]),
                                linetype  = unname(scenario_lty[plot_labels])), ncol = 1)) +
         scale_x_datetime(date_labels = "%b %d", date_breaks = "2 weeks",
                          limits = as.POSIXct(c(PLOT_START, PLOT_END))) +
         scale_y_continuous(name = "Salinity (psu)") +
         labs(x = "Date (2016)", color = "Scenario",
              title    = sprintf("%s Lag%d, October 2016", experiment_label, k),
              subtitle = subtitle_str) +
         theme_bw() +
         theme(plot.title    = element_text(size = 13, face = "bold", color = "#002030"),
               plot.subtitle = element_text(size = 8.5, color = "#555555"),
               axis.title    = element_text(size = 12, face = "bold", color = "#002030"),
               axis.text.x   = element_text(size = 9, angle = 15, hjust = 1),
               axis.text.y   = element_text(size = 10),
               panel.border  = element_rect(colour = "#002030", fill = NA, linewidth = 1),
               legend.position  = "right",
               legend.title     = element_text(size = 10, face = "bold"),
               legend.text      = element_text(size = 9),
               legend.key.width = unit(1.5, "cm"))
      
      fname <- sprintf("%s_Lag%02d_Oct2016.png",
                       gsub(" ", "_", experiment_label), k)
      ggsave(file.path(out_dir, fname), p, width = 13, height = 6, dpi = 600)
   }
   
   # ---- Summary sensitivity plot ------------------------------------------
   summary_df <- bind_rows(summary_rows) %>%
      mutate(
         Scenario = factor(Scenario, levels = all_labels[-1]),
         Group    = factor(Group, levels = c("Pre-release", "Pulse", "Combined"))
      )
   
   p_summary <- ggplot(summary_df,
                       aes(x = LeadTime, y = Reduction,
                           color = Group, group = Scenario, linetype = Scenario)) +
      geom_hline(yintercept = 0, color = "#aaaaaa", linewidth = 0.4) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 2) +
      scale_color_manual(values = group_colors) +
      scale_x_continuous(breaks = lead_times, name = "Forecast Lead Time (days)") +
      scale_y_continuous(name = "Max Salinity Reduction (psu)\n(Observed baseline \u2212 Scenario)") +
      labs(
         title    = sprintf("%s — Discharge Scenario Sensitivity", experiment_label),
         # subtitle = "October 2016 event  |  Reduction = observed max \u2212 scenario max",
         color    = "Scenario group", linetype = "Scenario"
      ) +
      theme_bw() +
      theme(
         plot.title    = element_text(size = 13, face = "bold", color = "#002030"),
         plot.subtitle = element_text(size = 9, color = "#555555"),
         axis.title    = element_text(size = 11, face = "bold", color = "#002030"),
         axis.text     = element_text(size = 10),
         panel.border  = element_rect(colour = "#002030", fill = NA, linewidth = 1),
         legend.position  = "right",
         legend.title     = element_text(size = 10, face = "bold"),
         legend.text      = element_text(size = 9),
         legend.key.width = unit(1.2, "cm")
      )
   
   ggsave(file.path(out_dir, sprintf("%s_Sensitivity_Summary.png",
                                     gsub(" ", "_", experiment_label))),
          p_summary, width = 10, height = 6, dpi = 600)
   cat(sprintf("  Saved summary: %s\n", experiment_label))
   
   invisible(summary_df)
}

# =============================================================================
# RUN BOTH EXPERIMENTS
# =============================================================================

summary_exp1 <- run_experiment(
   experiment_label  = "Counterfactual",
   sim_start         = NULL,
   prerelease_start  = EXP1_PRERELEASE_START,
   prerelease_end    = EXP1_PRERELEASE_END,
   event_start       = EVENT_START,
   event_end         = EVENT_END,
   out_dir           = OUT_EXP1
)

summary_exp2 <- run_experiment(
   experiment_label  = "ForecastOperations",
   sim_start         = EXP2_SIM_START,
   prerelease_start  = EXP2_PRERELEASE_START,
   prerelease_end    = EXP2_PRERELEASE_END,
   event_start       = EVENT_START,
   event_end         = EVENT_END,
   out_dir           = OUT_EXP2
)

# =============================================================================
# CROSS-EXPERIMENT COMPARISON PANEL
# One facet per scenario group, both experiments overlaid
# =============================================================================

comparison_df <- bind_rows(summary_exp1, summary_exp2) %>%
   mutate(
      Experiment = factor(Experiment,
                          levels = c("Counterfactual", "ForecastOperations")),
      Group      = factor(Group, levels = c("Pre-release", "Pulse", "Combined"))
   )

exp_colors <- c("Counterfactual"       = "#002030",
                "ForecastOperations"  = "#cc6600")

p_comparison <- ggplot(comparison_df,
                       aes(x = LeadTime, y = Reduction,
                           color = Experiment, group = interaction(Experiment, Scenario),
                           linetype = Scenario)) +
   facet_wrap(~ Group, ncol = 1, scales = "free_y") +
   geom_hline(yintercept = 0, color = "#aaaaaa", linewidth = 0.4) +
   geom_line(linewidth = 0.7) +
   geom_point(size = 1.8) +
   scale_color_manual(values = exp_colors) +
   scale_x_continuous(breaks = seq(1, 30, 2), name = "Forecast Lead Time (days)") +
   scale_y_continuous(name = "Max Salinity Reduction (psu)") +
   labs(
      title    = "Cross-Experiment Sensitivity Comparison",
      #subtitle = "October 2016  |  Solid = Counterfactual  |  Dashed = Forecast-Operations",
      color    = "Experiment", linetype = "Scenario"
   ) +
   theme_bw() +
   theme(
      plot.title    = element_text(size = 13, face = "bold", color = "#002030"),
      plot.subtitle = element_text(size = 9,  color = "#555555"),
      axis.title    = element_text(size = 11, face = "bold", color = "#002030"),
      axis.text     = element_text(size = 10),
      strip.text    = element_text(size = 11, face = "bold", color = "#002030"),
      strip.background = element_rect(fill = "#e8eef2", color = "#002030"),
      panel.border  = element_rect(colour = "#002030", fill = NA, linewidth = 1),
      legend.position  = "right",
      legend.title     = element_text(size = 10, face = "bold"),
      legend.text      = element_text(size = 9),
      legend.key.width = unit(1.2, "cm")
   )

ggsave(file.path(OUT_COMP, "CrossExperiment_Comparison.png"),
       p_comparison, width = 11, height = 12, dpi = 600)
cat("\nSaved: CrossExperiment_Comparison.png\n")

rm(list = ls())