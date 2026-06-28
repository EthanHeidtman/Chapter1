# =============================================================================
# Script 06 — Discharge Scenario Sensitivity
# Unified GAM | October 2016 Event
#
# For each horizon h=1:20, perturb raw discharge in scenario windows,
# rebuild all predictors exactly as Script 01, stack, and evaluate the
# model's predicted event peak. Reports peak salinity reduction vs baseline.
# =============================================================================

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(mgcv)
library(zoo)
library(lubridate)
library(purrr)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/ComputePredictors.R')

# =============================================================================
# THEME
# =============================================================================

theme_rf <- function() {
   theme_bw() +
      theme(
         plot.title        = element_text(size = 14, face = "bold", color = "grey20"),
         axis.title        = element_text(size = 12, face = "bold", color = "grey20"),
         axis.text         = element_text(size = 10, color = "grey20"),
         panel.border      = element_rect(colour = "grey20", fill = NA, linewidth = 1),
         legend.title      = element_text(size = 11, face = "bold", color = "grey20"),
         legend.text       = element_text(size = 10, color = "grey20"),
         legend.background = element_rect(fill = "white", color = "grey20", linewidth = 0.5),
         legend.key        = element_rect(fill = "white", color = NA),
         strip.text        = element_text(size = 11, face = "bold", color = "grey20"),
         strip.background  = element_rect(fill = "grey92", color = "grey20")
      )
}

# =============================================================================
# USER INPUTS
# =============================================================================

YEAR        <- 2016
SIM_START   <- as.Date("2016-09-09")
EVENT_START <- as.Date("2016-10-09")
EVENT_END   <- as.Date("2016-10-24")
HORIZONS    <- 1:20
H_MAX       <- 20
ESTUARY_AXIS_DEG <- 0

OUT_DIR <- "Outputs/Plots/SensitivitySimulations/Discharge"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# COLORS
# Three scenario groups, each a sequential ramp within the project palette.
# Pre-event: blues (#4A90D9 family)
# Pulse:     teals (#009bba family)
# Combined:  purples (#8B4789 family)
# =============================================================================

SCENARIO_COLORS <- c(
   "Pre \u00d71.5"               = "#9ecae1",
   "Pre \u00d72"                 = "#4A90D9",
   "Pre \u00d73"                 = "#08519c",
   "Pulse \u00d72"               = "#99d8c9",
   "Pulse \u00d73"               = "#009bba",
   "Pre \u00d72 + Pulse \u00d73" = "#8B4789"
)

GROUP_COLORS <- c(
   "Pre-event" = "#4A90D9",
   "Pulse"     = "#009bba",
   "Combined"  = "#8B4789"
)

# =============================================================================
# LOAD MODEL AND RAW DATA
# =============================================================================

gam_unified   <- read_qs_files("Outputs/Models/UnifiedGAM/GamUnified.qs")
gam_obj       <- gam_unified$gam_object
gam_pred_vars <- names(gam_obj$model)
gam_pred_vars <- gam_pred_vars[gam_pred_vars != "Salinity_h"]

wind_var       <- gam_pred_vars[grepl("RollingWind", gam_pred_vars) & !grepl("Dir", gam_pred_vars)][1]
wind_component <- if (grepl("Along", wind_var)) "Along" else "Cross"

raw_data <- as.data.frame(read_qs_files("Data/Tidied/Final/Daily/DailyRawData.qs")) %>%
   mutate(DateTime = as.Date(DateTime)) %>%
   arrange(DateTime)

# =============================================================================
# PREDICTOR PIPELINE — identical to Script 01
# =============================================================================

FLUSH_THRESHOLD <- quantile(
   raw_data$MaxDischarge[month(raw_data$DateTime) %in% c(8, 9, 10, 11)],
   0.90, na.rm = TRUE
)

clim_discharge <- raw_data %>%
   mutate(DayOfYear = as.numeric(format(DateTime, "%j"))) %>%
   group_by(DayOfYear) %>%
   summarise(ClimDischarge = mean(Discharge, na.rm = TRUE), .groups = "drop") %>%
   mutate(ClimDischarge = zoo::rollmean(ClimDischarge, 15, fill = "extend", align = "center"))


# =============================================================================
# STACKING — identical to Script 01
# =============================================================================

stack_horizons <- function(daily_data, h_max = H_MAX) {
   salinity_lookup <- daily_data %>%
      dplyr::select(DateTime, Salinity) %>%
      rename(target_date = DateTime, Salinity_h = Salinity)
   predictor_data <- daily_data %>% dplyr::select(-Salinity)
   purrr::map_dfr(1:h_max, function(h) {
      predictor_data %>%
         mutate(h = h, target_date = DateTime + h) %>%
         left_join(salinity_lookup, by = "target_date") %>%
         dplyr::select(-target_date)
   }) %>%
      filter(!is.na(Salinity_h)) %>%
      arrange(DateTime, h) %>%
      relocate(h, Salinity_h, .after = DateTime)
}

# =============================================================================
# ADD WindDir — matches FitGam.R exactly, always from observed wind sign
# =============================================================================

obs_model_data    <- build_model_data(raw_data)
obs_winddir_daily <- obs_model_data %>% dplyr::select(DateTime, !!wind_var)

add_wind_dir_observed <- function(stacked) {
   stacked %>%
      left_join(
         obs_winddir_daily %>% rename(obs_wind_join = !!wind_var),
         by = "DateTime"
      ) %>%
      mutate(WindDir = factor(
         ifelse(obs_wind_join >= 0,
                if (grepl("Along", wind_var)) "UpEstuary" else "RightBank",
                if (grepl("Along", wind_var)) "DownEstuary" else "LeftBank"),
         levels = levels(gam_obj$model$WindDir)
      )) %>%
      dplyr::select(-obs_wind_join)
}

# =============================================================================
# SCENARIO DEFINITIONS
# Perturbation window: SIM_START through EVENT_START (pre-event)
# Pulse window:        EVENT_START through EVENT_END (during event)
# Both Discharge and MaxDischarge are perturbed together
# =============================================================================

in_pre  <- function(dates) as.Date(dates) >= SIM_START   & as.Date(dates) <  EVENT_START
in_event_window <- function(dates) as.Date(dates) >= EVENT_START & as.Date(dates) <= EVENT_END

year_raw     <- raw_data %>% filter(Year == YEAR)
event_mean_q <- mean(year_raw$Discharge[in_event_window(year_raw$DateTime)], na.rm = TRUE)

scenarios <- list(
   list(label = "Pre \u00d71.5",               group = "Pre-event",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]    <- d$Discharge[in_pre(d$DateTime)] * 1.5
           d$MaxDischarge[in_pre(d$DateTime)] <- d$MaxDischarge[in_pre(d$DateTime)] * 1.5
           d }),
   list(label = "Pre \u00d72",                 group = "Pre-event",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]    <- d$Discharge[in_pre(d$DateTime)] * 2.0
           d$MaxDischarge[in_pre(d$DateTime)] <- d$MaxDischarge[in_pre(d$DateTime)] * 2.0
           d }),
   list(label = "Pre \u00d73",                 group = "Pre-event",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]    <- d$Discharge[in_pre(d$DateTime)] * 3.0
           d$MaxDischarge[in_pre(d$DateTime)] <- d$MaxDischarge[in_pre(d$DateTime)] * 3.0
           d }),
   list(label = "Pulse \u00d72",               group = "Pulse",
        modifier = function(d) {
           d$Discharge[in_event_window(d$DateTime)]    <- event_mean_q * 2
           d$MaxDischarge[in_event_window(d$DateTime)] <- event_mean_q * 2
           d }),
   list(label = "Pulse \u00d73",               group = "Pulse",
        modifier = function(d) {
           d$Discharge[in_event_window(d$DateTime)]    <- event_mean_q * 3
           d$MaxDischarge[in_event_window(d$DateTime)] <- event_mean_q * 3
           d }),
   list(label = "Pre \u00d72 + Pulse \u00d73", group = "Combined",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]             <- d$Discharge[in_pre(d$DateTime)] * 2.0
           d$MaxDischarge[in_pre(d$DateTime)]          <- d$MaxDischarge[in_pre(d$DateTime)] * 2.0
           d$Discharge[in_event_window(d$DateTime)]    <- event_mean_q * 3
           d$MaxDischarge[in_event_window(d$DateTime)] <- event_mean_q * 3
           d })
)

all_labels <- sapply(scenarios, `[[`, "label")
req_cols   <- c("h", "LagSalinity", "RollingDischarge50", "MaxDischarge10",
                "TideRange60", wind_var, "WindDir")

# =============================================================================
# BUILD STACKED DATA PER SCENARIO
# Run on full raw_data (all years) so rolling windows at start of 2016
# have correct history, then filter to YEAR after stacking
# =============================================================================

cat("Building stacked scenario datasets...\n")

# Baseline first
baseline_daily  <- build_model_data(raw_data)
baseline_stack  <- stack_horizons(baseline_daily) %>%
   add_wind_dir_observed() %>%
   filter(Year == YEAR)

build_scenario_stack <- function(sc) {
   cat(sprintf("  %s\n", sc$label))
   perturbed <- sc$modifier(raw_data)
   build_model_data(perturbed) %>%
      stack_horizons() %>%
      add_wind_dir_observed() %>%
      filter(Year == YEAR)
}

scenario_stacks        <- lapply(scenarios, build_scenario_stack)
names(scenario_stacks) <- all_labels

# =============================================================================
# DIRECT FORECAST SENSITIVITY ACROSS HORIZONS
# For each h, find predicted event peak under baseline and each scenario.
# LagSalinity is always observed truth — no simulation, no recursion.
# =============================================================================

cat("\nEvaluating forecast sensitivity h=1:20...\n")

predict_event_peak <- function(stack, h_val) {
   h_df    <- stack %>% filter(h == h_val)
   na_rows <- rowSums(is.na(h_df[, req_cols])) > 0
   preds   <- rep(NA_real_, nrow(h_df))
   if (sum(!na_rows) > 0) {
      preds[!na_rows] <- pmax(0, as.numeric(
         predict(gam_obj, newdata = h_df[!na_rows, ], type = "response")
      ))
   }
   max(preds[in_event_window(h_df$DateTime)], na.rm = TRUE)
}

summary_rows <- list()

for (h in HORIZONS) {
   cat(sprintf("  h = %d\n", h))
   obs_max <- predict_event_peak(baseline_stack, h)
   
   for (sc in scenarios) {
      sc_max <- predict_event_peak(scenario_stacks[[sc$label]], h)
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
         Horizon   = h,
         Scenario  = sc$label,
         Group     = sc$group,
         Obs_Max   = obs_max,
         Scen_Max  = sc_max,
         Reduction = obs_max - sc_max
      )
   }
}

summary_df <- bind_rows(summary_rows) %>%
   mutate(
      Scenario = factor(Scenario, levels = all_labels),
      Group    = factor(Group, levels = c("Pre-event", "Pulse", "Combined"))
   )

# =============================================================================
# SENSITIVITY PLOT
# =============================================================================

p_sensitivity <- ggplot(summary_df,
                        aes(x = Horizon, y = Reduction,
                            color = Scenario, group = Scenario)) +
   geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
   geom_line(linewidth = 0.8) +
   geom_point(size = 2.5) +
   scale_color_manual(values = SCENARIO_COLORS) +
   scale_x_continuous(breaks = HORIZONS, name = "Forecast Horizon h (days)") +
   scale_y_continuous(name = "Peak Salinity Reduction (psu)") +
   labs(title = "Discharge Scenario Sensitivity — October 2016 Event",
        color = "Scenario") +
   theme_rf() +
   theme(legend.key.width = unit(1.2, "cm"))

ggsave(file.path(OUT_DIR, "Discharge_Sensitivity_ByHorizon.png"),
       p_sensitivity, width = 10, height = 6, dpi = 600)
cat("Saved: Discharge_Sensitivity_ByHorizon.png\n")

cat("\nScript 06 complete.\n")
rm(list = ls())