# =============================================================================
# Script 06 — Sensitivity Analyses
# Unified GAM | October 2016 Event
#
# For each horizon h=1:20, perturb raw discharge and wind in scenario windows,
# rebuild all predictors exactly as Script 01, stack, and evaluate the
# model's predicted event peak. Reports peak salinity difference vs baseline.
# =============================================================================

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(mgcv)
library(zoo)
library(lubridate)
library(purrr)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/ComputePredictors.R')
source('Scripts/Utilities/SensitivityUtilities.R')

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

WIND_SHIFTS  <- seq(0.25, 2.0, by = 0.25)
SHIFT_LABELS <- sprintf("-%.2f m/s", WIND_SHIFTS)

OUT_DIR <- "Outputs/Plots/UnifiedGAM/SensitivitySimulations"
dir.create(file.path(OUT_DIR, "Discharge"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "Wind"),      recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "Combined"),  recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# COLORS
# =============================================================================
SUSTAINED_DISCHARGE_BASE <- "#4A90D9"
FLUSHING_DISCHARGE_BASE  <- "#2E8B57"
COMBINED_COLOR           <- "#002030"

pre_colors   <- colorRampPalette(c("#BFDCF2", SUSTAINED_DISCHARGE_BASE, "#1B4F72"))(3)
pulse_colors <- colorRampPalette(c("#A8D5BA", FLUSHING_DISCHARGE_BASE, "#145A32"))(2)

DISCHARGE_SCENARIO_COLORS <- c(
   "Pre \u00d71.5"               = pre_colors[1],
   "Pre \u00d72"                 = pre_colors[2],
   "Pre \u00d73"                 = pre_colors[3],
   "Pulse \u00d72"               = pulse_colors[1],
   "Pulse \u00d73"               = pulse_colors[2],
   "Pre \u00d72 + Pulse \u00d73" = COMBINED_COLOR
)

WIND_GRADIENT <- c(low = "#d4b8e0", high = "#8B4789")

# =============================================================================
# LOAD MODEL AND RAW DATA
# =============================================================================
gam_unified   <- read_qs_files("Outputs/Models/UnifiedGAM/GamUnified_Adjusted.qs2")
gam_obj       <- gam_unified$gam_object
gam_pred_vars <- names(gam_obj$model)
gam_pred_vars <- gam_pred_vars[gam_pred_vars != "Salinity_h"]

wind_var <- detect_wind_var(gam_pred_vars)
req_cols <- get_req_cols(gam_obj)
cat(sprintf("Detected wind predictor: %s\n", wind_var))
cat(sprintf("Detected required columns: %s\n", paste(req_cols, collapse = ", ")))

raw_data <- as.data.frame(read_qs_files("Data/Tidied/Final/Daily/DailyRawData.qs2")) %>%
   mutate(DateTime = as.Date(DateTime)) %>%
   arrange(DateTime)

# Threshold and Climatology calculations
FLUSH_THRESHOLD <- quantile(
   raw_data$MaxDischarge[month(raw_data$DateTime) %in% c(8, 9, 10, 11)],
   0.90, na.rm = TRUE
)

clim_discharge <- raw_data %>%
   mutate(DayOfYear = as.numeric(format(DateTime, "%j"))) %>%
   group_by(DayOfYear) %>%
   summarise(ClimDischarge = mean(Discharge, na.rm = TRUE), .groups = "drop") %>%
   mutate(ClimDischarge = zoo::rollmean(ClimDischarge, 15, fill = "extend", align = "center"))

# Wind mapper factory initialization
add_wind_dir_observed <- build_wind_direction_mapper(
   raw_data        = raw_data,
   gam_obj         = gam_obj,
   wind_var        = wind_var,
   clim_discharge  = clim_discharge,
   flush_threshold = FLUSH_THRESHOLD,
   estuary_axis_deg = ESTUARY_AXIS_DEG
)

# =============================================================================
# SCENARIO DEFINITIONS — Discharge
# =============================================================================
in_pre          <- function(dates) as.Date(dates) >= SIM_START   & as.Date(dates) <  EVENT_START
in_event_window <- function(dates) as.Date(dates) >= EVENT_START & as.Date(dates) <= EVENT_END

year_raw     <- raw_data %>% filter(Year == YEAR)
event_mean_q <- mean(year_raw$Discharge[in_event_window(year_raw$DateTime)], na.rm = TRUE)

discharge_scenarios <- list(
   list(label = "Pre \u00d71.5", Group = "Pre-event",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]    <- d$Discharge[in_pre(d$DateTime)] * 1.5
           d$MaxDischarge[in_pre(d$DateTime)] <- d$MaxDischarge[in_pre(d$DateTime)] * 1.5
           d }),
   list(label = "Pre \u00d72", Group = "Pre-event",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]    <- d$Discharge[in_pre(d$DateTime)] * 2.0
           d$MaxDischarge[in_pre(d$DateTime)] <- d$MaxDischarge[in_pre(d$DateTime)] * 2.0
           d }),
   list(label = "Pre \u00d73", Group = "Pre-event",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]    <- d$Discharge[in_pre(d$DateTime)] * 3.0
           d$MaxDischarge[in_pre(d$DateTime)] <- d$MaxDischarge[in_pre(d$DateTime)] * 3.0
           d }),
   list(label = "Pulse \u00d72", Group = "Pulse",
        modifier = function(d) {
           d$Discharge[in_event_window(d$DateTime)]    <- event_mean_q * 2
           d$MaxDischarge[in_event_window(d$DateTime)] <- event_mean_q * 2
           d }),
   list(label = "Pulse \u00d73", Group = "Pulse",
        modifier = function(d) {
           d$Discharge[in_event_window(d$DateTime)]    <- event_mean_q * 3
           d$MaxDischarge[in_event_window(d$DateTime)] <- event_mean_q * 3
           d }),
   list(label = "Pre \u00d72 + Pulse \u00d73", Group = "Combined",
        modifier = function(d) {
           d$Discharge[in_pre(d$DateTime)]             <- d$Discharge[in_pre(d$DateTime)] * 2.0
           d$MaxDischarge[in_pre(d$DateTime)]          <- d$MaxDischarge[in_pre(d$DateTime)] * 2.0
           d$Discharge[in_event_window(d$DateTime)]    <- event_mean_q * 3
           d$MaxDischarge[in_event_window(d$DateTime)] <- event_mean_q * 3
           d })
)

# =============================================================================
# SCENARIO DEFINITIONS — Wind
# =============================================================================
in_perturb <- function(dates) as.Date(dates) >= SIM_START & as.Date(dates) < EVENT_START

wind_scenarios <- lapply(seq_along(WIND_SHIFTS), function(i) {
   shift <- WIND_SHIFTS[i]
   list(
      label = SHIFT_LABELS[i],
      Shift = shift,
      modifier = function(d) {
         d$WSPD[in_perturb(d$DateTime)] <- pmax(0, d$WSPD[in_perturb(d$DateTime)] - shift)
         d
      }
   )
})

# =============================================================================
# RUN BOTH ANALYSES
# =============================================================================
cat("\n--- Discharge scenarios ---\n")
discharge_summary <- run_sensitivity_scenarios(
   raw_data         = raw_data,
   gam_obj          = gam_obj,
   scenarios        = discharge_scenarios,
   year             = YEAR,
   h_max            = H_MAX,
   horizons         = HORIZONS,
   event_start      = EVENT_START,
   event_end        = EVENT_END,
   add_wind_dir_fn  = add_wind_dir_observed,
   req_cols         = req_cols,
   clim_discharge   = clim_discharge,
   flush_threshold  = FLUSH_THRESHOLD,
   estuary_axis_deg = ESTUARY_AXIS_DEG,
   extra_col_name   = "Group"
) %>%
   mutate(Scenario = factor(Scenario, levels = sapply(discharge_scenarios, `[[`, "label")),
          Group    = factor(Group, levels = c("Pre-event", "Pulse", "Combined")))

cat("\n--- Wind scenarios ---\n")
wind_summary <- run_sensitivity_scenarios(
   raw_data         = raw_data,
   gam_obj          = gam_obj,
   scenarios        = wind_scenarios,
   year             = YEAR,
   h_max            = H_MAX,
   horizons         = HORIZONS,
   event_start      = EVENT_START,
   event_end        = EVENT_END,
   add_wind_dir_fn  = add_wind_dir_observed,
   req_cols         = req_cols,
   clim_discharge   = clim_discharge,
   flush_threshold  = FLUSH_THRESHOLD,
   estuary_axis_deg = ESTUARY_AXIS_DEG,
   extra_col_name   = "Shift"
)

# =============================================================================
# SHARED Y-AXIS
# =============================================================================
y_range   <- range(c(discharge_summary$Difference, wind_summary$Difference), na.rm = TRUE)
y_pad     <- diff(y_range) * 0.08
Y_LIMITS  <- c(y_range[1] - y_pad, y_range[2] + y_pad)

# =============================================================================
# INDIVIDUAL PLOTS
# =============================================================================
p_discharge <- ggplot(discharge_summary,
                      aes(x = Horizon, y = Difference, color = Scenario, group = Scenario)) +
   geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
   geom_line(linewidth = 0.8) +
   geom_point(size = 2.5) +
   scale_color_manual(values = DISCHARGE_SCENARIO_COLORS) +
   scale_x_continuous(breaks = HORIZONS, name = "Forecast Horizon (days)") +
   scale_y_continuous(name = "Salinity Peak Difference (ppt)", limits = Y_LIMITS) +
   labs(title = "Discharge Scenario Sensitivity \u2014 October 2016 Event", color = "Scenario") +
   theme_rf() +
   theme(legend.key.width = unit(1.2, "cm"), legend.position = 'bottom')

p_wind <- ggplot(wind_summary,
                 aes(x = Horizon, y = Difference, color = Shift, group = Scenario)) +
   geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
   geom_line(linewidth = 0.8) +
   geom_point(size = 2.5) +
   scale_color_gradient(low = WIND_GRADIENT["low"], high = WIND_GRADIENT["high"],
                        name = "Easterly Wind Reduction (m/s)") +
   scale_x_continuous(breaks = HORIZONS, name = "Forecast Horizon (days)") +
   scale_y_continuous(name = "Salinity Peak Difference (ppt)", limits = Y_LIMITS) +
   labs(title = "Wind Scenario Sensitivity \u2014 October 2016 Event") +
   theme_rf() +
   theme(legend.key.width = unit(1.0, "cm"), legend.position = 'bottom',
         legend.box.spacing = unit(0.2, 'cm'))

ggsave(file.path(OUT_DIR, "Discharge", "Discharge_Sensitivity_ByHorizon.png"), p_discharge, width = 10, height = 6, dpi = 600)
ggsave(file.path(OUT_DIR, "Discharge", "Discharge_Sensitivity_ByHorizon.svg"), p_discharge, width = 10, height = 6, dpi = 600)
ggsave(file.path(OUT_DIR, "Wind", "Wind_Sensitivity_ByHorizon.png"), p_wind, width = 10, height = 6, dpi = 600)
ggsave(file.path(OUT_DIR, "Wind", "Wind_Sensitivity_ByHorizon.svg"), p_wind, width = 10, height = 6, dpi = 600)

# =============================================================================
# COMBINED TWO-PANEL PLOT
# =============================================================================
p_discharge_combo <- p_discharge +
   labs(title = "A) Discharge Scenario Sensitivity") +
   scale_y_continuous(name = "Salinity Peak Difference (ppt)", limits = Y_LIMITS,
                      sec.axis = dup_axis(labels = NULL, name = NULL)) +
   theme(legend.key.width = unit(1.0, "cm"),
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 9))

p_wind_combo <- p_wind +
   labs(title = "B) Wind Scenario Sensitivity") +
   scale_y_continuous(name = NULL, limits = Y_LIMITS,
                      sec.axis = dup_axis(name = NULL)) +
   theme(axis.text.y.left  = element_blank(),
         axis.text.y.right = element_text(size = 10, color = "grey20"),
         legend.key.width = unit(0.9, "cm"),
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 9))

p_combined <- p_discharge_combo + p_wind_combo + plot_layout(ncol = 2)

ggsave(file.path(OUT_DIR, "Combined", "DischargeWind_Sensitivity_Combined.png"),
       p_combined, width = 16, height = 6.5, dpi = 600)
ggsave(file.path(OUT_DIR, "Combined", "DischargeWind_Sensitivity_Combined.svg"),
       p_combined, width = 16, height = 6.5, dpi = 600)

cat("\nScript complete: individual + combined sensitivity figures saved.\n")
rm(list = ls())