# =============================================================================
# Script 07 — Wind Scenario Sensitivity
# Unified GAM | October 2016 Event
#
# For each horizon h=1:20, reduce WSPD in the pre-event window, rebuild
# all predictors exactly as Script 01, stack, and evaluate the model's
# predicted event peak. WindDir always derived from observed wind sign.
# Reports peak salinity reduction vs baseline across horizons.
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

# WSPD is reduced by these amounts in the perturbation window.
# Negative shift weakens intrusion-favorable (easterly) wind.
WIND_SHIFTS  <- seq(0.25, 2.0, by = 0.25)
SHIFT_LABELS <- sprintf("\u22120.%s m/s", formatC(WIND_SHIFTS * 100, format = "d"))
SHIFT_LABELS <- sprintf("-%.2f m/s", WIND_SHIFTS)

OUT_DIR <- "Outputs/Plots/SensitivitySimulations/Wind"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# COLORS — purple sequential ramp matching project palette
# =============================================================================

shift_colors <- colorRampPalette(c("#d4b8e0", "#8B4789"))(length(WIND_SHIFTS))
SCENARIO_COLORS <- setNames(shift_colors, SHIFT_LABELS)

# =============================================================================
# LOAD MODEL AND RAW DATA
# =============================================================================

gam_unified   <- read_qs_files("Outputs/Models/UnifiedGAM/GamUnified.qs")
gam_obj       <- gam_unified$gam_object
gam_pred_vars <- names(gam_obj$model)
gam_pred_vars <- gam_pred_vars[gam_pred_vars != "Salinity_h"]

wind_var       <- gam_pred_vars[grepl("RollingWind", gam_pred_vars) & !grepl("Dir", gam_pred_vars)][1]
wind_component <- if (grepl("Along", wind_var)) "Along" else "Cross"
cat(sprintf("Wind predictor: %s (%s component)\n", wind_var, wind_component))

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
# ADD WindDir — always from OBSERVED wind sign, never from perturbed wind.
# Perturbation changes WSPD magnitude only; WDIR (direction) is unchanged.
# WindDir must reflect observed direction to stay in the training distribution.
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
# WSPD is reduced by each shift amount in the perturbation window.
# Reducing WSPD while preserving WDIR weakens the intrusion-favorable
# (easterly) wind component without changing its direction.
# Floor at zero prevents unphysical negative wind speeds.
# =============================================================================

in_perturb <- function(dates) as.Date(dates) >= SIM_START & as.Date(dates) < EVENT_START

scenarios <- lapply(seq_along(WIND_SHIFTS), function(i) {
   shift <- WIND_SHIFTS[i]
   list(
      label    = SHIFT_LABELS[i],
      shift    = shift,
      modifier = function(d) {
         d$WSPD[in_perturb(d$DateTime)] <-
            pmax(0, d$WSPD[in_perturb(d$DateTime)] - shift)
         d
      }
   )
})

all_labels <- sapply(scenarios, `[[`, "label")
req_cols   <- c("h", "LagSalinity", "RollingDischarge50", "MaxDischarge10",
                "TideRange60", wind_var, "WindDir")

# =============================================================================
# BUILD STACKED DATA PER SCENARIO
# =============================================================================

cat("Building stacked scenario datasets...\n")

baseline_daily <- build_model_data(raw_data)
baseline_stack <- stack_horizons(baseline_daily) %>%
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
# =============================================================================

cat("\nEvaluating forecast sensitivity h=1:20...\n")

in_event_window <- function(dates) as.Date(dates) >= EVENT_START & as.Date(dates) <= EVENT_END

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
         Shift     = sc$shift,
         Obs_Max   = obs_max,
         Scen_Max  = sc_max,
         Reduction = obs_max - sc_max
      )
   }
}

summary_df <- bind_rows(summary_rows)

# =============================================================================
# SENSITIVITY PLOT
# =============================================================================

p_sensitivity <- ggplot(summary_df,
                        aes(x = Horizon, y = Reduction,
                            color = Shift, group = Scenario)) +
   geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
   geom_line(linewidth = 0.8) +
   geom_point(size = 2.5) +
   scale_color_gradient(
      low  = "#d4b8e0",
      high = "#8B4789",
      name = sprintf("Wind speed\nreduction\n(%s, m/s)", wind_component)
   ) +
   scale_x_continuous(breaks = HORIZONS, name = "Forecast Horizon h (days)") +
   scale_y_continuous(name = "Peak Salinity Reduction (psu)") +
   labs(title = "Wind Scenario Sensitivity — October 2016 Event") +
   theme_rf() +
   theme(legend.key.width = unit(1.2, "cm"))

ggsave(file.path(OUT_DIR, "Wind_Sensitivity_ByHorizon.png"),
       p_sensitivity, width = 10, height = 6, dpi = 600)
cat("Saved: Wind_Sensitivity_ByHorizon.png\n")

cat("\nScript 07 complete.\n")
rm(list = ls())