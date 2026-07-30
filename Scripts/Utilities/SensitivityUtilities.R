library(dplyr)
library(purrr)
library(zoo)
library(lubridate)
library(ggplot2)

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
# STACKING HELPER
# Safe fallback if not already sourced from ComputePredictors.R
# =============================================================================
if (!exists("stack_horizons")) {
   stack_horizons <- function(daily_data, h_max) {
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
}

# =============================================================================
# AUTO-DETECTION FROM THE FITTED MODEL
# =============================================================================
detect_wind_var <- function(gam_pred_vars) {
   wv <- gam_pred_vars[grepl("RollingWind", gam_pred_vars) & !grepl("Dir", gam_pred_vars)]
   if (length(wv) == 0) stop("No wind predictor found in gam_obj$model.")
   wv[1]
}

get_req_cols <- function(gam_obj) {
   form_vars <- all.vars(formula(gam_obj))
   resp_var  <- form_vars[1]          # first term in formula is response
   setdiff(form_vars, resp_var)
}

# =============================================================================
# WIND DIRECTION MAPPER FACTORY
# Builds a closure function mapping WindDir based on OBSERVED wind sign so that
# sensitivity scenarios stay inside training distribution for direction.
# =============================================================================
build_wind_direction_mapper <- function(raw_data, gam_obj, wind_var,
                                        clim_discharge, flush_threshold,
                                        estuary_axis_deg = 0) {
   
   obs_model_data    <- build_model_data(raw_data, clim_discharge, flush_threshold, estuary_axis_deg)
   obs_winddir_daily <- obs_model_data %>% dplyr::select(DateTime, !!sym(wind_var))
   
   is_along   <- grepl("Along", wind_var)
   levels_vec <- if (is_along) c("DownEstuary", "UpEstuary") else c("LeftBank", "RightBank")
   pos_label  <- if (is_along) "UpEstuary" else "RightBank"
   neg_label  <- if (is_along) "DownEstuary" else "LeftBank"
   
   function(stacked) {
      stacked %>%
         left_join(
            obs_winddir_daily %>% rename(obs_wind_join = !!sym(wind_var)),
            by = "DateTime"
         ) %>%
         mutate(WindDir = factor(
            if_else(obs_wind_join >= 0, pos_label, neg_label),
            levels = levels_vec
         )) %>%
         dplyr::select(-obs_wind_join)
   }
}

# =============================================================================
# GENERIC SCENARIO RUNNER
# =============================================================================
run_sensitivity_scenarios <- function(raw_data, gam_obj, scenarios,
                                      year, h_max, horizons,
                                      event_start, event_end,
                                      add_wind_dir_fn, req_cols,
                                      clim_discharge, flush_threshold,
                                      estuary_axis_deg = 0,
                                      extra_col_name = NULL) {
   
   in_event_window <- function(dates) as.Date(dates) >= event_start & as.Date(dates) <= event_end
   
   build_stack <- function(daily_raw) {
      build_model_data(daily_raw, clim_discharge, flush_threshold, estuary_axis_deg) %>%
         stack_horizons(h_max) %>%
         add_wind_dir_fn() %>%
         filter(Year == year)
   }
   
   baseline_stack <- build_stack(raw_data)
   
   scenario_stacks <- lapply(scenarios, function(sc) {
      cat(sprintf("  %s\n", sc$label))
      build_stack(sc$modifier(raw_data))
   })
   names(scenario_stacks) <- sapply(scenarios, `[[`, "label")
   
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
   for (h in horizons) {
      cat(sprintf("  h = %d\n", h))
      obs_max <- predict_event_peak(baseline_stack, h)
      for (sc in scenarios) {
         sc_max <- predict_event_peak(scenario_stacks[[sc$label]], h)
         row <- data.frame(
            Horizon    = h,
            Scenario   = sc$label,
            Obs_Max    = obs_max,
            Scen_Max   = sc_max,
            Difference = sc_max - obs_max
         )
         if (!is.null(extra_col_name)) row[[extra_col_name]] <- sc[[extra_col_name]]
         summary_rows[[length(summary_rows) + 1]] <- row
      }
   }
   bind_rows(summary_rows)
}