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
# STACKING — identical structure to Script 01
# =============================================================================
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

# =============================================================================
# AUTO-DETECTION FROM THE FITTED MODEL
# No predictor names are ever hardcoded in a calling script — everything
# needed is pulled from gam_obj$model, so this stays correct across any
# future candidate model or transferred system.
# =============================================================================
detect_wind_var <- function(gam_pred_vars) {
   wv <- gam_pred_vars[grepl("RollingWind", gam_pred_vars) & !grepl("Dir", gam_pred_vars)]
   if (length(wv) == 0) stop("No wind predictor found in gam_obj$model.")
   wv[1]
}

get_req_cols <- function(gam_obj) {
   form_vars <- all.vars(formula(gam_obj))
   resp_var  <- form_vars[1]          # first term in the formula is the response
   setdiff(form_vars, resp_var)
}

# =============================================================================
# WindDir — always derived from OBSERVED wind sign, never from perturbed
# wind, so scenarios stay inside the training distribution for direction.
# =============================================================================
make_wind_dir_adder <- function(raw_data, gam_obj, wind_var) {
   obs_model_data    <- build_model_data(raw_data)
   obs_winddir_daily <- obs_model_data %>% dplyr::select(DateTime, !!wind_var)
   is_along <- grepl("Along", wind_var)
   
   function(stacked) {
      stacked %>%
         left_join(
            obs_winddir_daily %>% rename(obs_wind_join = !!wind_var),
            by = "DateTime"
         ) %>%
         mutate(WindDir = factor(
            ifelse(obs_wind_join >= 0,
                   if (is_along) "UpEstuary" else "RightBank",
                   if (is_along) "DownEstuary" else "LeftBank"),
            levels = levels(gam_obj$model$WindDir)
         )) %>%
         dplyr::select(-obs_wind_join)
   }
}

# =============================================================================
# GENERIC SCENARIO RUNNER
# scenarios: list of list(label=, group_or_shift_field=, modifier=function(d))
# extra_cols: named list of extra columns to attach to each summary row
#             (e.g. Group for discharge, Shift for wind)
# =============================================================================
run_sensitivity_scenarios <- function(raw_data, gam_obj, scenarios,
                                      year, h_max, horizons,
                                      event_start, event_end,
                                      add_wind_dir_fn, req_cols,
                                      extra_col_name = NULL) {
   
   in_event_window <- function(dates) as.Date(dates) >= event_start & as.Date(dates) <= event_end
   
   build_stack <- function(daily_raw) {
      build_model_data(daily_raw) %>%
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