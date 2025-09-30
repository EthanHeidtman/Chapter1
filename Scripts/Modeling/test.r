create_raw_daily_curves <- function(final_data, risk_tolerance = 0.10) {
   
   cat("Method 1: Raw daily minimums (no smoothing)\n")
   
   daily_minimums <- final_data %>%
      filter(
         !is.na(exceedance_probability),
         !is.na(Discharge)
      ) %>%
      group_by(threshold, DayOfYear) %>%
      summarise(
         # point estimate
         min_safe_flow = if (sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         
         # 95% CI lower bound
         min_safe_flow_lower = if (sum(exceedance_upper <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_upper <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         
         # 95% CI upper bound
         min_safe_flow_upper = if (sum(exceedance_lower <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_lower <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         
         typical_safe_flow = if (sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) >= 3) {
            median(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
         n_total_obs = n(),
         .groups = "drop"
      ) %>%
      mutate(
         date  = as.Date(paste("2024", DayOfYear), format = "%Y %j"),
         month = month(date, label = TRUE),
         method = "Raw Daily"
      )
   
   return(daily_minimums)
}

create_rolling_average_curves <- function(final_data, risk_tolerance = 0.10, window_days = 7) {
   
   cat("Method 2: Rolling", window_days, "day average\n")
   
   # First get raw daily values
   raw_daily <- create_raw_daily_curves(final_data, risk_tolerance)
   
   # Apply rolling average
   rolling_curves <- raw_daily %>%
      group_by(threshold) %>%
      arrange(DayOfYear) %>%
      mutate(
         # Use centered rolling mean
         roll_min_safe = rollmean(min_safe_flow, k = window_days, fill = NA, align = "center"),
         roll_typical_safe = rollmean(typical_safe_flow, k = window_days, fill = NA, align = "center"),
         method = paste0("Rolling ", window_days, "d")
      ) %>%
      ungroup()
   
   return(rolling_curves)
}

create_monthly_step_curves <- function(final_data, risk_tolerance = 0.10) {
   
   cat("Method 3: Monthly step function with 95% CI\n")
   
   monthly_minimums <- final_data %>%
      filter(!is.na(exceedance_probability) & !is.na(Discharge)) %>%
      mutate(month = lubridate::month(Date, label = TRUE)) %>%
      group_by(threshold, month) %>%
      summarise(
         # central estimate
         min_safe_flow = if (sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) > 0)
            min(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         else NA_real_,
         
         # propagate CI
         min_safe_flow_lo = if (sum(exceedance_upper <= risk_tolerance, na.rm = TRUE) > 0)
            min(Discharge[exceedance_upper <= risk_tolerance], na.rm = TRUE)
         else NA_real_,
         
         min_safe_flow_hi = if (sum(exceedance_lower <= risk_tolerance, na.rm = TRUE) > 0)
            min(Discharge[exceedance_lower <= risk_tolerance], na.rm = TRUE)
         else NA_real_,
         
         typical_safe_flow = if (sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) >= 5)
            median(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         else NA_real_,
         
         n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
         .groups = "drop"
      )
   
   # expand to daily step function
   daily_step <- tidyr::expand_grid(
      threshold = unique(monthly_minimums$threshold),
      DayOfYear = 1:366
   ) %>%
      mutate(
         date  = as.Date(paste("2024", DayOfYear), format = "%Y %j"),
         month = lubridate::month(date, label = TRUE)
      ) %>%
      left_join(monthly_minimums,
                by = c("threshold", "month")) %>%
      mutate(method = "Monthly Steps")
   
   return(daily_step)
}



daily_curve <- create_raw_daily_curves(final_data, risk_tolerance = 0.10)
daily_curve <- daily_curve %>%
   left_join(., ferc, by = 'DayOfYear') %>%
   relocate(FERC, .after = month)
daily <- ggplot(daily_curve %>% filter(threshold %in% c(0.13, 0.14, 0.15, 0.16, 0.19, 0.28))) + 
   #geom_ribbon(aes(x = DayOfYear, ymin = min_safe_flow_lower, ymax = min_safe_flow_upper, fill = factor(threshold)), alpha = 0.3) + 
   geom_line(aes(x = DayOfYear, y = min_safe_flow, color = factor(threshold)), na.rm = TRUE) + 
   geom_line(aes(x = DayOfYear, y = FERC, color = 'FERC'), na.rm = TRUE, linewidth = 1.5) + 
   scale_y_continuous(trans = 'log10',
                      breaks = scales::log_breaks(base = 10),
                      # small ticks at every integer multiple of 10^n (no labels)
                      minor_breaks = {
                         rng  <- range(daily_curve$min_safe_flow, na.rm = TRUE)
                         pwr  <- floor(log10(rng[1])):ceiling(log10(rng[2]))
                         as.numeric(outer(1:9, 10^pwr))                  # 1–9 × each decade
                      }) + 
   scale_color_brewer(name = 'Threshold (psu)',
                      palette = 'Dark2') +
   # scale_fill_brewer(name = '95th % Confidence Interval',
   #                   palette = 'Dark2') +
   labs(x = 'Day of Year',
        y = 'Minimum Safe Flow (cubic m/s)',
        title = 'Minimum Safe Flows by Salinity Exceedance Threshold') + 
   theme_bw() + 
   theme(plot.title = element_text(size = 16, face = 'bold'),
         axis.title = element_text(size = 14, face = 'bold'),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14, face = 'bold'))
ggsave(paste0(PLOT_PATH, '/DailyStaticFR.png'), daily, width = 13, height = 8, dpi = 600)



monthly_curve <- create_monthly_step_curves(final_data, risk_tolerance = 0.1)
monthly_curve <- monthly_curve %>%
   left_join(., ferc, by = 'DayOfYear') %>%
   relocate(FERC, .after = month)
monthly <- ggplot(monthly_curve %>% filter(threshold %in% c(0.13, 0.14, 0.15, 0.16, 0.19, 0.28))) + 
   #geom_ribbon(aes(x = DayOfYear, ymin = min_safe_flow_lo, ymax = min_safe_flow_hi, fill = factor(threshold)), alpha = 0.3) + 
   geom_step(aes(x = DayOfYear, y = min_safe_flow, color = factor(threshold)), na.rm = TRUE) + 
   geom_line(aes(x = DayOfYear, y = FERC, color = 'FERC'), na.rm = TRUE, linewidth = 1.5) + 
   scale_y_continuous(trans = 'log10',
                      breaks = scales::log_breaks(base = 10),
                      # small ticks at every integer multiple of 10^n (no labels)
                      minor_breaks = {
                         rng  <- range(daily_curve$min_safe_flow, na.rm = TRUE)
                         pwr  <- floor(log10(rng[1])):ceiling(log10(rng[2]))
                         as.numeric(outer(1:9, 10^pwr))                  # 1–9 × each decade
                      }) + 
   scale_color_brewer(name = 'Threshold (psu)',
                      palette = 'Dark2') +
   # scale_fill_brewer(name = '95th % Confidence Interval',
   #                   palette = 'Dark2') +
   labs(x = 'Day of Year',
        y = 'Minimum Safe Flow (cubic m/s)',
        title = 'Minimum Safe Flows by Salinity Exceedance Threshold') + 
   lims(x = c(60, 360)) + 
   theme_bw() + 
   theme(plot.title = element_text(size = 16, face = 'bold'),
         axis.title = element_text(size = 14, face = 'bold'),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14, face = 'bold'))
ggsave(paste0(PLOT_PATH, '/MonthlyStaticFR.png'), monthly, width = 13, height = 8, dpi = 600)


daily_median <- ggplot(daily_curve %>% filter(threshold %in% c(0.13, 0.14, 0.15, 0.16, 0.19, 0.28))) + 
   #geom_ribbon(aes(x = DayOfYear, ymin = min_safe_flow_lower, ymax = min_safe_flow_upper, fill = factor(threshold)), alpha = 0.3) + 
   geom_line(aes(x = DayOfYear, y = typical_safe_flow, color = factor(threshold)), na.rm = TRUE) + 
   geom_line(aes(x = DayOfYear, y = FERC, color = 'FERC'), na.rm = TRUE, linewidth = 1.5) + 
   scale_y_continuous(trans = 'log10',
                      breaks = scales::log_breaks(base = 10),
                      # small ticks at every integer multiple of 10^n (no labels)
                      minor_breaks = {
                         rng  <- range(daily_curve$min_safe_flow, na.rm = TRUE)
                         pwr  <- floor(log10(rng[1])):ceiling(log10(rng[2]))
                         as.numeric(outer(1:9, 10^pwr))                  # 1–9 × each decade
                      }) + 
   scale_color_brewer(name = 'Threshold (psu)',
                      palette = 'Dark2') +
   # scale_fill_brewer(name = '95th % Confidence Interval',
   #                   palette = 'Dark2') +
   labs(x = 'Day of Year',
        y = 'Median Safe Flow (cubic m/s)',
        title = 'Median Safe Flows by Salinity Exceedance Threshold') + 
   theme_bw() + 
   theme(plot.title = element_text(size = 16, face = 'bold'),
         axis.title = element_text(size = 14, face = 'bold'),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14, face = 'bold'))
ggsave(paste0(PLOT_PATH, '/DailyMedianStaticFR.png'), daily_median, width = 13, height = 8, dpi = 600)

monthly_median <- ggplot(monthly_curve %>% filter(threshold %in% c(0.13, 0.14, 0.15, 0.16, 0.19, 0.28))) + 
   #geom_ribbon(aes(x = DayOfYear, ymin = min_safe_flow_lo, ymax = min_safe_flow_hi, fill = factor(threshold)), alpha = 0.3) + 
   geom_step(aes(x = DayOfYear, y = typical_safe_flow, color = factor(threshold)), na.rm = TRUE) + 
   geom_line(aes(x = DayOfYear, y = FERC, color = 'FERC'), na.rm = TRUE, linewidth = 1.5) + 
   scale_y_continuous(trans = 'log10',
                      breaks = scales::log_breaks(base = 10),
                      # small ticks at every integer multiple of 10^n (no labels)
                      minor_breaks = {
                         rng  <- range(daily_curve$min_safe_flow, na.rm = TRUE)
                         pwr  <- floor(log10(rng[1])):ceiling(log10(rng[2]))
                         as.numeric(outer(1:9, 10^pwr))                  # 1–9 × each decade
                      }) + 
   scale_color_brewer(name = 'Threshold (psu)',
                      palette = 'Dark2') +
   # scale_fill_brewer(name = '95th % Confidence Interval',
   #                   palette = 'Dark2') +
   labs(x = 'Day of Year',
        y = 'Median Safe Flow (cubic m/s)',
        title = 'Median Safe Flows by Salinity Exceedance Threshold') + 
   lims(x = c(60, 360)) + 
   theme_bw() + 
   theme(plot.title = element_text(size = 16, face = 'bold'),
         axis.title = element_text(size = 14, face = 'bold'),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14, face = 'bold'))
ggsave(paste0(PLOT_PATH, '/MonthlyMedianStaticFR.png'), monthly_median, width = 13, height = 8, dpi = 600)

create_raw_daily_curves <- function(final_data, risk_tolerance = 0.10) {
   
   daily_minimums <- final_data %>%
      filter(
         !is.na(exceedance_probability),
         !is.na(Discharge)
      ) %>%
      group_by(threshold, DayOfYear) %>%
      summarise(
         
         # 
         
         # point estimate
         min_safe_flow = if (sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         
         # 95% CI lower bound
         min_safe_flow_lower = if (sum(exceedance_upper <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_upper <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         
         # 95% CI upper bound
         min_safe_flow_upper = if (sum(exceedance_lower <= risk_tolerance, na.rm = TRUE) > 0) {
            min(Discharge[exceedance_lower <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         
         typical_safe_flow = if (sum(exceedance_probability <= risk_tolerance, na.rm = TRUE) >= 3) {
            median(Discharge[exceedance_probability <= risk_tolerance], na.rm = TRUE)
         } else {
            NA_real_
         },
         n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
         n_total_obs = n(),
         .groups = "drop"
      ) %>%
      mutate(
         date  = as.Date(paste("2024", DayOfYear), format = "%Y %j"),
         month = month(date, label = TRUE),
         method = "Raw Daily"
      )
   
   return(daily_minimums)
}
