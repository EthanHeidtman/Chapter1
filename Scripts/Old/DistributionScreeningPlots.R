salinity_threshold = 0.5
thresholds <- c(0.3, 0.5, 0.7)
dist_data <- dist_data %>%
   mutate(
      actual_exceedance = Salinity > salinity_threshold
   )

# Overall Metrics and Summary plot
avg_prob_summary <- dist_data %>%
   group_by(distribution_family) %>%
   summarise(avg_prob_exc = mean(exceedance_probability[actual_exceedance], na.rm = TRUE),
             .groups = "drop")

# hit_rate (for multiple thresholds)
hit_rate_summary <- map_dfr(thresholds, function(thresh) {
   dist_data %>%
      group_by(distribution_family) %>%
      summarise(hit_rate = mean(exceedance_probability[actual_exceedance] > thresh, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(threshold = thresh)
})

# ---- 3. Reshape for plotting ----
avg_prob_long <- avg_prob_summary %>%
   pivot_longer(cols = avg_prob_exc, names_to = "metric", values_to = "value")

hit_rate_long <- hit_rate_summary %>%
   mutate(metric = paste0("hit_rate_", threshold)) %>%
   select(distribution_family, metric, value = hit_rate)

overall_long <- bind_rows(avg_prob_long, hit_rate_long)

# ---- 4. Plot ----
p_overall_multi <- ggplot(overall_long,
                          aes(x = reorder(distribution_family, value),
                              y = value, fill = metric)) +
   geom_col(position = position_dodge(width = 0.8), width = 0.7) +
   geom_text(aes(label = percent(value, accuracy = 0.1)),
             position = position_dodge(width = 0.8),
             hjust = -0.1, size = 3, fontface = "bold") +
   scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
   coord_flip() +
   scale_fill_manual(
      name = 'Metric',
      values = c("avg_prob_exc" = "steelblue",
                 "hit_rate_0.3" = "darkorange",
                 "hit_rate_0.5" = "tomato",
                 "hit_rate_0.7" = "firebrick"),
      labels = c("Avg prob (during exceedances)",
                 "Hit rate (prob > 0.3)",
                 "Hit rate (prob > 0.5)",
                 "Hit rate (prob > 0.7)")
   ) +
   labs(title = "Overall Distribution Performance",
        subtitle = "Blue = average predicted probability during exceedances | Orange/Red = hit rate at thresholds",
        x = "Distribution", y = "Performance") +
   theme_bw(base_size = 12) +
   theme(legend.position = "bottom")

p_overall_multi
ggsave(paste0(PLOT_PATH, '/OverallDistributionMetrics.png'), p_overall_multi, width = 10, height = 6, dpi = 600)

# Overall Calibration Plot
calibration_data <- dist_data %>%
   mutate(prob_bin = cut(exceedance_probability,
                         breaks = c(seq(0, 1.0, by = 0.01)),
                         include.lowest = TRUE)) %>%
   group_by(distribution_family, prob_bin) %>%
   summarise(
      avg_pred = mean(exceedance_probability, na.rm = TRUE),
      obs_freq = mean(actual_exceedance, na.rm = TRUE),
      n = n(),
      .groups = "drop"
   )
p_calibration <- ggplot(calibration_data, aes(x = avg_pred, y = obs_freq)) +
   geom_point(color = "#2C7BB6", alpha = 0.7) +
   geom_line(color = "#2C7BB6", size = 1) +
   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
   scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
   labs(
      title = "Calibration Curves by Distribution",
      subtitle = "Dashed line = perfect calibration",
      x = "Avg predicted exceedance probability (per bin)",
      y = "Observed exceedance frequency",
      size = "Bin count"
   ) +
   facet_wrap(~ distribution_family, ncol = 3) +
   theme_bw(base_size = 12)
p_calibration
ggsave(paste0(PLOT_PATH, '/OverallDistributionCalibration.png'), p_calibration, width = 10, height = 6, dpi = 600)


# October 2016 plot
oct16_data <- dist_data %>%
   filter(DateTime >= as.POSIXct("2016-10-05") &
             DateTime <= as.POSIXct("2016-10-25"))
   # filter(distribution_family == 'gpd') %>%
   # filter(Year >= 2006 & Year < 2025)

# Find threshold (assume one value, consistent)
salinity_threshold <- unique(oct16_data$salinity_threshold)[1]

# Manual facet labels
dist_labels <- c(
   "burr" = "Burr",
   "gamma" = "Gamma",
   "gengamma" = "Gen. Gamma",
   "gpd" = "GPD",
   "loglogistic" = "Log-Logistic",
   "lognormal" = "Lognormal"
)

p_oct2016 <- ggplot(oct16_data, aes(x = DateTime)) +
   # Exceedance probability
   geom_line(aes(y = exceedance_probability, color = distribution_family), size = 0.7) +
   
   # Raw salinity (scaled)
   geom_line(aes(y = Salinity / salinity_threshold), color = "grey40", linetype = "dashed") +
   
   # Highlight true exceedances as large red dots on salinity
   geom_point(
      data = oct16_data %>% filter(actual_exceedance == 1),
      aes(y = Salinity / salinity_threshold),
      color = "red", size = 1.4
   ) +
   
   facet_wrap(~distribution_family, ncol = 2, labeller = labeller(distribution_family = dist_labels)) +
   scale_y_continuous(
      name = "Exceedance probability",
      sec.axis = sec_axis(~.*salinity_threshold, name = "Salinity (psu)")
   ) +
   labs(x = "Date", title = 'October 2016 Intrusion Event: Exceedance Probability by Distribution',
        subtitle = 'Red points: observed exceedances. Grey dashed line: observed salinity. Colored lines: predicted exceedance probability (0-1)') +
   theme_bw(base_size = 12) +
   theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
   )
p_oct2016

ggsave(paste0(PLOT_PATH, '/Oct2016DistributionComparison.png'), p_oct2016, width = 10, height = 6, dpi = 600)
