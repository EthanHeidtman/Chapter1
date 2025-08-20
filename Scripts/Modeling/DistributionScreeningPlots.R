# 1. IDENTIFY ACTUAL EXCEEDANCES
# Get the salinity threshold (assuming it's consistent)
salinity_threshold <- unique(all_data$salinity_threshold)[1]

# Create exceedance indicators
all_data <- all_data %>%
   mutate(
      Date = as.Date(DateTime),
      actual_exceedance = Salinity > salinity_threshold,
      is_oct_2016 = (Year == 2016 & Month == 10)
   )

# Summarize actual exceedances
exceedance_summary <- all_data %>%
   group_by(Year, Month) %>%
   summarise(
      hours_exceeded = sum(actual_exceedance, na.rm = TRUE),
      max_salinity = max(Salinity, na.rm = TRUE),
      .groups = 'drop'
   ) %>%
   filter(hours_exceeded > 0)

print("=== ACTUAL EXCEEDANCE EVENTS ===")
print(exceedance_summary)

# October 2016 detailed data
oct_2016_data <- all_data %>%
   filter(is_oct_2016) %>%
   arrange(DateTime)

oct_2016_exceedances <- oct_2016_data %>%
   filter(actual_exceedance) %>%
   group_by(distribution_family) %>%
   summarise(
      n_exceedance_hours = n() / length(unique(distribution_family)), # Divide by # distributions since data is stacked
      mean_predicted_prob_during_exceedance = mean(exceedance_probability, na.rm = TRUE),
      max_predicted_prob_during_exceedance = max(exceedance_probability, na.rm = TRUE),
      min_predicted_prob_during_exceedance = min(exceedance_probability, na.rm = TRUE),
      .groups = 'drop'
   )

print("=== OCTOBER 2016 EXCEEDANCE PERFORMANCE ===")
print(oct_2016_exceedances)

# 2. VISUALIZATION 1: October 2016 Detective Story
# Create a multi-panel story of what happened

oct_daily <- oct_2016_data %>%
   mutate(hour_of_day = hour(DateTime)) %>%
   group_by(Day, distribution_family) %>%
   summarise(
      daily_max_salinity = max(Salinity, na.rm = TRUE),
      daily_max_exc_prob = max(exceedance_probability, na.rm = TRUE),
      daily_hours_exceeded = sum(actual_exceedance, na.rm = TRUE) / length(unique(distribution_family)),
      .groups = 'drop'
   )

# Panel 1: What actually happened (salinity time series)
p1_actual <- ggplot(oct_2016_data %>% filter(distribution_family == unique(distribution_family)[1]), 
                    aes(x = DateTime)) +
   geom_line(aes(y = Salinity), color = "steelblue", size = 1) +
   geom_hline(yintercept = salinity_threshold, color = "red", linetype = "dashed", size = 1) +
   geom_ribbon(aes(ymin = pmin(Salinity, salinity_threshold), 
                   ymax = salinity_threshold), 
               alpha = 0.3, fill = "gray") +
   geom_ribbon(data = oct_2016_data %>% 
                  filter(distribution_family == unique(distribution_family)[1] & actual_exceedance),
               aes(ymin = salinity_threshold, ymax = Salinity), 
               alpha = 0.7, fill = "red") +
   scale_y_log10() +
   labs(title = "October 2016: The Real Story",
        subtitle = paste("Red areas show actual exceedances above threshold =", salinity_threshold),
        x = "Date", y = "Salinity (log scale)") +
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"))

# Panel 2: Combined view inspired by your approach - faceted for clarity
# Extend the window slightly for context
start_date <- as.Date("2016-10-01") - 5
end_date <- as.Date("2016-10-31") + 5

oct_extended <- all_data %>%
   filter(Date >= start_date & Date <= end_date,
          !is.na(exceedance_probability)) %>%
   mutate(
      max_salinity = max(Salinity, na.rm = TRUE),
      prob_scaled = exceedance_probability * max_salinity
   )

p2_predictions <- ggplot(oct_extended, aes(x = DateTime)) +
   
   # Salinity line
   geom_line(aes(y = Salinity), color = "steelblue", size = 0.6, alpha = 0.8) +
   
   # Threshold line with label
   geom_hline(
      yintercept = salinity_threshold, 
      color = "firebrick", linetype = "solid", size = 1
   ) +
   annotate("text", 
            x = as.POSIXct("2016-10-05"), 
            y = salinity_threshold + 0.05, 
            label = "Threshold", 
            color = "firebrick", hjust = 0, vjust = -0.5, size = 3.5) +
   
   # Actual exceedances as red points
   geom_point(
      data = dplyr::filter(oct_extended, actual_exceedance),
      aes(y = Salinity), color = "red", size = 2.5, alpha = 0.9
   ) +
   
   # Predicted probability as colored points
   geom_point(
      aes(y = Salinity, color = exceedance_probability),
      size = 1.5, alpha = 0.8
   ) +
   
   # Predicted probability trend line
   geom_line(aes(y = prob_scaled), 
             color = "black", linetype = "longdash", size = 0.8, alpha = 0.6) +
   
   # Better probability color scale
   scale_color_gradientn(
      name = "Predicted Probability",
      colors = c("skyblue", "gold", "tomato", "darkred"),
      values = rescale(c(0, 0.1, 0.5, 1)),
      labels = percent_format(accuracy = 1),
      guide = guide_colorbar(
         barwidth = 10, barheight = 0.6, 
         title.position = "top", title.hjust = 0.5,
         direction = "horizontal"
      )
   ) +
   
   # Highlight October
   # annotate("rect",
   #          xmin = as.POSIXct("2016-10-01"),
   #          xmax = as.POSIXct("2016-11-01"),
   #          ymin = -Inf, ymax = Inf,
   #          fill = "grey90", alpha = 0.5) +
   
   # Facets: 2 columns, free y
   facet_wrap(~distribution_family, ncol = 2, scales = "free_y") +
   
   # Outer-only axes using ggh4x
   facetted_pos_scales(
      y = c(
         # Left column: salinity repeated for each row
         replicate(3, scale_y_continuous(name = "Salinity"), simplify = FALSE),
         # Right column: probability repeated for each row
         replicate(3, scale_y_continuous(
            sec.axis = sec_axis(
               ~ . / max(oct_extended$max_salinity, na.rm = TRUE),
               name = "Predicted Probability",
               labels = percent_format()
            )
         ), simplify = FALSE)
      )
   )+
   
   labs(
      title = "October 2016 Event: Model Performance by Distribution",
      subtitle = "Red points = actual exceedances | Point color = predicted probability | Black dashed = probability trend",
      x = "Date"
   ) +
   
   theme_minimal(base_size = 12) +
   theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "grey85", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y.right = element_text(margin = margin(l = 10)),
      axis.title.y.left = element_text(margin = margin(r = 10))
   )

# Panel 3: Direct comparison during exceedance events
exc_moments <- oct_2016_data %>% 
   filter(actual_exceedance) %>%
   group_by(distribution_family) %>%
   summarise(
      avg_prob_during_exc = mean(exceedance_probability, na.rm = TRUE),
      .groups = 'drop'
   )

p3_performance <- ggplot(exc_moments, aes(x = reorder(distribution_family, avg_prob_during_exc), 
                                          y = avg_prob_during_exc)) +
   geom_col(aes(fill = distribution_family), alpha = 0.8) +
   geom_text(aes(label = paste0(round(avg_prob_during_exc * 100, 1), "%")), 
             hjust = -0.1, size = 3.5, fontface = "bold") +
   scale_fill_viridis_d() +
   scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
   coord_flip() +
   labs(title = "Average Predicted Probability During Actual Exceedances",
        subtitle = "Higher is better - means the model 'saw it coming'",
        x = "Distribution", y = "Average Predicted Probability") +
   theme_minimal() +
   theme(legend.position = "none",
         plot.title = element_text(size = 12))

# Combine October 2016 story
oct_story <- p1_actual  / p3_performance
print("=== OCTOBER 2016 DETECTIVE STORY ===")
print(oct_story)
print(p2_predictions)

# 3. VISUALIZATION 2: Exceedance Detection Analysis
# How sensitive is each distribution to approaching exceedances?

# Create "approach to exceedance" periods (hours before exceedances)
approach_data <- oct_2016_data %>%
   filter(distribution_family == unique(distribution_family)[1]) %>%
   arrange(DateTime) %>%
   mutate(
      hours_to_exceedance = NA_real_,
      approaching_exceedance = FALSE
   )

# Find approach periods (24 hours before exceedances)
exc_times <- which(approach_data$actual_exceedance)
for(exc_time in exc_times) {
   start_approach <- max(1, exc_time - 24)
   approach_data$hours_to_exceedance[start_approach:exc_time] <- 
      (exc_time - (start_approach:exc_time))
   approach_data$approaching_exceedance[start_approach:(exc_time-1)] <- TRUE
}

# Add this back to the main data for all distributions
oct_2016_enhanced <- oct_2016_data %>%
   left_join(approach_data %>% select(DateTime, hours_to_exceedance, approaching_exceedance),
             by = "DateTime") %>%
   mutate(
      event_phase = case_when(
         actual_exceedance ~ "During Exceedance",
         approaching_exceedance ~ "Approaching (24h before)",
         TRUE ~ "Normal Conditions"
      )
   )

# Exceedance sensitivity plot
p4_sensitivity <- ggplot(oct_2016_enhanced %>% filter(!is.na(hours_to_exceedance)), 
                         aes(x = hours_to_exceedance, y = exceedance_probability)) +
   geom_smooth(aes(color = distribution_family), method = "loess", se = TRUE, alpha = 0.7) +
   geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
   scale_color_viridis_d(name = "Distribution") +
   scale_y_continuous(labels = percent_format()) +
   scale_x_reverse() +
   labs(title = "Early Warning Performance: Approaching Exceedances",
        subtitle = "How do predicted probabilities change as exceedances approach?\nRed line = exceedance occurs",
        x = "Hours Before Exceedance", 
        y = "Predicted Exceedance Probability") +
   theme_minimal() +
   theme(legend.position = "bottom")

print(p4_sensitivity)

# 4. VISUALIZATION 3: False Alarm vs Miss Analysis
# Create binary predictions at different probability thresholds
prob_thresholds <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.5)

roc_data <- map_dfr(prob_thresholds, function(thresh) {
   oct_2016_data %>%
      mutate(
         predicted_exceedance = exceedance_probability >= thresh,
         true_positive = actual_exceedance & predicted_exceedance,
         false_positive = !actual_exceedance & predicted_exceedance,
         false_negative = actual_exceedance & !predicted_exceedance,
         true_negative = !actual_exceedance & !predicted_exceedance
      ) %>%
      group_by(distribution_family) %>%
      summarise(
         threshold = thresh,
         sensitivity = sum(true_positive) / sum(actual_exceedance), # True positive rate
         specificity = sum(true_negative) / sum(!actual_exceedance), # True negative rate
         false_positive_rate = 1 - specificity,
         precision = ifelse(sum(predicted_exceedance) > 0, 
                            sum(true_positive) / sum(predicted_exceedance), 0),
         .groups = 'drop'
      )
})

# ROC-style plot
p5_roc <- ggplot(roc_data, aes(x = false_positive_rate, y = sensitivity, color = distribution_family)) +
   geom_line(size = 1.2) +
   geom_point(aes(size = threshold), alpha = 0.7) +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
   scale_color_viridis_d(name = "Distribution") +
   scale_size_continuous(name = "Probability\nThreshold", 
                         breaks = c(0.001, 0.01, 0.1, 0.5),
                         labels = c("0.1%", "1%", "10%", "50%")) +
   labs(title = "ROC Analysis: Detection vs False Alarms",
        subtitle = "October 2016 - Closer to top-left corner is better",
        x = "False Positive Rate", y = "Sensitivity (True Positive Rate)") +
   theme_minimal() +
   theme(legend.position = "right")

print(p5_roc)

# 5. VISUALIZATION 4: Full Time Series with Exceedance Focus
# Show the full dataset but highlight exceedance periods

# Sample data for full time series (every 168th observation = weekly)
ts_weekly <- all_data %>%
   arrange(DateTime) %>%
   slice(seq(1, n(), by = 168)) %>%  # Weekly samples
   group_by(Date, distribution_family) %>%
   summarise(
      max_exc_prob = max(exceedance_probability, na.rm = TRUE),
      any_exceedance = any(actual_exceedance, na.rm = TRUE),
      max_salinity = max(Salinity, na.rm = TRUE),
      .groups = 'drop'
   )

p6_full_series <- ggplot(ts_weekly, aes(x = Date, y = max_exc_prob)) +
   geom_line(aes(color = distribution_family), alpha = 0.6, size = 0.5) +
   geom_point(data = ts_weekly %>% filter(any_exceedance),
              aes(color = distribution_family), size = 3, alpha = 0.9) +
   geom_rect(data = data.frame(xmin = as.Date("2016-10-01"), xmax = as.Date("2016-10-31"),
                               ymin = -Inf, ymax = Inf),
             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
   scale_color_viridis_d(name = "Distribution") +
   scale_y_continuous(labels = percent_format()) +
   labs(title = "Full Time Series: Weekly Maximum Exceedance Probabilities",
        subtitle = "Large points = weeks with actual exceedances, Red box = October 2016",
        x = "Date", y = "Weekly Max Exceedance Probability") +
   theme_minimal() +
   theme(legend.position = "bottom")

print(p6_full_series)

# 6. SUMMARY PERFORMANCE TABLE
performance_summary <- oct_2016_exceedances %>%
   left_join(
      oct_2016_data %>%
         group_by(distribution_family) %>%
         summarise(
            correlation_with_salinity = cor(exceedance_probability, Salinity, use = "complete.obs"),
            peak_prediction_timing = DateTime[which.max(exceedance_probability)],
            peak_actual_timing = DateTime[which.max(Salinity)],
            .groups = 'drop'
         ),
      by = "distribution_family"
   ) %>%
   mutate(
      timing_offset_hours = as.numeric(difftime(peak_prediction_timing, peak_actual_timing, units = "hours")),
      performance_grade = case_when(
         mean_predicted_prob_during_exceedance >= 0.5 ~ "A (Excellent)",
         mean_predicted_prob_during_exceedance >= 0.2 ~ "B (Good)", 
         mean_predicted_prob_during_exceedance >= 0.1 ~ "C (Fair)",
         mean_predicted_prob_during_exceedance >= 0.05 ~ "D (Poor)",
         TRUE ~ "F (Failed)"
      )
   ) %>%
   arrange(desc(mean_predicted_prob_during_exceedance))

print("=== OCTOBER 2016 EXCEEDANCE DETECTION REPORT CARD ===")
print(performance_summary %>% 
         select(distribution_family, performance_grade, 
                mean_predicted_prob_during_exceedance, correlation_with_salinity))



plot_october_2016_event_all <- function(all_data, threshold = 1.0, extend_days = 7) {
   
   # Define October 2016 event window with buffer
   start_date <- as.Date("2016-10-01") - extend_days
   end_date   <- as.Date("2016-10-31") + extend_days
   
   # Prep data
   event_data <- all_data %>%
      mutate(DateTime = as.POSIXct(DateTime),
             Date = as.Date(DateTime),
             Actual_Exceedance = Salinity > threshold) %>%
      filter(Date >= start_date & Date <= end_date,
             !is.na(exceedance_probability))
   
   # Plot
   p <- ggplot(event_data, aes(x = DateTime)) +
      # Salinity timeseries
      geom_line(aes(y = Salinity), color = "blue", size = 0.8, alpha = 0.8) +
      
      # Threshold
      geom_hline(yintercept = threshold, color = "red", linetype = "dashed", size = 0.8) +
      
      # Highlight observed exceedances
      geom_point(data = filter(event_data, Actual_Exceedance),
                 aes(y = Salinity), color = "red", size = 2, alpha = 0.8) +
      
      # Predicted exceedance probability as color gradient on points
      geom_point(aes(y = Salinity, color = exceedance_probability),
                 size = 1.8, alpha = 0.9) +
      
      scale_color_gradient(
         name = "Predicted\nExceedance\nProbability",
         low = "yellow", high = "darkred",
         labels = scales::percent_format(accuracy = 1)
      ) +
      
      # Overlay probability line on secondary axis
      geom_line(aes(y = exceedance_probability * max(Salinity, na.rm = TRUE)),
                color = "black", linetype = "dotted", size = 0.7, alpha = 0.7) +
      
      scale_y_continuous(
         name = "Salinity (psu)",
         sec.axis = sec_axis(~ . / max(event_data$Salinity, na.rm = TRUE),
                             name = "Predicted Probability")
      ) +
      
      # Time axis formatting
      scale_x_datetime(date_labels = "%b %d", date_breaks = "3 days") +
      
      # Highlight October 2016
      annotate("rect",
               xmin = as.POSIXct("2016-10-01"),
               xmax = as.POSIXct("2016-10-31"),
               ymin = -Inf, ymax = Inf,
               alpha = 0.1, fill = "yellow") +
      
      labs(
         title = "October 2016 Salinity Event – Model Performance",
         subtitle = paste("Red points = actual exceedances above", threshold, 
                          "psu | Point color = predicted probability | Black dotted line = probability trend"),
         x = "Date"
      ) +
      facet_wrap(~distribution_family, ncol = 2, scales = "free_y") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   return(p)
}


plot_october_2016_event_all(all_data)
