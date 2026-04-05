library(dplyr)
library(zoo)
library(ggplot2)

# -------------------------
# USER PARAM
# -------------------------
SALINITY_THRESHOLD <- 0.19

# -------------------------
# STEP 1: Identify flushing + discharge metrics
# -------------------------
test <- plot_data %>%
   arrange(DateTime) %>%
   mutate(
      above = Salinity > SALINITY_THRESHOLD,
      
      # flush = crossing downward through threshold
      flush_day = above == FALSE & lag(above, 1) == TRUE,
      
      # rolling discharge metrics
      RollDischarge3  = rollapply(Discharge, 3,  mean, fill = NA, align = "right"),
      RollDischarge7  = rollapply(Discharge, 7,  mean, fill = NA, align = "right"),
      RollDischarge14 = rollapply(Discharge, 14, mean, fill = NA, align = "right"),
      MaxDischarge7   = rollapply(Discharge, 7,  max,  fill = NA, align = "right"),
      
      # ✅ NEW: pulse metric (simple + effective)
      PulseRatio = MaxDischarge7 / RollDischarge7
   )

# -------------------------
# STEP 2: Extract flush events (your original)
# -------------------------
flush_events <- test %>% filter(flush_day == TRUE)

cat("=== Flush events: N =", nrow(flush_events), "===\n")

cat("Rolling 7-day discharge at flush:\n")
print(quantile(flush_events$RollDischarge7, na.rm = TRUE))

cat("\nMax 7-day discharge at flush:\n")
print(quantile(flush_events$MaxDischarge7, na.rm = TRUE))

# -------------------------
# STEP 3: Compare to non-flushing above-threshold days (your original)
# -------------------------
event_nonflushing <- test %>% 
   filter(above == TRUE, flush_day == FALSE)

cat("\n=== Above-threshold non-flushing days: N =", nrow(event_nonflushing), "===\n")

cat("Rolling 7-day discharge (non-flushing event days):\n")
print(quantile(event_nonflushing$RollDischarge7, na.rm = TRUE))

cat("\nMax 7-day discharge (non-flushing event days):\n")
print(quantile(event_nonflushing$MaxDischarge7, na.rm = TRUE))

# -------------------------
# STEP 4: Seasonal pattern (your original)
# -------------------------
cat("\n=== Flush events by month ===\n")
print(table(lubridate::month(flush_events$DateTime, label = TRUE)))

# -------------------------
# STEP 5: 🔥 NEW — Threshold analysis
# -------------------------
# Restrict to periods where system is salty OR flushing
threshold_data <- test %>%
   filter((above == TRUE | flush_day == TRUE) & !is.na(PulseRatio))

# Bin pulse ratio and compute flush probability
threshold_df <- threshold_data %>%
   mutate(
      pulse_bin = cut(PulseRatio, breaks = seq(1, 5, by = 0.25))
   ) %>%
   group_by(pulse_bin) %>%
   summarise(
      n = n(),
      flush_rate = mean(flush_day, na.rm = TRUE),
      .groups = "drop"
   )

cat("\n=== Flush probability by pulse ratio ===\n")
print(threshold_df)

# -------------------------
# STEP 6: Plot (very helpful)
# -------------------------
ggplot(threshold_df, aes(x = pulse_bin, y = flush_rate)) +
   geom_point() +
   geom_line(group = 1) +
   theme_bw() +
   labs(
      x = "Pulse Ratio (Max7 / Mean7)",
      y = "Flush Probability",
      title = "Flushing Probability vs Discharge Pulse"
   ) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------
# STEP 7: Optional — logistic threshold
# -------------------------
model <- glm(flush_day ~ PulseRatio, data = threshold_data, family = binomial)

threshold <- -coef(model)[1] / coef(model)[2]

cat("\n=== Estimated pulse threshold (50% flush probability) ===\n")
print(threshold)
