# ============================================================================
# 4. TEMPORAL ANALYSIS
# ============================================================================

generate_temporal_plots <- function(pred_df, model_name) {
   
   library(ggplot2)
   
   # Add temporal features
   pred_df$Year <- year(pred_df$DateTime)
   pred_df$Month <- month(pred_df$DateTime)
   pred_df$DOY <- yday(pred_df$DateTime)
   
   # A. Seasonal Performance
   monthly_stats <- pred_df %>%
      group_by(Month) %>%
      summarise(
         n = n(),
         mae = mean(abs(Observed - Predicted), na.rm = TRUE),
         rmse = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
         r2 = cor(Observed, Predicted, use = "complete.obs")^2,
         high_event_count = sum(is_high, na.rm = TRUE),
         .groups = 'drop'
      )
   
   p1 <- ggplot(monthly_stats, aes(x = factor(Month))) +
      geom_col(aes(y = mae), fill = "lightblue", alpha = 0.8) +
      geom_line(aes(y = r2, group = 1), color = "red", size = 1.2) +
      geom_point(aes(y = r2), color = "red", size = 2) +
      scale_y_continuous(
         name = "MAE",
         sec.axis = sec_axis(~., name = "R²")
      ) +
      labs(title = paste(model_name, "- Monthly Performance"),
           subtitle = "Bars = MAE, Red line = R²",
           x = "Month") +
      theme_minimal()
   
   # B. Year-over-Year Performance
   yearly_stats <- pred_df %>%
      group_by(Year) %>%
      summarise(
         n = n(),
         mae = mean(abs(Observed - Predicted), na.rm = TRUE),
         rmse = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
         r2 = cor(Observed, Predicted, use = "complete.obs")^2,
         .groups = 'drop'
      ) %>%
      filter(n >= 50)  # Only years with sufficient data
   
   p2 <- ggplot(yearly_stats, aes(x = Year)) +
      geom_line(aes(y = r2), color = "blue", size = 1.2) +
      geom_point(aes(y = r2), color = "blue", size = 2) +
      geom_smooth(aes(y = r2), method = "loess", se = TRUE, alpha = 0.3) +
      labs(title = paste(model_name, "- Annual Performance Trend"),
           x = "Year", y = "R²") +
      theme_minimal()
   
   # C. Day of Year Analysis (Seasonal Pattern)
   doy_stats <- pred_df %>%
      mutate(DOY_bin = cut(DOY, breaks = seq(1, 366, by = 14))) %>%
      group_by(DOY_bin) %>%
      summarise(
         doy_mid = mean(DOY, na.rm = TRUE),
         mae = mean(abs(Observed - Predicted), na.rm = TRUE),
         obs_mean = mean(Observed, na.rm = TRUE),
         pred_mean = mean(Predicted, na.rm = TRUE),
         .groups = 'drop'
      )
   
   p3 <- ggplot(doy_stats, aes(x = doy_mid)) +
      geom_line(aes(y = obs_mean), color = "black", size = 1, alpha = 0.8) +
      geom_line(aes(y = pred_mean), color = "red", size = 1, alpha = 0.8) +
      geom_ribbon(aes(ymin = pred_mean - mae, ymax = pred_mean + mae), 
                  alpha = 0.3, fill = "red") +
      labs(title = paste(model_name, "- Seasonal Pattern Capture"),
           subtitle = "Black = observed mean, Red = predicted mean ± MAE",
           x = "Day of Year", y = "Mean Salinity") +
      theme_minimal()
   
   return(list(monthly = p1, yearly = p2, seasonal = p3))
}