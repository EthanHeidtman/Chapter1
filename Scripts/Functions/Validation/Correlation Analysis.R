# Correlation analysis for the two selected predictors
library(ggplot2)
library(corrplot)
library(dplyr)

# Function to analyze predictor correlation
analyze_predictor_correlation <- function(data, pred1 = "Norm_RollingPowDischarge14", pred2 = "Norm_StressHours_30day_Marietta") {
   
   # Basic correlation statistics
   correlation <- cor(data[[pred1]], data[[pred2]], use = "complete.obs")
   
   cat("PREDICTOR CORRELATION ANALYSIS\n")
   cat("==============================\n")
   cat(sprintf("Predictor 1: %s\n", pred1))
   cat(sprintf("Predictor 2: %s\n", pred2))
   cat(sprintf("Pearson correlation: %.3f\n", correlation))
   
   # Additional correlation metrics
   spearman_cor <- cor(data[[pred1]], data[[pred2]], method = "spearman", use = "complete.obs")
   cat(sprintf("Spearman correlation: %.3f\n", spearman_cor))
   
   # Create correlation matrix for visualization
   cor_data <- data[, c(pred1, pred2, "Salinity"), drop = FALSE]
   cor_data <- cor_data[complete.cases(cor_data), ]
   cor_matrix <- cor(cor_data)
   
   cat("\nFull correlation matrix:\n")
   print(round(cor_matrix, 3))
   
   # Statistical significance test
   cor_test <- cor.test(data[[pred1]], data[[pred2]])
   cat(sprintf("\nCorrelation significance test:\n"))
   cat(sprintf("t-statistic: %.3f\n", cor_test$statistic))
   cat(sprintf("p-value: %.2e\n", cor_test$p.value))
   cat(sprintf("95%% CI: [%.3f, %.3f]\n", cor_test$conf.int[1], cor_test$conf.int[2]))
   
   # Create visualizations
   plots <- list()
   
   # 1. Scatter plot
   plots$scatter <- ggplot(data, aes_string(x = pred1, y = pred2)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      geom_smooth(method = "loess", se = FALSE, color = "orange", linetype = "dashed") +
      labs(title = "Relationship Between Selected Predictors",
           subtitle = paste("Correlation =", round(correlation, 3)),
           x = gsub("_", " ", pred1),
           y = gsub("_", " ", pred2)) +
      theme_minimal()
   
   # 2. Scatter plot colored by salinity
   plots$scatter_salinity <- ggplot(data, aes_string(x = pred1, y = pred2, color = "Salinity")) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_c(name = "Salinity") +
      labs(title = "Predictors Colored by Salinity",
           x = gsub("_", " ", pred1),
           y = gsub("_", " ", pred2)) +
      theme_minimal()
   
   # 3. Time series of both predictors
   if ("DateTime" %in% names(data) || "Date" %in% names(data)) {
      time_col <- ifelse("DateTime" %in% names(data), "DateTime", "Date")
      
      # Reshape data for plotting
      plot_data <- data %>%
         select(all_of(c(time_col, pred1, pred2))) %>%
         pivot_longer(cols = c(pred1, pred2), names_to = "Predictor", values_to = "Value")
      
      plots$timeseries <- ggplot(plot_data, aes_string(x = time_col, y = "Value", color = "Predictor")) +
         geom_line(alpha = 0.7) +
         facet_wrap(~Predictor, scales = "free_y", ncol = 1) +
         labs(title = "Time Series of Selected Predictors",
              x = "Time",
              y = "Normalized Value") +
         theme_minimal() +
         theme(legend.position = "none")
   }
   
   # 4. Correlation heatmap
   plots$heatmap <- corrplot(cor_matrix, method = "color", type = "upper", 
                             tl.col = "black", tl.srt = 45, 
                             addCoef.col = "black", number.cex = 0.8)
   
   # 5. Check for regime-specific correlations
   # Create high/low salinity subsets
   high_sal_threshold <- quantile(data$Salinity, 0.9, na.rm = TRUE)
   low_sal_data <- data[data$Salinity <= quantile(data$Salinity, 0.5, na.rm = TRUE), ]
   high_sal_data <- data[data$Salinity >= high_sal_threshold, ]
   
   cor_low <- cor(low_sal_data[[pred1]], low_sal_data[[pred2]], use = "complete.obs")
   cor_high <- cor(high_sal_data[[pred1]], high_sal_data[[pred2]], use = "complete.obs")
   
   cat(sprintf("\nRegime-specific correlations:\n"))
   cat(sprintf("Low salinity periods (≤50th percentile): %.3f\n", cor_low))
   cat(sprintf("High salinity periods (≥90th percentile): %.3f\n", cor_high))
   
   # 6. October 2016 event analysis
   if ("DateTime" %in% names(data) || "Date" %in% names(data)) {
      time_col <- ifelse("DateTime" %in% names(data), "DateTime", "Date")
      oct2016_data <- data[format(data[[time_col]], "%Y-%m") == "2016-10", ]
      
      if (nrow(oct2016_data) > 0) {
         cor_oct2016 <- cor(oct2016_data[[pred1]], oct2016_data[[pred2]], use = "complete.obs")
         cat(sprintf("October 2016 event correlation: %.3f\n", cor_oct2016))
         
         # Plot October 2016 specifically
         plots$oct2016 <- ggplot(oct2016_data, aes_string(x = pred1, y = pred2, color = "Salinity")) +
            geom_point(size = 2, alpha = 0.8) +
            scale_color_viridis_c(name = "Salinity") +
            labs(title = "October 2016 Event: Predictor Relationship",
                 subtitle = paste("Correlation =", round(cor_oct2016, 3)),
                 x = gsub("_", " ", pred1),
                 y = gsub("_", " ", pred2)) +
            theme_minimal()
      }
   }
   
   # Return results
   results <- list(
      correlation = correlation,
      spearman_correlation = spearman_cor,
      correlation_matrix = cor_matrix,
      correlation_test = cor_test,
      regime_correlations = list(
         low_salinity = cor_low,
         high_salinity = cor_high,
         october_2016 = if(exists("cor_oct2016")) cor_oct2016 else NA
      ),
      plots = plots
   )
   
   return(results)
}