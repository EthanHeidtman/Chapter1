################################################################################
# Written by Ethan Heidtman, April 2025

# This script uses modeled tidal data, observed discharge data for the Conowingo 
# Dam, and observed/modeled salinity data at Havre de Grace to first develop a 
# predictive relationship for salinity near the Havre de Grace Drinking water
# intake. The predictive relationship is then formulated into an objective function
# that represents a shortage index, the amount of time/probability that the Dam's
# releases are not enough to dilute salt below the safe threshold.


############################ LOAD FUNCTIONS, PACKAGES, AND DATA ############################

# Source necessary functions
#func_env <- new.env()
dirs <- c("Scripts/Functions/LinearModeling", "Scripts/Functions/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Load necessary packages
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))

# Define salinity threshold
salinity_threshold = 1.0

######################### SIMPLE LINEAR MODEL DEVELOPMENT ############################

# Define predictor categories and their candidates
predictor_config <- list(

   # Tide predictors (will always include the best one in subsequent models)
   tide = c("Norm_Tide", "Norm_TideRate", 'Norm_LagTide1', 'Norm_LagTide2', 'Norm_LagTide4',
            'Norm_TideRange6', 'Norm_TideRange12', 'Norm_TideRange24', 'Norm_LowFlowTideRange',
            'Norm_WeightedTideRange12'),

   # Discharge predictors (test systematically)
   discharge_lag = c("Norm_PowLagDischarge1", "Norm_PowLagDischarge3", "Norm_PowLagDischarge6",
                     "Norm_PowLagDischarge10", "Norm_PowLagDischarge12", "Norm_PowLagDischarge24",
                     "Norm_PowLagDischarge36", "Norm_PowLagDischarge48", "Norm_PowLagDischarge72"),

   discharge_rolling = c("Norm_RollingPowDischarge0.5", "Norm_RollingPowDischarge1",
                         "Norm_RollingPowDischarge2", "Norm_RollingPowDischarge4",
                         "Norm_RollingPowDischarge7", "Norm_RollingPowDischarge10",
                         "Norm_RollingPowDischarge14"),

   # Inflow predictors
   inflow_lag = c("Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72"),

   inflow_rolling = c("Norm_RollingPowInflows1", "Norm_RollingPowInflows2",
                      "Norm_RollingPowInflows7", "Norm_RollingPowInflows10"),

   # Latent flow features
   latent_flow = c("Norm_SimpleLatent", "Norm_StressLatent", "Norm_BestLatent"),

   # Stress indicators
   stress_binary = c("IsModerateStress", "IsHighStress", "IsFlush", "IsStressed"),
   stress_continuous = c("Norm_StressHours_7day_Marietta", "Norm_StressHours_14day_Marietta",
                         "Norm_StressHours_30day_Marietta", "Norm_StressHours_7day_Conowingo",
                         "Norm_StressHours_14day_Conowingo", "Norm_StressHours_30day_Conowingo",
                         "Norm_CumulativeStress_7day_Marietta", "Norm_CumulativeStress_14day_Marietta",
                         "Norm_CumulativeStress_30day_Marietta", "DaysSinceHighFlow"),

   # Seasonal/temporal
   temporal = c("SalinitySeason", "DayOfYear")

)

# Define performance criteria with updated weights
performance_criteria <- list(
   weights = c(
      # High salinity event metrics (65% of total weight)
      "high_sal_detection" = 0.30,      # Detection capability 
      "high_sal_accuracy" = 0.25,       # Accuracy of high salinity predictions
      "high_sal_reliability" = 0.10,    # Reliability (false alarm control)
      
      # Overall model performance (30% of total weight)
      "overall_performance" = 0.25,     # General model fit
      "model_stability" = 0.05,         # Consistent performance across conditions
      
      # Model characteristics (5% of total weight)
      "parsimony" = 0.05                # Model complexity penalty
   ),
   
   thresholds = list(
      min_high_sal_count = 3,           
      high_salinity_threshold = 0.3,    
      acceptable_far = 0.30,            
      min_hit_rate = 0.40               
   )
)

# Save model building output as a text file
sink("Outputs/LinearModeling/LinearModelBuilderLog.txt")

linear_model_results <- linear_model_builder(model_data, salinity_threshold)

# Stop redirecting output and return to console
sink()

# Strip stage results before writing (huge, take time to save and not really needed)
linear_model_results$stage_results <- NULL

# Write output files
outputs <- list(linear_model_results)
file_names <- c('LinearModelResults')
write_qs_files(outputs, 'Outputs/LinearModeling', file_names)

# Clear environment
rm(list = ls())





############################### MODEL EVALUATION ###############################
linear_df <- get_predictions(linear_model_results$model, model_data)
linear_df <- linear_df %>%
   mutate(Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime))
ggplot(linear_df, aes(x = DateTime)) +
   geom_line(aes(y = Observed, color = 'Observed'), na.rm = TRUE, linewidth = 0.5) + 
   geom_line(aes(y = Predicted, color = 'Predicted'), na.rm = TRUE, linewidth = 1.1) + 
   geom_point(data = filter(linear_df, Observed >= 1), aes(y = Observed, color = 'Above Threshold'), na.rm = TRUE, size = 2) +
   scale_color_manual(name = NULL, values = c('Observed' = 'black', 'Predicted' = 'blue', 'Above Threshold' = 'red')) + 
   #scale_x_datetime(limits = c(as_datetime('2011-02-28'), as_datetime('2011-12-31')), date_labels = '%b-%Y') + 
   theme_bw() + 
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('Best Linear Model:\n', linear_model)) + 
   theme(plot.title = element_text(size = 16),
         legend.text = element_text(size = 14), 
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 14)) +
   facet_wrap(~Year, scales = 'free')

gam_df <- func_env$get_predictions(gam_ar$model, test_data, 'gam')
gam_df <- gam_df %>%
   mutate(Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime))
ggplot(gam_df, aes(x = DateTime)) +
   geom_line(aes(y = Observed, color = 'Observed'), na.rm = TRUE, linewidth = 0.5) + 
   geom_line(aes(y = Predicted, color = 'Predicted'), na.rm = TRUE, linewidth = 1.1) + 
   geom_point(data = filter(gam_df, Observed >= 1), aes(y = Observed, color = 'Above Threshold'), na.rm = TRUE, size = 2) +
   scale_color_manual(name = NULL, values = c('Observed' = 'black', 'Predicted' = 'blue', 'Above Threshold' = 'red')) + 
   #scale_x_datetime(limits = c(as_datetime('2011-02-28'), as_datetime('2011-12-31')), date_labels = '%b-%Y') + 
   theme_bw() + 
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('Best GAM Model:\n', gam_ar$model$formula)) + 
   theme(plot.title = element_text(size = 16),
         legend.text = element_text(size = 14), 
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 14)) +
   facet_wrap(~Year, scales = 'free')

test_df <- func_env$get_predictions(basic_gam2016$model, test_data, 'gam')
test_df <- test_df %>%
   mutate(Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime))
ggplot(test_df, aes(x = DateTime)) +
   geom_line(aes(y = Observed, color = 'Observed'), na.rm = TRUE, linewidth = 0.5) + 
   geom_line(aes(y = Predicted, color = 'Predicted'), na.rm = TRUE, linewidth = 1.1) + 
   geom_point(data = filter(test_df, Observed >= 1), aes(y = Observed, color = 'Above Threshold'), na.rm = TRUE, size = 2) +
   scale_color_manual(name = NULL, values = c('Observed' = 'black', 'Predicted' = 'blue', 'Above Threshold' = 'red')) + 
   #scale_x_datetime(limits = c(as_datetime('2011-02-28'), as_datetime('2011-12-31')), date_labels = '%b-%Y') + 
   theme_bw() + 
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('Best GAM Model:\n', basic_gam$model$formula)) + 
   theme(plot.title = element_text(size = 16),
         legend.text = element_text(size = 14), 
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 14)) +
   facet_wrap(~Year, scales = 'free')





# Check the results
cat("Predicted range:", range(test_preds$Predicted, na.rm = TRUE), "\n")
cat("Lower CI range:", range(test_preds$lower_ci, na.rm = TRUE), "\n") 
cat("Upper CI range:", range(test_preds$upper_ci, na.rm = TRUE), "\n")

# All should be positive now

# test <- get_predictions(linear_model_results[["model"]], model_data)
# test <- test %>%
#    mutate(Year = year(DateTime),
#           Month = month(DateTime),
#           Day = day(DateTime))
# high_events <- test %>% 
#    filter(is_high) %>% 
#    arrange(DateTime)
# 
# if(nrow(high_events) > 0) {
#    # Get a window around the first high event
#    first_high_event <- high_events$DateTime[1]
#    window_start <- first_high_event - days(5)
#    window_end <- first_high_event + days(5)
#    
#    p7 <- ggplot(filter(test, DateTime >= window_start & DateTime <= window_end), 
#                 aes(x = DateTime)) +
#       geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
#       geom_line(aes(y = Observed), color = "black") +
#       geom_line(aes(y = Predicted), color = "blue") +
#       geom_point(data = filter(test, is_high & DateTime >= window_start & DateTime <= window_end), 
#                  aes(y = Observed), color = "red", size = 2) +
#       labs(title = "10-Day Window Around a High Salinity Event",
#            x = "Date",
#            y = "Salinity (ppt)") +
#       theme_minimal() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1))
#    
#    print(p7)
# }

p1 <- ggplot(test3, aes(x = DateTime)) +
   geom_line(aes(y = Observed, color = 'Observed'), na.rm = TRUE, linewidth = 0.5) + 
   geom_line(aes(y = Predicted, color = 'Predicted'), na.rm = TRUE, linewidth = 1.1) + 
   geom_point(data = filter(test, Observed >= 1), aes(y = Observed, color = 'Above Threshold'), na.rm = TRUE, size = 2) +
   scale_color_manual(name = NULL, values = c('Observed' = 'black', 'Predicted' = 'blue', 'Above Threshold' = 'red')) + 
   facet_wrap(~Year, scale = 'free') + 
   labs(x = 'Date', y = 'Salinity (ppt)') + 
   theme_bw()
ggsave('all_years.png', p1, path = '~/Downloads', dpi = 700, height = 9, width = 15)



p1 <- ggplot(test, aes(x = DateTime)) +
   geom_line(aes(y = Observed, color = 'Observed'), na.rm = TRUE, linewidth = 0.5) + 
   geom_line(aes(y = Predicted, color = 'Predicted'), na.rm = TRUE, linewidth = 1.1) + 
   geom_point(data = filter(test, Observed >= 1), aes(y = Observed, color = 'Above Threshold'), na.rm = TRUE, size = 2) +
   scale_color_manual(name = NULL, values = c('Observed' = 'black', 'Predicted' = 'blue', 'Above Threshold' = 'red')) + 
   scale_x_datetime(limits = c(as_datetime('2011-02-28'), as_datetime('2011-12-31')), date_labels = '%b-%Y') + 
   theme_bw() + 
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('2015 Best Model:\n', linear_model_results[["formula"]])) + 
   #labs(x = 'Date', y = 'Salinity (ppt)', title = '2016 Best Model: Salinity ~ 4hrTideLag + 2WeekRollingDischarge + 10DayRollingInflow') +
   ylim(0, 0.5) + 
   theme(plot.title = element_text(size = 16),
         legend.text = element_text(size = 14), 
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 14)) 


ggsave('2015.png', p1, path = '~/Downloads', dpi = 700, height = 8, width = 14)
ggsave('2016.png', p1, path = '~/Downloads', dpi = 700, height = 8, width = 14)


quick_correlation <- function(data, pred1 = "Norm_RollingPowDischarge14", pred2 = "Norm_StressHours_30day_Marietta") {
   cor_val <- cor(data[[pred1]], data[[pred2]], use = "complete.obs")
   cat(sprintf("Correlation between %s and %s: %.3f\n", pred1, pred2, cor_val))
   return(cor_val)
}

quick_correlation(model_data)

correlation_results <- analyze_predictor_correlation(model_data)
print(correlation_results$plots$scatter_salinity)

plots <- create_cleaner_scatter(model_data)
print(plots$hexbin)           # Shows data density
print(plots$extreme_events)   # Highlights high salinity events
print(plots$october_2016)     # Focuses on Oct 2016
print(plots$contour_surface)  # Shows salinity surface


#plots <- generate_model_diagnostics(model = linear_model_results$model, model_name = 'Best Linear Model', data = model_data)
# plots$plots$performance
# plots$plots$high_salinity
# plots$plots$correlations
# plots$plots$temporal
# plots$plots$residuals
# plots$statistics

