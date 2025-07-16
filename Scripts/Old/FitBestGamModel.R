
# Source necessary functions
func_env <- new.env()
dirs <- c("Scripts/Functions/NonLinearModeling", "Scripts/Functions/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = func_env)
      })
   })
)

# Load necessary packages
library(here)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read in linear modeling results
linear_predictor_results <- func_env$read_qs_files('Outputs/Experiments/LinearModeling/LinearPredictors.qs')

# Read in model data 
model_data <- as.data.frame(func_env$read_qs_files('Data/Tidied/Final/FinalModelData.qs'))

# Set salinity threshold
salinity_threshold = 0.3

###################################### GAM Model Building #############################

# Create minimal data object - only necessary columns to save space
required_cols <- unique(c('DateTime', 'Year', 'Salinity', linear_predictor_results$predictors$all_predictors))
required_cols <- required_cols[required_cols %in% names(model_data)]
gam_data <- model_data[, required_cols, drop = FALSE]

test_data <- gam_data %>%
   filter(Year == 2016)

# Basic GAM model building (just 2016)
basic_gam2016 <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold)

   # Write output file
   outputs <- list(basic_gam2016)
   file_names <- c('2016TestBasicGAM')
   write_qs_files(outputs, 'Outputs/NonLinearModeling', file_names)
   basic_gam2016 <- func_env$read_qs_files('Outputs/NonLinearModeling/2016TestBasicGAM.qs')

# Basic GAM model building (all years)
basic_gam <- func_env$parallel_gam_model_builder(gam_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold)
   
   # Write output file
   outputs <- list(basic_gam)
   file_names <- c('FullTestBasicGAM')
   write_qs_files(outputs, 'Outputs/NonLinearModeling', file_names)
   basic_gam <- func_env$read_qs_files('Outputs/NonLinearModeling/FullTestBasicGAM.qs')

# GAM with Auto-regressive terms of order 1 (just 2016)
ar1_gam2016 <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 1)

   # Write output file
   outputs <- list(ar1_gam2016)
   file_names <- c('2016TestAR1')
   write_qs_files(outputs, 'Outputs/NonLinearModeling', file_names)
   ar1_gam2016 <- func_env$read_qs_files('Outputs/NonLinearModeling/2016TestAR1.qs')

# GAM with Auto-regressive terms of order 1 (all years)
ar1_gam <- func_env$parallel_gam_model_builder(gam_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 1)

   # Write output file
   outputs <- list(ar1_gam)
   file_names <- c('FullTestAR1')
   write_qs_files(outputs, 'Outputs/NonLinearModeling', file_names)
   ar1_gam <- func_env$read_qs_files('Outputs/NonLinearModeling/FullTestAR1.qs')

# GAM with Auto-regressive terms of order 2 (just 2016)
ar2_gam2016 <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 2)
qsave(ar2_gam2016, '~/Desktop/Testing/2016TestAR2.qs')
ar2_gam2016 <-  qread('~/Desktop/Testing/2016TestAR2.qs')

# GAM with Auto-regressive terms of order 2 (all years)
ar2_gam <- func_env$parallel_gam_model_builder(gam_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 2)
qsave(ar2_gam, '~/Desktop/Testing/FullTestAR2.qs')
ar2_gam <-  qread('~/Desktop/Testing/FullTestAR2.qs')

# Quantile GAM with 75th percentile (just 2016)
q75_gam2016 <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_qgam = TRUE, quantile = 0.75)

# Quantile GAM with 75th percentile (all years)
q75_gam <- func_env$parallel_gam_model_builder(gam_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_qgam = TRUE, quantile = 0.75)

# Combined AR + QGAM
gam_combined <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 1, use_qgam = TRUE, quantile = 0.9)












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
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('Best Linear Model:\n', linear_model_results$formula)) + 
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

test_df <- func_env$get_predictions(ar1_gam2016$model, test_data, 'gam')
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
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('Best GAM Model:\n', ar1_gam2016$model$formula)) + 
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


