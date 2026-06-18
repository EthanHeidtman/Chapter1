# =============================================================================
# Script Name:    05_EvaluateDailyModels.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tidyr)
library(mgcv)
library(svglite)

# Source necessary functions 
source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Plots/SimpleModels/ModelEvaluationPlots.R')
source('Scripts/Plots/MultiPanelModelPlot.R')
source('Scripts/Utilities/ComputeGamPerformance.R')
source('Scripts/Plots/GamEvalPlots.R')

# Define lead times that were run
lead_times <- seq(0, 30, 1)

# Initialize lists to store results
screened_data <- list()
rf_results <- list()
top_vars_by_k <- list()
gam_predictions <- list()
predictors_used <- list()
models <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
   
}

# Loop through each k to generate predictions and keep all data
for(k in lead_times) {
   
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k (FULL dataset)
   daily_data_k <- screened_data[[lag_name]]
   
   # Select variables
   salinity_cluster <- daily_data_k %>% dplyr::select(c(contains('Salinity')))
   sustained_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('RollingDischarge', 'RollingAnomaly'))))
   flushing_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('MaxDischarge', 'ExceedFlux')))) 
   tide_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('TideRange', 'TideMean'))))
   wind_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('RollingWindAlong', 'RollingWindCross'))))
   
   group_list_k <- list(
      Salinity = salinity_cluster,
      SustainedDischarge = sustained_discharge_cluster,
      FlushingDischarge = flushing_discharge_cluster,
      Tide = tide_cluster,
      Wind = wind_cluster
   )
   
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df = rf_results[[lag_name]]$importance,
      group_dfs = group_list_k,
      n_top = 1,
      # n_top = list(
      #    salinity = 1, 
      #    rolling_discharge = 1, 
      #    flushing_discharge = 1, 
      #    tide = 1, 
      #    wind = 1
      # ),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   # Top predictors
   top_vars <- unname(vapply(top_vars_by_k[[lag_name]], function(x) x$Variable, character(1)))
   predictors_used[[lag_name]] <- top_vars
   
   # Build model input
   model_data_k <- daily_data_k %>%
      dplyr::select(c(1:"Salinity", all_of(top_vars))) %>%
      # drop_na(Salinity, all_of(top_vars)) %>% 
      { 
         # If there is an "Along" wind variable 
         if (any(grepl("Along", top_vars))) {
            wind_var <- top_vars[grepl("Along", top_vars)][1]
            daily_data_k <- daily_data_k %>%
               mutate(WindDir = factor(
                  ifelse(.data[[wind_var]] >= 0, "UpEstuary", "DownEstuary")
               ))
         } else if (any(grepl("Cross", top_vars))) {
            wind_var <- top_vars[grepl("Cross", top_vars)][1]
            daily_data_k <- daily_data_k %>%
               mutate(WindDir = factor(
                  ifelse(.data[[wind_var]] >= 0, "RightBank", "LeftBank")
               ))
         } else {
            .
         }
      }
   
   
   # Mask rows with complete predictors
   valid_rows <- complete.cases(model_data_k[, top_vars])
   
   # Load GAM model
   gam_file <- paste0('Outputs/Experiments/Models/DailyGAM/Gam_', k, '.qs')
   model_obj <- read_qs_files(gam_file)
   
   models[[paste0('Lag', k)]] <- model_obj
   
   # Predict on valid rows
   pred <- rep(NA_real_, nrow(model_data_k))
   
   pred[valid_rows] <- tryCatch({
      
      if (!is.null(model_obj$gam_object)) {
         
         transform_info <- model_obj$transform_info
         family_type <- transform_info$family
         manual_transform <- transform_info$manual_transform
         
         pred_response <- predict(
            model_obj$gam_object,
            newdata = model_data_k[valid_rows, ],
            type = "response"
         )
         
         # Back-transform if needed
         if (family_type == "gaussian" && manual_transform == "log") {
            sigma_sq <- transform_info$sigma_sq
            pred_out <- exp(pred_response + sigma_sq/2)
            
            if (any(pred_out > 10, na.rm = TRUE) || any(is.infinite(pred_out))) {
               warning(sprintf("Model lag%d has extreme/infinite predictions", k))
            }
            
            pred_out
            
         } else if (family_type == "gaussian" && manual_transform == "sqrt") {
            pred_response^2
            
         } else {
            pred_response
         }
         
      } else if (!is.null(model_obj$final_fit)) {
         
         predict(
            model_obj$final_fit,
            new_data = model_data_k[valid_rows, ]
         )$.pred
         
      } else {
         stop("Model object missing both gam_object and final_fit")
      }
      
   }, error = function(e) {
      warning(sprintf("Failed to predict with model lag%d: %s", k, e$message))
      rep(NA_real_, sum(valid_rows))
   })
   
   # Attach predictions
   daily_data_k[[paste0(k, 'DayForecast')]] <- pred
   
   gam_predictions[[lag_name]] <- daily_data_k
   
   # Cleanup
   rm(model_data_k, model_obj, pred, pred_response, transform_info,
      top_vars, group_list_k, valid_rows)
}

# Merge all predictions
all_data <- gam_predictions[[paste0("lag", lead_times[1])]]
for(i in 2:length(lead_times)) {
   k <- lead_times[i]
   lag_name <- paste0("lag", k)
   
   # Select only datetime and the new prediction column
   pred_cols <- gam_predictions[[lag_name]] %>%
      dplyr::select(DateTime, starts_with(paste0(k, 'DayForecast')))
   
   # Join by datetime
   all_data <- all_data %>%
      left_join(pred_cols, by = "DateTime")
}

# Create a summary dataframe of which predictors were used for each k
predictors_summary <- data.frame(
   LeadTime = lead_times,
   Predictors = sapply(paste0("lag", lead_times), function(x) paste(predictors_used[[x]], collapse = ", "))
)

# Create base directory structure
base_dir <- "Outputs/Plots/DailyGAM"
if (!dir.exists(base_dir)) {
   dir.create(base_dir, recursive = TRUE)
}

# Plot GAM validation for each model
for (lag_name in names(models)) {
   
   # Create lag-specific directory
   lag_dir <- file.path(base_dir, lag_name)
   if (!dir.exists(lag_dir)) {
      dir.create(lag_dir, recursive = TRUE)
   }
   
   # Extract the GAM object
   gam_obj <- models[[lag_name]]$gam_object
   
   # Create and save plots in specified order
   
   # 1. ACF
   p_acf <- plot_gam_acf(gam_obj, title = paste(lag_name, "ACF"))
   ggsave(filename = file.path(lag_dir, "acf.png"),
          plot = p_acf,
          width = 8, height = 6, dpi = 600)
   
   # 2. PACF
   p_pacf <- plot_gam_pacf(gam_obj, title = paste(lag_name, "PACF"))
   ggsave(filename = file.path(lag_dir, "pacf.png"),
          plot = p_pacf,
          width = 8, height = 6, dpi = 600)
   
   # 3. Q-Q Plot
   p_qq <- plot_gam_qq(gam_obj, title = paste(lag_name, "Q-Q Plot"))
   ggsave(filename = file.path(lag_dir, "qq.png"),
          plot = p_qq,
          width = 8, height = 6, dpi = 600)
   
   # 4. Residuals vs Fitted
   p_resid <- plot_gam_resid_fitted(gam_obj, title = paste(lag_name, "Residuals vs Fitted"))
   ggsave(filename = file.path(lag_dir, "resid_fitted.png"),
          plot = p_resid,
          width = 8, height = 6, dpi = 600)
   
   # 5. Residual Histogram
   p_hist <- plot_gam_resid_hist(gam_obj, bins = 50, title = paste(lag_name, "Residual Histogram"))
   ggsave(filename = file.path(lag_dir, "resid_hist.png"),
          plot = p_hist,
          width = 8, height = 6, dpi = 600)
   
   # 6. Absolute Residuals
   p_abs_resid <- plot_gam_abs_resid(gam_obj, title = paste(lag_name, "Absolute Residuals"))
   ggsave(filename = file.path(lag_dir, "abs_resid.png"),
          plot = p_abs_resid,
          width = 8, height = 6, dpi = 600)
   
   # 7. Smooths
   p_smooths <- plot_gam_smooths(gam_obj,
                                 title = paste(lag_name, "Smooths"))
   ggsave(filename = file.path(lag_dir, "smooths.png"),
          plot = p_smooths,
          width = 12, height = 8, dpi = 600)
   
   # 8. ALL Prediction vs Observations
   p_obs_vs_pred <- plot_gam_pred_obs(gam_obj, title = paste(lag_name, 'Observed Salinity vs Predicted'))
   ggsave(filename = file.path(lag_dir, "obs_vs_pred.png"),
          plot = p_obs_vs_pred,
          width = 8, height = 6, dpi = 600)
   
   # 9. ALL AND HIGH Prediction vs Observations 
   p_obs_vs_pred_split <- plot_gam_pred_obs_split(gam_obj, threshold = 0.16)
   ggsave(filename = file.path(lag_dir, "obs_vs_pred_split.png"),
          plot = p_obs_vs_pred_split,
          width = 8, height = 6, dpi = 600)
   
   cat("Saved plots for", lag_name, "to", lag_dir, "\n")
}

# Compute Performance Metrics
performance_metrics <- calculate_performance_metrics(
   data = all_data,
   lead_times = lead_times,
   salinity_threshold = 0.16  # The 75th percentile
)

p_rmse <- plot_performance_by_leadtime(performance_metrics, metric = "RMSE", x_label = 'Lead Time (days)', y_label = 'RMSE (psu)')
ggsave(filename = file.path(base_dir, 'RMSE_OverK.png'), plot = p_rmse, width = 12, height = 8, dpi = 600)

p_r2 <- plot_performance_by_leadtime(performance_metrics, metric = "R2", x_label = 'Lead Time (days)', y_label = 'R2 (psu)')
ggsave(filename = file.path(base_dir, 'R2_OverK.png'), plot = p_r2, width = 12, height = 8, dpi = 600)

p_mae <- plot_performance_by_leadtime(performance_metrics, metric = "MAE", x_label = 'Lead Time (days)', y_label = 'MAE (psu)')
ggsave(filename = file.path(base_dir, 'MAE_OverK.png'), plot = p_mae, width = 12, height = 8, dpi = 600)

p_bias <- plot_performance_by_leadtime(performance_metrics, metric = "Bias", x_label = 'Lead Time (days)', y_label = 'Bias (psu)')
ggsave(filename = file.path(base_dir, 'Bias_OverK.png'), plot = p_bias, width = 12, height = 8, dpi = 600)

p_nse <- plot_performance_by_leadtime(performance_metrics, metric = "NSE", x_label = 'Lead Time (days)', y_label = 'NSE (psu)')
ggsave(filename = file.path(base_dir, 'NSE_OverK.png'), plot = p_nse, width = 12, height = 8, dpi = 600)


plot <- plot_salinity_forecast_panels(
   data       = all_data,
   date_range = c('2016-09-15', '2016-11-15'),
   models     = c('10DayForecast'),
   title      = "GAM models forecast the October 2016 event"
)

ggsave("Outputs/Plots/ForecastPlot.svg",
       plot, height = 8, width = 10, device = svglite)

# Clear global environment
rm(list = ls())

# plot_salinity_with_models(
#    data = all_data,
#    date_range = c('2016-09-15', '2016-11-15'),
#    models = c('1DayForecast'),
#    title = "Havre de Grace Salinity"
# )
# 
# 
# 
# create_salinity_predictor_plot(
#    data = all_data,
#    date_range = c('2016-01-01', '2016-12-31'),
#    models = c('14DayForecast', '7DayForecast'),
#    predictors = c('RollingDischarge30', 'TideRange21', 'RollingU30'),
#    highlight_start = as_datetime("2016-10-05"),
#    highlight_end = as_datetime("2016-10-31"),
#    title = "October 2016 Saltwater Intrusion Event"
# )
# 
# create_salinity_predictor_plot(
#    data = all_data,
#    date_range = c('2013-01-01', '2019-12-31'),
#    models = c('14DayForecast', '7DayForecast'),
#    predictors = c('RollingDischarge30', 'TideRange21', 'RollingU30'),
#    title = "October 2016 Saltwater Intrusion Event"
# )
# 

