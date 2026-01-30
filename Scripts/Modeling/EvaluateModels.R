# =============================================================================
# Script Name:    EvaluateModels.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-11-25
# Last Updated:   2025-11-25
# Description:    Takes the output of ScreenWithRF.R and creates regularized 
#                 statistical models to further screen predictors and predict
#                 salinity exceedance or raw salinity values.
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

# Source necessary functions 
source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Plots/SimpleModels/SimpleModelEvaluationPlots.R')
source('Scripts/Plots/MultiPanelModelPlot.R')

dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Define lead times that were run
lead_times <- c(0, 1, 6, 12, 24, 48, 72, 168, 336, 504)

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
      paste0('Data/Tidied/Final/FinalHourlyDataScreened_lag', k, '.qs')
   )
   
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/RF/RFHourlyScreening_lag', k, '.qs')
   )

}

# Loop through each k to generate predictions and keep all data
for(k in lead_times) {
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k
   hourly_data_k <- screened_data[[lag_name]]
   
   # Define groups for this specific k
   salinity_cluster_k <- hourly_data_k %>% dplyr::select(contains('Salinity'))
   discharge_cluster_k <- hourly_data_k %>% dplyr::select(c('Salinity', contains('Discharge')))
   tide_cluster_k <- hourly_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster_k <- hourly_data_k %>% dplyr::select(c('Salinity', contains(c('U', 'V', 'Gust', 'Wind'))))
   
   # Generate group list
   group_list_k <- list(
      salinity = salinity_cluster_k,
      discharge = discharge_cluster_k,
      tide = tide_cluster_k,
      wind = wind_cluster_k
   )
   
   # Get top variable from each group
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df = rf_results[[lag_name]]$importance,
      group_dfs = group_list_k,
      n_top = list(salinity = 1, discharge = 1, tide = 1, wind = 1),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   # Top variables
   top_vars <- unname(vapply(top_vars_by_k[[lag_name]], function(x) x$Variable, character(1)))
   
   # Store which predictors were used for this k
   predictors_used[[lag_name]] <- top_vars
   
   # Clean data
   hourly_data_k <- hourly_data_k %>%
      drop_na() %>%
      dplyr::select(c(1 : 8, top_vars, contains('Day'))) %>%
      { 
         # If there is a V wind variable → North (-) vs South (+)
         if (any(grepl("V", top_vars))) {
            
            wind_var <- top_vars[grepl("V", top_vars)][1]
            
            mutate(., WindDir = factor(
               ifelse(.data[[wind_var]] < 0, "North", "South")
            ))
            
            # Else if there is a U wind variable → East (-) vs West (+)
         } else if (any(grepl("U", top_vars))) {
            
            wind_var <- top_vars[grepl("U", top_vars)][1]
            
            mutate(., WindDir = factor(
               ifelse(.data[[wind_var]] < 0, "East", "West")
            ))
            
         } else {
            .
         }
      }
   
   # Create a subset with only the predictors used in the model
   model_data_k <- hourly_data_k %>%
      dplyr::select(c(1:8, all_of(top_vars), contains('Day'), 
                      if(any(grepl("[VU]", top_vars))) "WindDir" else NULL))
   
   # Load the GAM model for this k
   gam_file <- paste0('Outputs/Experiments/Models/HourlyGAM/Gam_', k, '.qs')
   model_obj <- read_qs_files(gam_file)
   
   # Store model file in list
   models[[paste0('Lag', k)]] <- model_obj
   
   # Generate predictions
   pred <- tryCatch({
      if (!is.null(model_obj$gam_object)) {
         
         # Extract transformation info
         transform_info <- model_obj$transform_info
         family_type <- transform_info$family
         manual_transform <- transform_info$manual_transform
         
         # Predict using type="response"
         pred_response <- predict(model_obj$gam_object, 
                                  newdata = model_data_k, 
                                  type = "response")
         
         # Back-transform ONLY if Gaussian with manual transformation
         if (family_type == "gaussian" && manual_transform == "log") {
            sigma_sq <- transform_info$sigma_sq
            pred_original <- exp(pred_response + sigma_sq/2)
            
            if (any(pred_original > 10, na.rm = TRUE) || 
                any(is.infinite(pred_original))) {
               warning(sprintf("Model lag%d has extreme/infinite predictions", k))
            }
            pred_original
            
         } else if (family_type == "gaussian" && manual_transform == "sqrt") {
            pred_response^2
            
         } else {
            pred_response
         }
         
      } else if (!is.null(model_obj$final_fit)) {
         predict(model_obj$final_fit, new_data = model_data_k)$.pred
         
      } else {
         stop("Model object missing both gam_object and final_fit")
      }
      
   }, error = function(e) {
      warning(sprintf("Failed to predict with model lag%d: %s", k, e$message))
      rep(NA_real_, nrow(model_data_k))
   })
   
   # Add prediction to the FULL dataset
   hourly_data_k[[paste0("Prediction_lag", k)]] <- pred
   
   # Store in list
   gam_predictions[[lag_name]] <- hourly_data_k
   
   # Clear space
   rm(discharge_cluster_k, salinity_cluster_k, tide_cluster_k, wind_cluster_k,
      model_obj, group_list_k, transform_info, top_vars, pred, pred_response,
      lag_name, manual_transform, family_type, k, hourly_data_k, model_data_k, gam_file)
}

# Merge all predictions
all_data <- gam_predictions[[paste0("lag", lead_times[1])]]
for(i in 2:length(lead_times)) {
   k <- lead_times[i]
   lag_name <- paste0("lag", k)
   
   # Select only datetime and the new prediction column
   pred_cols <- gam_predictions[[lag_name]] %>%
      dplyr::select(DateTime, starts_with(paste0("Prediction_lag", k)))
   
   # Join by datetime
   all_data <- all_data %>%
      left_join(pred_cols, by = "DateTime")
}

# Create a summary dataframe of which predictors were used for each k
predictors_summary <- data.frame(
   LeadTime = lead_times,
   Predictors = sapply(paste0("lag", lead_times), function(x) paste(predictors_used[[x]], collapse = ", "))
)






calculate_performance_metrics <- function(data, lead_times, salinity_threshold = NULL) {
   
   nse <- function(obs, pred) {
      1 - sum((obs - pred)^2, na.rm = TRUE) /
         sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
   }
   
   metrics_list <- list()
   
   for (k in lead_times) {
      pred_col <- paste0("Prediction_lag", k)
      
      if (!pred_col %in% names(data)) {
         warning(sprintf("Prediction column %s not found, skipping", pred_col))
         next
      }
      
      # Overall metrics
      overall <- data %>%
         filter(!is.na(.data[[pred_col]]) & !is.na(Salinity)) %>%
         summarise(
            LeadTime = k,
            Subset = "Overall",
            RMSE = sqrt(mean((Salinity - .data[[pred_col]])^2)),
            MAE  = mean(abs(Salinity - .data[[pred_col]])),
            R2   = cor(Salinity, .data[[pred_col]])^2,
            NSE  = nse(Salinity, .data[[pred_col]]),
            Bias = mean(.data[[pred_col]] - Salinity),
            N    = n()
         )
      
      metrics_list[[paste0("overall_lag", k)]] <- overall
      
      # High salinity metrics
      if (!is.null(salinity_threshold)) {
         
         high_sal <- data %>%
            filter(!is.na(.data[[pred_col]]) & !is.na(Salinity)) %>%
            filter(Salinity >= salinity_threshold) %>%
            summarise(
               LeadTime = k,
               Subset = paste0("Salinity >= ", salinity_threshold),
               RMSE = sqrt(mean((Salinity - .data[[pred_col]])^2)),
               MAE  = mean(abs(Salinity - .data[[pred_col]])),
               R2   = cor(Salinity, .data[[pred_col]])^2,
               NSE  = nse(Salinity, .data[[pred_col]]),
               Bias = mean(.data[[pred_col]] - Salinity),
               N    = n()
            )
         
         metrics_list[[paste0("high_sal_lag", k)]] <- high_sal
      }
   }
   
   bind_rows(metrics_list)
}


performance_metrics <- calculate_performance_metrics(
   data = all_data,
   lead_times = lead_times,
   salinity_threshold = 0.16  # The 75th percentile
)

plot_performance_by_leadtime <- function(metrics_df, 
                                         metric = "RMSE",
                                         title = NULL,
                                         y_label = NULL) {
   
   # Set default labels if not provided
   if (is.null(title)) {
      title <- paste(metric, "Across Lead Times")
   }
   
   if (is.null(y_label)) {
      y_label <- metric
   }
   
   # Create plot
   p <- ggplot(metrics_df, aes(x = LeadTime, y = .data[[metric]], 
                               color = Subset, shape = Subset)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Set1") +
      labs(
         title = title,
         x = "Lead Time (hours)",
         y = y_label,
         color = "Data Subset",
         shape = "Data Subset"
      ) +
      theme_minimal() +
      theme(
         legend.position = "bottom"
      )
   
   return(p)
}

plot_performance_by_leadtime(performance_metrics, metric = "RMSE")
plot_performance_by_leadtime(performance_metrics, metric = "R2")
plot_performance_by_leadtime(performance_metrics, metric = "MAE")
plot_performance_by_leadtime(performance_metrics, metric = "Bias")
plot_performance_by_leadtime(performance_metrics, metric = "NSE")







# resid_gam <- residuals(gam6$gam_object)
# # Plot ACF and PACF
# acf(resid_gam, main = "ACF of GAM Residuals")
# pacf(resid_gam, main = "PACF of GAM Residuals")
# plot(fitted(gam6$gam_object), resid_gam,
#      xlab="Fitted values", ylab="Residuals",
#      main="Residuals vs Fitted")
# abline(h=0, col="red")
# qqnorm(resid_gam); qqline(resid_gam, col="red")
# plot(gam6$gam_object, residuals=TRUE, pch=20, cex=0.3)

# "Prediction_lag0"   "Prediction_lag1"   "Prediction_lag6"   "Prediction_lag12"  "Prediction_lag24"  "Prediction_lag48"  "Prediction_lag72"  "Prediction_lag168" "Prediction_lag336" "Prediction_lag504"

plot_salinity_with_models(
   data = all_data,
   date_range = c('2016-09-15', '2016-10-25'),
   models = c('Prediction_lag1', 'Prediction_lag168'),
   highlight_start = as_datetime("2016-10-09"),
   highlight_end = as_datetime("2016-10-25"),
   title = "October 2016 High Salinity Event"
)



create_salinity_predictor_plot(
   data = hourly_data,
   date_range = c('2016-10-05', '2016-10-31'),
   models = c('Gam6', 'GamNoSal'),
   predictors = c('RollingDischarge48', 'LagTide4', 'RollingV168'),
   highlight_start = as_datetime("2016-10-05"),
   highlight_end = as_datetime("2016-10-31"),
   title = "October 2016 Saltwater Intrusion Event"
)
