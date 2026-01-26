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

hourly_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalHourlyDataScreened.qs'))
#daily_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalDailyDataScreened.qs'))

# Tidy hourly data
hourly_data <- hourly_data %>%
   drop_na() %>%
   mutate(LogRollingDischarge24 = log(RollingDischarge24)) %>%
   relocate(LogRollingDischarge24, .after = RollingDischarge24) %>%
   #dplyr::select(-c('RollingDischarge48')) %>%
   mutate(WindSign = factor(RollingV168 >= 0))

# Tidy daily data
# daily_data <- daily_data %>%
#    drop_na() %>%
#    mutate(LogRollingDischarge48 = log(RollingDischarge48)) %>%
#    relocate(LogRollingDischarge48, .after = RollingDischarge48) %>%
#    mutate(WindSign = factor(RollingV168 >= 0)) %>%
#    rename(DateTime = Date) %>%
#    mutate_if(is.numeric, round, digits = 3)


# Discover and load all models (excluding Screening files)
hourly_dir <- 'Outputs/Experiments/Models/HourlyGAM/RawDischarge'
hourly_files <- list.files(c(hourly_dir, 'Outputs/Experiments/Models/Linear'), pattern = "\\.qs$", full.names = TRUE, recursive = TRUE)
hourly_files <- hourly_files[!grepl("Screening\\.qs$", hourly_files)]
hourly_files <- hourly_files[!grepl("Daily\\.qs$", hourly_files)]

# daily_dir <- 'Outputs/Experiments/Models/DailyGAM'
# daily_files <- list.files(c(daily_dir, 'Outputs/Experiments/Models/Linear'), pattern = "\\.qs$", full.names = TRUE, recursive = TRUE)
# daily_files <- daily_files[!grepl("Screening\\.qs$", daily_files)]
# daily_files <- daily_files[!grepl("Hourly\\.qs$", daily_files)]

# elastic <- read_qs_files(hourly_files[12])
# ridge <- read_qs_files(hourly_files[14])
# lasso <- read_qs_files(hourly_files[13])
#gam10 <- read_qs_files(hourly_files[2])
#gam_nosal <- read_qs_files(hourly_files[11])
gam6 <- read_qs_files(hourly_files[7])

# test <- lm(Salinity ~ LagSalinity1, hourly_data)

# Load models and generate predictions
hourly_predictions <- lapply(hourly_files, function(file) {
   model_name <- tools::file_path_sans_ext(basename(file))
   model_obj <- read_qs_files(file)
   
   # Generate prediction based on model structure
   pred <- tryCatch({
      if (!is.null(model_obj$gam_object)) {
         
         # Extract transformation info
         transform_info <- model_obj$transform_info
         family_type <- transform_info$family
         manual_transform <- transform_info$manual_transform
         
         # Predict using type="response" (automatically handles link functions)
         pred_response <- predict(model_obj$gam_object, 
                                  newdata = hourly_data, 
                                  type = "response")
         
         # Back-transform ONLY if Gaussian with manual transformation
         if (family_type == "gaussian" && manual_transform == "log") {
            
            # Back-transform from log scale with bias correction
            sigma_sq <- transform_info$sigma_sq
            pred_original <- exp(pred_response + sigma_sq/2)
            
            # Check for extreme values
            if (any(pred_original > 10, na.rm = TRUE) || 
                any(is.infinite(pred_original))) {
               warning(sprintf("Model %s has extreme/infinite predictions - model may be unstable", 
                               model_name))
            }
            
            pred_original
            
         } else if (family_type == "gaussian" && manual_transform == "sqrt") {
            
            # Back-transform from sqrt scale
            pred_response^2
            
         } else {
            # For Gamma/Tweedie: type="response" already gives original scale
            # For Gaussian with no transform: already on original scale
            pred_response
         }
         
      } else if (!is.null(model_obj$final_fit)) {
         # Fallback to tidymodels structure if gam_object doesn't exist   
         predict(model_obj$final_fit, new_data = hourly_data)$.pred
         
      } else {
         stop("Model object missing both gam_object and final_fit")
      }
      
   }, error = function(e) {
      warning(sprintf("Failed to predict with model %s: %s", model_name, e$message))
      rep(NA_real_, nrow(hourly_data))
   })
   
   setNames(list(pred), model_name)
})

# Add all predictions
hourly_data <- bind_cols(hourly_data, hourly_predictions)
#daily_data <- bind_cols(daily_data, daily_predictions)


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



plot_salinity_with_models(
   data = hourly_data,
   date_range = c('2016-09-15', '2016-10-31'),
   models = c('ElasticHourly'),
   highlight_start = as_datetime("2016-10-09"),
   highlight_end = as_datetime("2016-10-25"),
   title = "October 2016 High Salinity Event"
)

plot_salinity_with_models(
   data = hourly_data,
   date_range = c('2016-10-08', '2016-10-12'),
   models = c('Gam2', 'Gam5', 'Gam10', 'GamNoSal'),
   title = "October 2016 High Salinity Event"
)


create_salinity_predictor_plot(
   data = hourly_data,
   date_range = c('2016-10-05', '2016-10-31'),
   models = c('Gam9'),
   predictors = c('RollingDischarge48', 'LagTide4', 'RollingV168'),
   highlight_start = as_datetime("2016-10-05"),
   highlight_end = as_datetime("2016-10-31"),
   title = "October 2016 Saltwater Intrusion Event"
)
