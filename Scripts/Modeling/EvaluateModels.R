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

elastic <- read_qs_files(hourly_files[12])
gam10 <- read_qs_files(hourly_files[2])
gam_nosal <- read_qs_files(hourly_files[11])

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

plot_salinity_with_models(
   data = hourly_data,
   date_range = c('2016-09-15', '2016-10-31'),
   models = c('ElasticHourly', 'Gam10', 'GamNoSal'),
   highlight_start = as_datetime("2016-10-09"),
   highlight_end = as_datetime("2016-10-25"),
   title = "October 2016 High Salinity Event"
)

plot_salinity_with_models(
   data = daily_data,
   date_range = c('2015-09-01', '2015-10-31'),
   models = c('Gam5', 'Gam6', 'Gam7', 'Gam8', 'Gam9', 'Gam11'),
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



# Get fold level metrics
get_fold_metrics <- function(model_obj, model_name) {
   if (model_obj$model_type %in% c("logistic", "linear")) {
      # Linear models have penalty and mixture
      collect_metrics(model_obj$tune_results, summarize = FALSE) %>%
         filter(penalty == model_obj$best_params$penalty,
                mixture == model_obj$best_params$mixture) %>%
         mutate(model = model_name) %>%
         select(model, id, .metric, .estimate)
   } else if (model_obj$model_type == "rf") {
      # RF has mtry and min_n
      collect_metrics(model_obj$tune_results, summarize = FALSE) %>%
         filter(mtry == model_obj$best_params$mtry,
                min_n == model_obj$best_params$min_n) %>%
         mutate(model = model_name) %>%
         select(model, id, .metric, .estimate)
   }
}

fold_metrics <- bind_rows(
   get_fold_metrics(elastic_linear, "Elastic"),
   get_fold_metrics(lasso_linear, "Lasso"),
   get_fold_metrics(ridge_linear, "Ridge"),
   get_fold_metrics(rf, 'RF'),
   get_fold_metrics(gam, 'GAM')
)

# Get metrics across all folds
cv_summary <- fold_metrics %>%
   group_by(model, .metric) %>%
   summarize(
      mean = mean(.estimate),
      std_err = sd(.estimate) / sqrt(n()),
      min = min(.estimate),
      max = max(.estimate),
      .groups = "drop"
   )

# Get best hyperparameters - separate for linear and RF
best_params_linear <- bind_rows(
   elastic_linear$best_params %>% mutate(model = "Elastic"),
   lasso_linear$best_params %>% mutate(model = "Lasso"),
   ridge_linear$best_params %>% mutate(model = "Ridge")
) %>%
   select(model, penalty, mixture)

best_params_rf <- rf$best_params %>%
   mutate(model = "RF") %>%
   select(model, mtry, min_n)

# Combine into one table (with NAs for non-applicable params)
best_params <- bind_rows(
   best_params_linear %>% mutate(mtry = NA_real_, min_n = NA_real_),
   best_params_rf %>% mutate(penalty = NA_real_, mixture = NA_real_)
) %>%
   select(model, penalty, mixture, mtry, min_n)

# Variable selection info - RF has importance instead of selection
var_selection <- tibble(
   model = c("Elastic", "Lasso", "Ridge", "RF"),
   n_selected = c(
      length(elastic_linear$selected_vars),
      length(lasso_linear$selected_vars),
      length(ridge_linear$selected_vars),
      nrow(rf$var_importance)  
   ),
   selected_vars = list(
      elastic_linear$selected_vars,
      lasso_linear$selected_vars,
      ridge_linear$selected_vars,
      rf$var_importance$Variable  # all variables
   )
)

# Variable importance for RF (top 10)
var_importance_rf <- rf$var_importance %>%
   head(10)

# Get full dataset prediction metrics
insample_metrics <- model_data %>%
   summarize(
      across(c(Elastic, Lasso, Ridge, RF, GAM),
             list(
                rmse = ~sqrt(mean((Salinity - .x)^2)),
                mae = ~mean(abs(Salinity - .x)),
                rsq = ~cor(Salinity, .x)^2
             ))
   ) %>%
   pivot_longer(everything(), 
                names_to = c("pred", "metric"), 
                names_sep = "_(?=[^_]+$)") %>%
   mutate(model = pred) %>%
   select(model, metric, value) %>%
   pivot_wider(names_from = metric, values_from = value)


# Fold Performance
p1 <- plot_fold_performance(fold_metrics, 'rmse')
p2 <- plot_fold_performance(fold_metrics, 'rsq')

# Across-fold performance
p3 <- plot_cv_summary(cv_summary, metric = "rsq")
p4 <- plot_all_metrics_comparison(cv_summary)

# Observed vs predicted
p5 <- plot_obs_pred(model_data)
p6 <- plot_obs_pred(model_data, start_date = "2007-01-01", end_date = "2024-12-31", models = c('GAM_CONS', 'GAM_AGG', 'GAM_EXTREME', 'GAM_SMOOTH', 'Elastic', 'Ridge'))
p7 <- plot_obs_pred(model_data, models = c( 'GamAllVars', 'GamNoTide', 'GamNoTideNoTime', 'GamNoInflows'))

# Plot Time series
p8 <- plot_timeseries(model_data, start_date = "2016-01-01", end_date = "2016-12-31")
p9 <- plot_timeseries(model_data, show_residuals = TRUE)

# Diagnostic plots
diag_plots <- plot_residual_diagnostics(model_data, "GAM")
diag_plots$residuals_vs_fitted
diag_plots$qq_plot





