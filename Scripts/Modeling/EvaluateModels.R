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

# Source necessary functions 
source('Scripts/Plots/SimpleModels/SimpleModelEvaluationPlots.R')
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelDataScreened.qs'))
model_data <- model_data %>% drop_na()

# Discover and load all models (excluding Screening files)
model_dir <- 'Outputs/Experiments/Models'
model_files <- list.files(model_dir, pattern = "\\.qs$", full.names = TRUE)
model_files <- model_files[!grepl("Screening\\.qs$", model_files)]

# Load models and generate predictions
predictions <- lapply(model_files, function(file) {
   model_name <- tools::file_path_sans_ext(basename(file))
   model_obj <- read_qs_files(file)
   
   # Generate prediction based on model structure
   pred <- tryCatch({
      if (!is.null(model_obj$gam_object)) {
         # Check if this is a transformed GAM
         if (!is.null(model_obj$transform_info) && 
             model_obj$transform_info$type == "log") {
            
            # Predict on log-scale
            pred_log <- predict(model_obj$gam_object, 
                                newdata = model_data, 
                                type = "response")
            
            # Back-transform with bias correction
            sigma_sq <- model_obj$transform_info$sigma_sq
            pred_original <- exp(pred_log + sigma_sq/2)
            
            # Check for extreme values
            if (any(pred_original > 10, na.rm = TRUE) || 
                any(is.infinite(pred_original))) {
               warning(sprintf("Model %s has extreme/infinite predictions - model is unstable", 
                               model_name))
            }
            
            pred_original
            
         } else if (!is.null(model_obj$transform_info) && 
                    model_obj$transform_info$type == "sqrt") {
            
            # Sqrt transformation
            pred_sqrt <- predict(model_obj$gam_object, 
                                 newdata = model_data, 
                                 type = "response")
            pred_sqrt^2
            
         } else {
            # No transformation
            predict(model_obj$gam_object, 
                    newdata = model_data, 
                    type = "response")
         }
      } else {
         # Use tidymodels final_fit (no transformation handled here)
         predict(model_obj$final_fit, new_data = model_data)$.pred
      }
   }, error = function(e) {
      warning(sprintf("Failed to predict with model %s: %s", model_name, e$message))
      rep(NA, nrow(model_data))
   })
   
   setNames(list(pred), model_name)
})

# Add all predictions to model_data
model_data <- bind_cols(model_data, predictions)

plot_salinity_with_models(
   data = model_data,
   date_range = c('2016-09-15', '2016-10-31'),
   models = c("GamAllVars", 'GamNoTide', 'GamNoTideNoTime', 'GamNoInflows'),
   highlight_start = as_datetime("2016-10-09"),
   highlight_end = as_datetime("2016-10-25"),
   title = "October 2016 High Salinity Event"
)

plot_salinity_with_models(
   data = model_data,
   date_range = c('2007-01-01', '2007-12-31'),
   models = c("GamAllVars", 'GamNoTide', 'GamNoTideNoTime', 'GamNoInflows'),
   title = "October 2016 High Salinity Event"
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





