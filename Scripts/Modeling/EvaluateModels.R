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
model_data <- model_data %>%
   drop_na

#folds <- make_expanding_folds(model_data, initial_train_length = 5)

# Read in model results 
elastic_linear <- read_qs_files('Outputs/Experiments/Models/ElasticLinearModel.qs')
lasso_linear <- read_qs_files('Outputs/Experiments/Models/LassoLinearModel.qs')
ridge_linear <- read_qs_files('Outputs/Experiments/Models/RidgeLinearModel.qs')
rf <- read_qs_files('Outputs/Experiments/Models/RFModel.qs')
gam <- read_qs_files('Outputs/Experiments/Models/GamModel.qs')

# Predict gam externally to prevent conflict with tidymodels
gam_pred <- predict(gam$gam_object, newdata = model_data, type = "response")

# Use the best model to predict the salinity
model_data <- model_data %>%
   mutate(
      Elastic  = predict(elastic_linear$final_fit, new_data = model_data)$.pred,
      Lasso    = predict(lasso_linear$final_fit, new_data = model_data)$.pred,
      Ridge    = predict(ridge_linear$final_fit, new_data = model_data)$.pred,
      RF       = predict(rf$final_fit, new_data = model_data)$.pred,
      GAM      = gam_pred
   ) 

# Evaluate Oct 2016 specifically
oct_2016 <- model_data %>%
   filter(DateTime >= as.POSIXct("2016-10-01"),
          DateTime <= as.POSIXct("2016-10-31"))

baseline_comparison <- oct_2016 %>%
   summarize(
      across(c(Elastic, Lasso, Ridge, RF, GAM),
             list(
                rmse = ~sqrt(mean((Salinity - .x)^2)),
                peak_error = ~max(Salinity) - max(.x),
                max_salinity = ~max(.x)
             ))
   ) %>%
   pivot_longer(everything(),
                names_to = c("model", "metric"),
                names_sep = "_(?=[^_]+$)") %>%
   pivot_wider(names_from = metric, values_from = value)

oct_2016 %>%
   select(DateTime, Salinity, Lasso, RF, GAM) %>%
   pivot_longer(c(Lasso, GAM, RF), names_to = "model", values_to = "predicted") %>%
   ggplot(aes(x = DateTime)) +
   geom_line(aes(y = Salinity), color = "black", linewidth = 1.2) +
   geom_line(aes(y = predicted, color = model), linewidth = 0.9) +
   geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
   facet_wrap(~model, ncol = 1) +
   labs(
      title = "October 2016 Salinity Event",
      y = "Salinity (psu)"
   ) +
   theme_minimal() +
   theme(legend.position = "none")

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
p6 <- plot_obs_pred(model_data, start_date = "2016-01-01", end_date = "2016-12-31")
p7 <- plot_obs_pred(model_data, models = c("Lasso"))

# Plot Time series
p8 <- plot_timeseries(model_data, start_date = "2016-01-01", end_date = "2016-12-31")
p9 <- plot_timeseries(model_data, show_residuals = TRUE)

# Diagnostic plots
diag_plots <- plot_residual_diagnostics(model_data, "GAM")
diag_plots$residuals_vs_fitted
diag_plots$qq_plot









