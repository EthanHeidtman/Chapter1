# =============================================================================
# Script Name:    TestRandomForest.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-06-01
# Last Updated:   2025-07-17
# Description:    Loads cleaned model data and the list of best linear predictors,
#                 then rudimentary tests a CART model and RF model on the cleaned
#                 data. Some basic plots are produced that show poor performance,
#                 and justify moving to python for the gradient boosting.
# =============================================================================


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
library(rpart)
library(rpart.plot)
library(ranger)
library(yardstick)
library(ggplot2)
library(dplyr)

# Read in linear modeling results
linear_predictor_results <- func_env$read_qs_files('Outputs/Experiments/LinearModeling/LinearPredictors.qs')

# Read in model data 
model_data <- as.data.frame(func_env$read_qs_files('Data/Tidied/Final/CleanFinalModelData.qs'))

# Set salinity threshold
salinity_threshold = 0.3

outcome <- 'Salinity'
predictors <- linear_predictor_results$predictors$all_predictors
fml <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))

# Use time-based split (e.g., last 20% as test set)
n <- nrow(model_data)
train_index <- 1:floor(0.9 * n)
test_index  <- (floor(0.9 * n) + 1):n

train_data <- model_data[train_index, ]
test_data  <- model_data[test_index, ]

# Fit CART
fit_cart <- rpart(fml, data = train_data, method = "anova", control = rpart.control(cp = 0.01))

# Predict
test_data$pred_cart <- predict(fit_cart, newdata = test_data)

# Fit Random Forest
fit_rf <- ranger(
   fml,
   data = train_data,
   num.trees = 300,
   importance = "impurity",
   max.depth = 15,
   min.node.size = 5
)

# Predict
test_data$pred_rf <- predict(fit_rf, data = test_data)$predictions

# CART
cat("CART RMSE:", rmse_vec(test_data[[outcome]], test_data$pred_cart), "\n")
cat("CART R²:", rsq_vec(test_data[[outcome]], test_data$pred_cart), "\n")

# RF
cat("RF RMSE:", rmse_vec(test_data[[outcome]], test_data$pred_rf), "\n")
cat("RF R²:", rsq_vec(test_data[[outcome]], test_data$pred_rf), "\n")

rpart.plot(fit_cart, digits = 3, fallen.leaves = TRUE)

ggplot(test_data) +
   geom_point(aes(x = !!sym(outcome), y = pred_cart), color = "blue", alpha = 0.4) +
   geom_point(aes(x = !!sym(outcome), y = pred_rf), color = "green", alpha = 0.4) +
   geom_abline(linetype = "dashed") +
   labs(
      title = "Observed vs Predicted Salinity",
      subtitle = "Blue = CART, Green = RF",
      x = "Observed Salinity", y = "Predicted Salinity"
   )

ggplot(test_data) +
   geom_point(aes(x = !!sym(outcome), y = pred_cart - !!sym(outcome)), color = "blue", alpha = 0.4) +
   geom_point(aes(x = !!sym(outcome), y = pred_rf - !!sym(outcome)), color = "green", alpha = 0.4) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(
      title = "Residuals vs Observed Salinity",
      subtitle = "Blue = CART, Green = RF",
      y = "Residual (Predicted - Actual)", x = "Observed Salinity"
   )

residuals_df <- tibble(
   residual_cart = test_data$pred_cart - test_data[[outcome]],
   residual_rf = test_data$pred_rf - test_data[[outcome]]
) %>%
   tidyr::pivot_longer(cols = everything(), names_to = "model", values_to = "residual")

ggplot(residuals_df, aes(x = residual, fill = model)) +
   geom_density(alpha = 0.4) +
   labs(title = "Density of Residuals: CART vs RF", x = "Residual", y = "Density") +
   theme_minimal()

pred_obs_df <- test_data %>%
   select(!!sym(outcome), pred_cart, pred_rf) %>%
   rename(observed = !!sym(outcome)) %>%
   tidyr::pivot_longer(cols = -observed, names_to = "model", values_to = "prediction")

ggplot() +
   geom_histogram(data = pred_obs_df, aes(x = prediction, fill = model), alpha = 0.4, bins = 50, position = "identity") +
   geom_histogram(data = test_data, aes(x = !!sym(outcome)), fill = "black", alpha = 0.2, bins = 50) +
   labs(title = "Histogram of Observed and Predicted Salinity", x = "Salinity", y = "Count") +
   theme_minimal()

ggplot(test_data, aes(x = 1:nrow(test_data))) +
   geom_line(aes(y = pred_cart - !!sym(outcome), color = "CART Residual")) +
   geom_line(aes(y = pred_rf - !!sym(outcome), color = "RF Residual")) +
   labs(title = "Residuals Over Time", x = "Time Index", y = "Residual") +
   theme_minimal() +
   scale_color_manual(values = c("blue", "green"))

var_imp <- as.data.frame(fit_rf$variable.importance)
var_imp$Variable <- rownames(var_imp)

ggplot(var_imp, aes(x = reorder(Variable, fit_rf$variable.importance), y = fit_rf$variable.importance)) +
   geom_col(fill = "darkgreen") +
   coord_flip() +
   labs(title = "Random Forest Variable Importance", x = "Variable", y = "Importance")




