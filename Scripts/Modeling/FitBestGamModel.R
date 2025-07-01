
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
linear_model_results <- func_env$read_qs_files('Outputs/LinearModeling/LinearModelResults.qs')

# Read in model data 
model_data <- as.data.frame(func_env$read_qs_files('Data/Tidied/Final/FinalModelData.qs'))

###################################### GAM Model Building #############################

# Clean up results from linear modeling
linear_model <- linear_model_results$model
linear_formula <- formula(linear_model)
linear_predictors <- all.vars(linear_formula)[-1]
rm(linear_model_results, linear_model)
environment(linear_formula) <- baseenv() # strip the environment to save ram when parallelizing

# Create minimal data object - only necessary columns to save space
required_cols <- unique(c('DateTime', 'Year', 'Salinity', linear_predictors))
required_cols <- required_cols[required_cols %in% names(model_data)]
gam_data <- model_data[, required_cols, drop = FALSE]


func_env <- new.env()
lapply(list.files("Scripts/Functions", full.names = TRUE, pattern = "\\.R$", recursive = TRUE), function(f) {
   sys.source(f, envir = func_env)
})

test_data <- gam_data %>%
   filter(Year == 2016)

# Basic GAM model building (just 2016)
# basic_gam2016 <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold)
# qsave(basic_gam2016, '~/Desktop/Testing/2016TestBasicGAM.qs')
basic_gam2016 <- qread('~/Desktop/Testing/2016TestBasicGAM.qs')

# Basic GAM model building (all years)
# basic_gam <- func_env$parallel_gam_model_builder(gam_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold)
# qsave(basic_gam, '~/Desktop/Testing/FullTestBasicGAM.qs')
basic_gam <- qread('~/Desktop/Testing/FullTestBasicGAM.qs')

# GAM with Auto-regressive terms of order 1 (just 2016)
ar1_gam2016 <- func_env$parallel_gam_model_builder(test_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 1)
qsave(ar1_gam2016, '~/Desktop/Testing/2016TestAR1.qs')
ar1_gam2016 <-  qread('~/Desktop/Testing/2016TestAR1.qs')

# GAM with Auto-regressive terms of order 1 (all years)
ar1_gam <- func_env$parallel_gam_model_builder(gam_data, linear_formula, linear_predictors, response_var = "Salinity", salinity_threshold, use_ar = TRUE, ar_order = 1)
qsave(ar_gam1, '~/Desktop/Testing/FullTestAR1.qs')
ar1_gam <-  qread('~/Desktop/Testing/FullTestAR1.qs')

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

