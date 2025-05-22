################################################################################
# Written by Ethan Heidtman, April 2025

# This script uses modeled tidal data, observed discharge data for the Conowingo 
# Dam, and observed/modeled salinity data at Havre de Grace to first develop a 
# predictive relationship for salinity near the Havre de Grace Drinking water
# intake. The predictive relationship is then formulated into an objective function
# that represents a shortage index, the amount of time/probability that the Dam's
# releases are not enough to dilute salt below the safe threshold.


############################ Load Data and Packages ############################

# Source all external functions
lapply(list.files(path = 'Scripts/Functions', pattern = "\\.R$", full.names = TRUE), source)

# Load necessary packages
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cowplot)
library(patchwork)
library(readxl)
library(viridis)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(cmdstanr)
library(svglite)
library(logistf)
library(bayesplot)
library(posterior)

# Read in final hourly data
data <- read.csv('Data/Tidied/HourlyDataFinal.csv', 
                 colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

data <- data %>%
   dplyr::select(-c(9, 10)) %>%                              # Remove extra columns
   mutate(DateTime = as_datetime(DateTime),                  # Make dates class datetime
          Season = case_when(
             Month %in% c(12, 1, 2) ~ "Winter",
             Month %in% c(3, 4, 5) ~ "Spring",
             Month %in% c(6, 7, 8) ~ "Summer",
             Month %in% c(9, 10, 11) ~ "Fall"),
          Season = factor(Season, 
                          levels = c("Winter", 
                                     "Spring", 
                                     "Summer", 
                                     "Fall"))) %>%           # Create a season factor
   relocate(Season, .after = Day) %>%
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00'))     # Keep only dates before 
   
####################### Prepare all data for modeling ##########################

# Salinity threshold
salinity_threshold = 0.8                                             # practical salt units (PSU), equivalent to parts per thousand

model_data <- data %>%
   filter(!is.na(Salinity)) %>%                                      # Keep only times with available salinity data
   mutate(
      ## Lagged Discharges (Hours)
      LagDischarge1 = lag(Discharge, 1),                             # 1 hour lagged discharge
      LagDischarge3 = lag(Discharge, 3),                             # 3 hour lagged discharge
      LagDischarge6 = lag(Discharge, 6),                             # 6 hour lagged discharge
      LagDischarge12 = lag(Discharge, 12),                           # 12 hour lagged discharge
      LagDischarge24 = lag(Discharge, 24),                           # 24 hour lagged discharge
      
      ## Lagged Salinities
      LagSalinity1 = lag(Salinity, 1),                               # 1 hour lagged salinity
      LagSalinity3 = lag(Salinity, 3),                               # 3 hour lagged salinity
      LagSalinity6 = lag(Salinity, 6),                               # 6 hour lagged salinity
      
      ## Power Law Transformations of Discharge 
      ## Kukulka & Jay (2003) suggested exponents around -0.35 to -0.4
      PowDischarge1 = Discharge ^ (-0.35),
      PowDischarge2 = Discharge ^ (-0.4),
      PowLagDischarge21 = LagDischarge1 ^ (-0.4),                    # 1-hr lag, power transformation 2
      PowLagDischarge23 = LagDischarge3 ^ (-0.4),                    # 3-hr lag, power transformation 2
      PowLagDischarge26 = LagDischarge6 ^ (-0.4),                    # 6-hr lag, power transformation 2
      PowLagDischarge212 = LagDischarge12 ^ (-0.4),                  # 12-hr lag, power transformation 2
      PowLagDischarge224 = LagDischarge24 ^ (-0.4),                  # 24-hr lag, power transformation 2
      
      ## Log Transformations of Discharge
      LogDischarge = log(Discharge),                                 # Log of raw hourly discharge
      
      ## Rolling Average Transformations of Discharge
      ## Picked the best transformation: Power law, -0.4
      RollingPowDischarge26 = zoo::rollmean(PowDischarge2, 
                                           6,
                                           fill = NA,
                                           align = "right"),         # 6-hr rolling average of power-transformed discharge
      RollingPowDischarge28 = zoo::rollmean(PowDischarge2, 
                                            8,
                                            fill = NA,
                                            align = "right"),        # 8-hr rolling average of power-transformed discharge
      RollingPowDischarge210 = zoo::rollmean(PowDischarge2, 
                                             10,
                                             fill = NA,
                                             align = "right"),       # 10-hr rolling average of power-transformed discharge
      RollingPowDischarge212 = zoo::rollmean(PowDischarge2, 
                                             12,
                                             fill = NA,
                                             align = "right"),       # 12-hr rolling average of power-transformed discharge
      RollingPowDischarge224 = zoo::rollmean(PowDischarge2, 
                                             24,
                                             fill = NA,
                                             align = "right"),       # 24-hr rolling average of power-transformed discharge
      
      ## Seasonal Cycle
      SeasonSine = sin(2 * pi * Month / 12),
      SeasonCosine = cos(2 * pi * Month / 12),
      
      ## Interaction Terms
      ## Tide/Discharge, Discharge/Season, Tide/Season
      TidePowLagDischarge23 = Tide * PowLagDischarge23,                       # Interaction btwn tide and best lagged discharge
      TideRollingPowDischarge212 = Tide * RollingPowDischarge212,             # Interaction btwn tide and 12-hr rolling best power transformation
      SeasonSinePowLagDischarge23 = SeasonSine * PowLagDischarge23,           # Interaction btwn season and best lagged discharge
      SeasonCosPowLagDischarge23 = SeasonCosine * PowLagDischarge23,          # Interaction btwn season and best lagged discharge
      SeasonSineRollingPowDischarge212 = SeasonSine * RollingPowDischarge212, # Interaction btwn season and 12-hr rolling best power transformation
      SeasonCosRollingPowDischarge212 = SeasonCosine * RollingPowDischarge212 # Interaction btwn season and 12-hr rolling best power transformation
   ) %>%
   na.omit()                                                                  # Remove NAs from these calculations


# Normalize Predictors and Add to model_data
preds_to_normalize <- c('Discharge', 'Tide', 'PowDischarge1', 
                        'PowDischarge2', 'PowLagDischarge21', 
                        'PowLagDischarge23', 'PowLagDischarge26', 'PowLagDischarge212',
                        'PowLagDischarge224',
                        'LogDischarge', 'RollingPowDischarge26', 'RollingPowDischarge28',
                        'RollingPowDischarge210', 'RollingPowDischarge212', 
                        'RollingPowDischarge224', 'TidePowLagDischarge23',
                        'TideRollingPowDischarge212', 'SeasonSinePowLagDischarge23', 
                        'SeasonCosPowLagDischarge23', 'SeasonSineRollingPowDischarge212',
                        'SeasonCosRollingPowDischarge212', 'LagSalinity1', 'LagSalinity3',
                        'LagSalinity6')



# Apply the normalization function
normalized_predictors <- normalize_multiple_predictors(model_data, preds_to_normalize)
model_data <- normalized_predictors$data
norm_params <- normalized_predictors$parameters

# p1 <- ggplot(model_data, aes(x = DateTime, y = Salinity)) + 
#    geom_point() + 
#    scale_x_datetime(limits = c(as_datetime('2016-09-15'), as_datetime('2016-12-05')))
# p2 <- ggplot(model_data, aes(x = DateTime, y = Discharge)) + 
#    geom_point(color = 'red') + 
#    scale_x_datetime(limits = c(as_datetime('2016-09-15'), as_datetime('2016-12-05'))) + 
#    ylim(0, 3000)
# p3 <- ggplot(model_data, aes(x = DateTime, y = Inflows)) + 
#    geom_point(color = 'blue') + 
#    scale_x_datetime(limits = c(as_datetime('2016-09-15'), as_datetime('2016-12-05'))) + 
#    ylim(0, 2000)
# 
# p1 + p2 + p3


################ Model Development with Increasing Complexity ######################

### BASIC MODELS: COMPARISON OF TRANSFORMATIONS ###
### Which base transformation performs the best?

## Model 1a: Basic
model1a <- lm(Salinity ~ Norm_Discharge + Norm_Tide, data = model_data)

## Model 1b: Power law, -0.35
model1b <- lm(Salinity ~ Norm_PowDischarge1 + Norm_Tide, data = model_data)

## Model 1c: Power Law, -0.4
model1c <- lm(Salinity ~ Norm_PowDischarge2 + Norm_Tide, data = model_data)

## Model 1d: Log Transformation
model1d <- lm(Salinity ~ Norm_LogDischarge + Norm_Tide, data = model_data)

models <- list(model1a, model1b, model1c, model1d)
model_names <- c('Basic', 'Power0.35', 'Power0.4', 'Log')

# Evaluate each model
results <- lapply(models, evaluate_model, data = model_data, threshold = salinity_threshold)

# Summarise results in dataframe
results <- data.frame(
   Model = model_names,
   Overall_RMSE = sapply(results, function(x) x$overall_rmse),
   Weighted_RMSE = sapply(results, function(x) x$weighted_rmse),
   Overall_R2 = sapply(results, function(x) x$overall_r2),
   High_Salinity_RMSE = sapply(results, function(x) x$high_salinity_rmse),
   High_Salinity_MAE = sapply(results, function(x) x$high_salinity_mae),
   High_Salinity_Bias = sapply(results, function(x) x$high_salinity_bias),
   High_Salinity_R2 = sapply(results, function(x) x$high_salinity_r2)
)

### TESTING DIFFERENT LAGS ###
### Which lag performs best with the best base transformation (MODEL1C, POWER LAW -0.4)
### The best lag turns out to be the 3-hr lag with the power law: MODEL2e

## Model 2a: 1-hr lag with power law
model2a <- lm(Salinity ~ Norm_PowLagDischarge21 + Norm_Tide, data = model_data)

## Model 2b: 3-hr lag with power law
model2b <- lm(Salinity ~ Norm_PowLagDischarge23 + Norm_Tide, data = model_data)

## Model 2c: 6-hr lag with power law
model2c <- lm(Salinity ~ Norm_PowLagDischarge26 + Norm_Tide, data = model_data)

## Model 2d: 12-hr lag with power law
model2d <- lm(Salinity ~ Norm_PowLagDischarge212 + Norm_Tide, data = model_data)

## Model 2e: 24-hr lag with power law
model2e <- lm(Salinity ~ Norm_PowLagDischarge224 + Norm_Tide, data = model_data)

models <- list(model2a, model2b, model2c, model2d, model2e)
model_names <- c('1hour', '3hour', '6hour', '12hour', '24hour')

# Evaluate each model
results <- lapply(models, evaluate_model, data = model_data, threshold = salinity_threshold)

# Summarise results in dataframe
results <- data.frame(
   Model = model_names,
   Overall_RMSE = sapply(results, function(x) x$overall_rmse),
   Weighted_RMSE = sapply(results, function(x) x$weighted_rmse),
   Overall_R2 = sapply(results, function(x) x$overall_r2),
   High_Salinity_RMSE = sapply(results, function(x) x$high_salinity_rmse),
   High_Salinity_MAE = sapply(results, function(x) x$high_salinity_mae),
   High_Salinity_Bias = sapply(results, function(x) x$high_salinity_bias),
   High_Salinity_R2 = sapply(results, function(x) x$high_salinity_r2)
)

### TESTING DIFFERENT ROLLING AVERAGES ###
### Which rolling average is the best?
### MODEL3B turns out to be the best

## Model 3a: 6-hr rolling average with power law
model3a <- lm(Salinity ~ Norm_RollingPowDischarge26 + Norm_Tide, data = model_data)

## Model 3b: 8-hr rolling average with power law
model3b <- lm(Salinity ~ Norm_RollingPowDischarge28 + Norm_Tide, data = model_data)

## Model 3c: 10-hr rolling average with power law
model3c <- lm(Salinity ~ Norm_RollingPowDischarge210 + Norm_Tide, data = model_data)

## Model 3d: 12-hr rolling average with power law
model3d <- lm(Salinity ~ Norm_RollingPowDischarge212 + Norm_Tide, data = model_data)

## Model 3e: 24-hr rolling average with power law
model3e <- lm(Salinity ~ Norm_RollingPowDischarge224 + Norm_Tide, data = model_data)

models <- list(model3a, model3b, model3c, model3d, model3e)
model_names <- c('6hour', '8hour', '10hour', '12hour', '24hour')

# Evaluate each model
results <- lapply(models, evaluate_model, data = model_data, threshold = salinity_threshold)

# Summarise results in dataframe
results <- data.frame(
   Model = model_names,
   Overall_RMSE = sapply(results, function(x) x$overall_rmse),
   Weighted_RMSE = sapply(results, function(x) x$weighted_rmse),
   Overall_R2 = sapply(results, function(x) x$overall_r2),
   High_Salinity_RMSE = sapply(results, function(x) x$high_salinity_rmse),
   High_Salinity_MAE = sapply(results, function(x) x$high_salinity_mae),
   High_Salinity_Bias = sapply(results, function(x) x$high_salinity_bias),
   High_Salinity_R2 = sapply(results, function(x) x$high_salinity_r2)
)
### COMBINED MODELS ###
### What are the best combinations of predictors?
### Model 4d is the best of these

## Model 4a: Best Raw Discharge Transformation + Best Lag
model4a <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + Norm_Tide,
              data = model_data)

## Model 4b: Best raw discharge transformation + best rolling average
model4b <- lm(Salinity ~ Norm_PowDischarge2 + Norm_RollingPowDischarge212 + Norm_Tide,
              data = model_data)

## Model 4c: Best lag + best rolling average
model4c <- lm(Salinity ~ Norm_PowLagDischarge23 + Norm_RollingPowDischarge212 + Norm_Tide,
              data = model_data)

## Model 4d: Best raw discharge + best lag + best rolling average
model4d <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + Norm_Tide, data = model_data)

### SEASONALITY AND INTERACTIONS ###
### Adding seasonal effects and other interactions to the best from 4: Model4d
### Best of this section:

## Model 5a: Add seasonality
model5a <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + Norm_Tide + 
                 SeasonSine + SeasonCosine, data = model_data)

## Model 5b: Add Tide- 3hr Lagged Discharge Interaction
model5b <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + 
                 Norm_Tide + Norm_TidePowLagDischarge2 +
                 SeasonSine + SeasonCosine, data = model_data)

## Model 5c: Add Tide-12-hr rolling discharge interaction
model5c <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + 
                 Norm_Tide + Norm_TideRollingPowDischarge212 +
                 SeasonSine + SeasonCosine, data = model_data)

### FULL MODEL ###
### Combining all of the best components



## Model 10: One Layer Bayesian Hierarchical Model
model10 <- cmdstan_model('Scripts/Stan/BayesOneLayer.stan')
model10 <- model10$sample(
   data = stan_data,
   seed = 123,
   chains = 4,
   parallel_chains = 4,
   iter_warmup = 1000,
   iter_sampling = 1000
)

## Model 11: Two Layer Bayesian Hierarchical Model
model11 <- cmdstan_model('Scripts/Stan/BayesTwoLayer.stan')
model11 <- model11$sample(
   data = stan_data,
   seed = 123,
   chains = 4,
   parallel_chains = 4,
   iter_warmup = 1000,
   iter_sampling = 1000
)

## Model 12: Three Layer Bayesian Hierarchical Model
model12 <- cmdstan_model('Scripts/Stan/BayesThreeLayer.stan')
model12 <- model12$sample(
   data = stan_data,
   seed = 123,
   chains = 4,
   parallel_chains = 4,
   iter_warmup = 1000,
   iter_sampling = 1000
)

############################### Model Evaluation ###############################

models <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9)
model_names <- c('Basic', 'LogQ', 'LagQ1', 'LagQ3', 'LagQ6', 'RollQ6', 'RollQ12', 'ComboRoll6', 'ComboRoll12')

# Evaluate each model
results <- lapply(models, evaluate_model, data = model_data, threshold = salinity_threshold)

# Summarise results in dataframe
results <- data.frame(
   Model = model_names,
   Overall_RMSE = sapply(results, function(x) x$overall_rmse),
   Weighted_RMSE = sapply(results, function(x) x$weighted_rmse),
   High_Salinity_RMSE = sapply(results, function(x) x$high_salinity_rmse),
   High_Salinity_MAE = sapply(results, function(x) x$high_salinity_mae),
   High_Salinity_Bias = sapply(results, function(x) x$high_salinity_bias),
   High_Salinity_R2 = sapply(results, function(x) x$high_salinity_r2)
)



# ggplot(test, aes(x = date_time)) +
#    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
#    geom_line(aes(y = observed), color = "black") +
#    geom_line(aes(y = predicted), color = "blue") +
#    geom_point(data = subset(test, is_high), aes(y = observed), color = "red", size = 2) +
#    labs(title = "Model Predictions vs Observed Salinity",
#         subtitle = "Red points indicate high salinity events",
#         x = "Date",
#         y = "Salinity (ppt)") +
#    theme_minimal() +
#    theme(axis.text.x = element_text(angle = 45, hjust = 1))


test <- get_predictions(model9, model_data)
high_events <- test %>% 
   filter(is_high) %>% 
   arrange(date_time)

if(nrow(high_events) > 0) {
   # Get a window around the first high event
   first_high_event <- high_events$date_time[1]
   window_start <- first_high_event - days(5)
   window_end <- first_high_event + days(5)
   
   p7 <- ggplot(filter(test, date_time >= window_start & date_time <= window_end), 
                aes(x = date_time)) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
      geom_line(aes(y = observed), color = "black") +
      geom_line(aes(y = predicted), color = "blue") +
      geom_point(data = filter(test, is_high & date_time >= window_start & date_time <= window_end), 
                 aes(y = observed), color = "red", size = 2) +
      labs(title = "10-Day Window Around a High Salinity Event",
           x = "Date",
           y = "Salinity (ppt)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   print(p7)
}
########### Predictive Relationship for Salt with Bayesian Inference ###########







violations <- data %>%
   mutate(Inflow_Violation = ifelse(!is.na(Inflows), Inflows < FERC, NA), 
          Discharge_Violation = ifelse(!is.na(Discharge), Discharge < FERC, NA)) %>%
   summarise(Inflow_Total = sum(!is.na(Inflows)),
             Inflow_Violations = sum(Inflow_Violation, na.rm = TRUE),
             Inflow_ViolationPerc = mean(Inflow_Violation, na.rm = TRUE) * 100, 
             
             Discharge_Total = sum(!is.na(Discharge)),
             Discharge_Violations = sum(Discharge_Violation, na.rm = TRUE),
             Discharge_ViolationPerc = mean(Discharge_Violation, na.rm = TRUE) * 100)











### Hourly Plots
fdc <- data %>%
   pivot_longer(cols = c(Inflows, Discharge, FERC), names_to = 'Location', values_to = 'Flow') %>%
   filter(!is.na(Flow)) %>%
   group_by(Location) %>%
   arrange(desc(Flow)) %>%
   mutate(rank = row_number(),
          exceedance_prob = 100 * rank / n()) %>%
   ungroup()

ggplot(fdc, aes(x = exceedance_prob, y = Flow, color = Location)) +
   geom_line() +
   scale_x_continuous(name = "Exceedance Probability (%)") +
   scale_y_log10(name = "Discharge (log scale)") +
   theme_bw() +
   ggtitle("Flow Duration Curves") +
   theme(legend.title = element_blank()) + 
   scale_color_manual(values = c('Inflows' = 'red', 'Discharge' = 'forestgreen', 'FERC' = 'black'))


ggplot(data, aes(x = DateTime, y = Discharge)) + 
   geom_line(na.rm = TRUE) + 
   geom_line(aes(x = DateTime, y = FERC), color = 'red', na.rm = TRUE) + 
   geom_line(aes(x = DateTime, y = Inflows), color = 'forestgreen', na.rm = TRUE) + 
   scale_x_datetime(limits = c(as_datetime('2023-01-01'), as_datetime('2024-12-31'))) + 
   theme_bw() + 
   ylim(0, 10000) + 
   labs(x = 'DateTime', y = 'Discharge (cubic meters per second)')


ggplot(data) + 
   geom_histogram(aes(x = Inflows, fill = 'Conowingo Inflows'), na.rm = TRUE, alpha = 1, bins = 50) + 
   geom_histogram(aes(x = Discharge, fill = 'Conowingo Discharge'), na.rm = TRUE, alpha = 0.7, bins = 50) + 
   scale_x_log10() +
   theme_bw() + 
   labs(x = 'Discharge (cubic m/s)', y = 'Observation Count', title = 'Histograms of Conowingo and Marietta Flows') + 
   scale_fill_manual(name = 'Location', values = c('Conowingo Inflows' = 'red', 'Conowingo Discharge' = 'forestgreen'))




### Monthly Plots
monthly <- data %>%
   group_by(Year, Month) %>%
   summarise(across(Inflows : FERC, ~ mean(.x, na.rm = TRUE))) %>%
   ungroup() %>%
   mutate_at(vars(Inflows, Discharge, Salinity, CCity, HdG, Fitted_HdG, FERC), ~replace(., is.nan(.), NA)) %>% # Replace NaN with NA
   mutate(Date = as.Date(paste(Year, Month, 15, sep = "-")))

ggplot(monthly) + 
   geom_histogram(aes(x = Inflows, fill = 'Marietta'), na.rm = TRUE, alpha = 1) + 
   geom_histogram(aes(x = Discharge, fill = 'Conowingo'), na.rm = TRUE, alpha = 0.7) + 
   theme_bw() + 
   scale_x_log10() + 
   scale_fill_manual(name = 'Location', values = c('Marietta' = 'red', 'Conowingo' = 'forestgreen')) + 
   labs(x = 'Flow (cubic m/s)', y = 'Count', title = 'Monthly Mean Flows')

monthly_fdc <- monthly %>%
   pivot_longer(cols = c(Inflows, Discharge, FERC), names_to = 'Location', values_to = 'Flow') %>%
   filter(!is.na(Flow)) %>%
   group_by(Location) %>%
   arrange(desc(Flow)) %>%
   mutate(rank = row_number(),
          exceedance_prob = 100 * rank / n()) %>%
   ungroup()

ggplot(monthly_fdc, aes(x = exceedance_prob, y = Flow, color = Location)) +
   geom_line() +
   scale_x_continuous(name = "Exceedance Probability (%)") +
   scale_y_log10(name = "Discharge (log scale)") +
   theme_bw() +
   ggtitle("Monthly Flow Duration Curves") +
   theme(legend.title = element_blank()) + 
   scale_color_manual(values = c('Inflows' = 'red', 'Discharge' = 'forestgreen', 'FERC' = 'black'))

# Susquehanna Morphological Characteristics
d = 9.9 * 1.609 * 1000                   # dam's distance from the mouth in meters (~9.9 miles)
depth = 6                                # average depth of the river from the dam to the mouth in meters
width = 1600                             # average width of the river from the dam to the mouth in meters
area = depth * width                     # average cross-sectional area of the river below the dam  (m^2)


#### Formulate Predictive Relationship as Shortage Objective Function to Minimize ####



##################### Comparison to Old FERC Requirement #######################

