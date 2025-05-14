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
salinity_threshold = 1                   # practical salt units (PSU), equivalent to parts per thousand

# Rolling Average Window Size
rolling_window = 6                       # Hours

model_data <- data %>%
   filter(!is.na(Salinity)) %>%                                   # Keep only times when salinity observations are available
   mutate(RollingDischarge = zoo::rollmean(Discharge, 
                                            rolling_window, 
                                            fill = NA,
                                            align = "right")) %>% # Rolling average discharge 
   mutate(LagDischarge1 = lag(Discharge, 1),
          LagDischarge3 = lag(Discharge, 3),
          LagDischarge6 = lag(Discharge, 6))                     # Lagged Discharge variables (number = # of hours)

# Create Normalized Versions of Data 
# Save original stats for return transformation
Norm_Inflows <- normalize_with_parameters(model_data$Inflows)
Norm_Discharge <- normalize_with_parameters(model_data$Discharge)
Norm_Tide <- normalize_with_parameters(model_data$Tide)
Norm_RollingDischarge <- normalize_with_parameters(model_data$RollingDischarge) 
Norm_LagDischarge1 <- normalize_with_parameters(model_data$LagDischarge1)
Norm_LagDischarge3 <- normalize_with_parameters(model_data$LagDischarge3)
Norm_LagDischarge6 <- normalize_with_parameters(model_data$LagDischarge6)

# Add these normalized data to the model data
model_data <- model_data %>%
   mutate(Norm_Discharge = Norm_Discharge$normalized,
          Norm_Tide = Norm_Tide$normalized,
          Norm_RollingDischarge = Norm_RollingDischarge$normalized,
          Norm_Inflows = Norm_Inflows$normalized,
          Norm_LagDischarge1 = Norm_LagDischarge1$normalized,
          Norm_LagDischarge3 = Norm_LagDischarge3$normalized,
          Norm_LagDischarge6 = Norm_LagDischarge6$normalized) %>%
   mutate(across(where(is.numeric), ~ifelse(is.na(.), 
                                            median(., na.rm=TRUE), .)))         # Deal with NAs (assign median value)

# Collect the normalization parameters so we can return to raw data later for interpretation
norm_params <- list(
   Discharge = list(mean = Norm_Discharge$mean, sd = Norm_Discharge$sd),
   Tide = list(mean = Norm_Tide$mean, sd = Norm_Tide$sd),
   RollingDischarge = list(mean = Norm_RollingDischarge$mean, sd = Norm_RollingDischarge$sd),
   Inflows = list(mean = Norm_Inflows$mean, sd = Norm_Inflows$sd),
   LagDischarge1 = list(mean = Norm_LagDischarge1$mean, sd = Norm_LagDischarge1$sd),
   LagDischarge3 = list(mean = Norm_LagDischarge3$mean, sd = Norm_LagDischarge3$sd),
   LagDischarge6 = list(mean = Norm_LagDischarge6$mean, sd = Norm_LagDischarge6$sd)
)


######################### Increasingly Complex Models ##########################

## Model 1: Basic
model1 <- lm(Salinity ~ Norm_Discharge + Norm_Tide, data = model_data)

## Model 2: Lagged Discharge
model2 <- lm(Salinity ~ Norm_Discharge + Norm_Tide + Norm_LagDischarge1 + 
                Norm_LagDischarge3 + Norm_LagDischarge6, data = model_data)

## Model 3: Rolling Average
model3 <- lm(Salinity ~ Norm_RollingDischarge + Norm_Tide, data = model_data)

## Model 4: Seasonal Effects
model4 <- lm(Salinity ~ Norm_RollingDischarge + Norm_Tide + Season, data = model_data)

## Model 5: Discharge/Tide Interactions
model5 <- lm(Salinity ~ Norm_RollingDischarge * Norm_Tide + Season, data = model_data)

## Model 6: One Layer Bayesian Hierarchical Model
model6 <- cmdstan_model()

## Model 7: Two Layer Bayesian Hierarchical Model
model7 <- cmdstan_model()

## Model 8: Three Layer Bayesian Hierarchical Model
model8 <- cmdstan_model()

############################### Model Evaluation ###############################

models <- list(model1, model2, model3, model4, model5)
model_names <- c('Basic', 'LaggedQ', 'RollingQ', 'Seasons', 'Interactions')

# Evaluate each model
results <- lapply(models, evaluate_model, data = model_data, threshold = salinity_threshold)

# Summarise Results in Dataframe
results <- data.frame(
   Model = model_names,
   Overall_RMSE = sapply(results, function(x) x$overall_rmse),
   Weighted_RMSE = sapply(results, function(x) x$weighted_rmse),
   High_Salinity_RMSE = sapply(results, function(x) x$high_salinity_rmse),
   High_Salinity_MAE = sapply(results, function(x) x$high_salinity_mae),
   High_Salinity_Bias = sapply(results, function(x) x$high_salinity_bias),
   High_Salinity_R2 = sapply(results, function(x) x$high_salinity_r2)
)


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

