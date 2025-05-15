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
salinity_threshold = 1                                              # practical salt units (PSU), equivalent to parts per thousand


model_data <- data %>%
   filter(!is.na(Salinity)) %>%                                     # Keep only times when salinity observations are available
   mutate(RollingDischarge6 = zoo::rollmean(Discharge, 
                                            6, 
                                            fill = NA,
                                            align = "right"),
          RollingDischarge12 = zoo::rollmean(Discharge,
                                              12, 
                                              fill = NA,
                                              align = 'right')) %>% # Rolling average discharge 
   mutate(LagDischarge1 = lag(Discharge, 1),
          LagDischarge3 = lag(Discharge, 3),
          LagDischarge6 = lag(Discharge, 6)) %>%                    # Lagged Discharge variables (number = # of hours)
   mutate(LogDischarge = round(log10(Discharge), digits = 4))

# Create Normalized Versions of Data 
# Save original stats for return transformation
Norm_Inflows <- normalize_with_parameters(model_data$Inflows)
Norm_Discharge <- normalize_with_parameters(model_data$Discharge)
Norm_Tide <- normalize_with_parameters(model_data$Tide)
Norm_RollingDischarge6 <- normalize_with_parameters(model_data$RollingDischarge6) 
Norm_RollingDischarge12 <- normalize_with_parameters(model_data$RollingDischarge12) 
Norm_LagDischarge1 <- normalize_with_parameters(model_data$LagDischarge1)
Norm_LagDischarge3 <- normalize_with_parameters(model_data$LagDischarge3)
Norm_LagDischarge6 <- normalize_with_parameters(model_data$LagDischarge6)
Norm_LogDischarge <- normalize_with_parameters(model_data$LogDischarge)

# Add these normalized data to the model data
model_data <- model_data %>%
   mutate(Norm_Discharge = Norm_Discharge$normalized,
          Norm_Tide = Norm_Tide$normalized,
          Norm_RollingDischarge6 = Norm_RollingDischarge6$normalized,
          Norm_RollingDischarge12 = Norm_RollingDischarge12$normalized,
          Norm_Inflows = Norm_Inflows$normalized,
          Norm_LagDischarge1 = Norm_LagDischarge1$normalized,
          Norm_LagDischarge3 = Norm_LagDischarge3$normalized,
          Norm_LagDischarge6 = Norm_LagDischarge6$normalized,
          Norm_LogDischarge = Norm_LogDischarge$normalized) %>%
   mutate(across(where(is.numeric), ~ifelse(is.na(.), 
                                            median(., na.rm=TRUE), .)))         # Deal with NAs (assign median value)

# Collect the normalization parameters so we can return to raw data later for interpretation
norm_params <- list(
   Discharge = list(mean = Norm_Discharge$mean, sd = Norm_Discharge$sd),
   Tide = list(mean = Norm_Tide$mean, sd = Norm_Tide$sd),
   RollingDischarge6 = list(mean = Norm_RollingDischarge6$mean, sd = Norm_RollingDischarge6$sd),
   RollingDischarge12 = list(mean = Norm_RollingDischarge12$mean, sd = Norm_RollingDischarge12$sd),
   Inflows = list(mean = Norm_Inflows$mean, sd = Norm_Inflows$sd),
   LagDischarge1 = list(mean = Norm_LagDischarge1$mean, sd = Norm_LagDischarge1$sd),
   LagDischarge3 = list(mean = Norm_LagDischarge3$mean, sd = Norm_LagDischarge3$sd),
   LagDischarge6 = list(mean = Norm_LagDischarge6$mean, sd = Norm_LagDischarge6$sd),
   LogDischarge = list(mean = Norm_LogDischarge$mean, sd = Norm_LogDischarge$sd)
)


######################### Increasingly Complex Models ##########################

## Model 1: Basic
model1 <- lm(Salinity ~ Norm_Discharge + Norm_Tide, data = model_data)

## Model 2: Log of Discharge
model2 <- lm(Salinity ~ Norm_LogDischarge + Norm_Tide, data = model_data)

## Model 3: Lagged Discharge (1hr)
model3 <- lm(Salinity ~ Norm_LagDischarge1 + Norm_Tide, data = model_data)

## Model 4: Lagged Discharge (3 hr)
model4 <- lm(Salinity ~ Norm_LagDischarge3 + Norm_Tide, data = model_data)

## Model 5: Lagged Discharge (6 hr)
model5 <- lm(Salinity ~ Norm_LagDischarge6 + Tide, data = model_data)

## Model 6: Rolling Average (6 hr)
model6 <- lm(Salinity ~ Norm_RollingDischarge6 + Norm_Tide, data = model_data)

## Model 7: Rolling Average (12 hr) 
model7 <- lm(Salinity ~ Norm_RollingDischarge12 + Tide, data = model_data)

## Model 8: Combined Effects (Raw + Rolling6)
model8 <- lm(Salinity ~ Norm_Discharge + Norm_RollingDischarge6 + Tide, data = model_data)

## Model 9:Combined Effects (Raw + Rolling12)
model9 <- lm(Salinity ~ Norm_Discharge + Norm_RollingDischarge12 + Tide, data = model_data)

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

