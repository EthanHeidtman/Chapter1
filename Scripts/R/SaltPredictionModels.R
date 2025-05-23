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
   mutate(DateTime = as_datetime(DateTime)) %>%              # Make dates class datetime
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00'))     # Keep only dates before 
   
# daily: 1.85% more
# monthly: 0.8% less

####################### Prepare all data for modeling ##########################

# Salinity threshold
salinity_threshold = 1.0                                             # practical salt units (PSU), equivalent to parts per thousand

# Create the model data
model_data <- data %>%
   filter(!is.na(Salinity)) %>%                                      # Keep only times with available salinity data
   
   # =======================================================================================
   # PART 1: BASIC DISCHARGE FEATURES (BASED ON THE BEST PERFORMERS)
   # =======================================================================================
   mutate(
      
      # Lagged Conowingo Discharges
      LagDischarge1 = lag(Discharge, 1),
      LagDischarge3 = lag(Discharge, 3),
      LagDischarge6 = lag(Discharge, 6),
      LagDischarge10 = lag(Discharge, 10),
      LagDischarge12 = lag(Discharge, 12),    # BEST PERFORMER
      LagDischarge24 = lag(Discharge, 24),
      LagDischarge36 = lag(Discharge, 36),
      LagDischarge48 = lag(Discharge, 48),
      LagDischarge72 = lag(Discharge, 72),
      
      # Lagged Marietta Inflows (account for residence time and travel)
      LagInflows12 = lag(Inflows, 12),
      LagInflows24 = lag(Inflows, 24),
      LagInflows48 = lag(Inflows, 48),
      LagInflows72 = lag(Inflows, 72),
      
      # Power Law Transformations (-0.4 determined to be best)
      # compared to -0.35 and a log transformation of discharge
      PowDischarge = Discharge ^ (-0.4),
      PowLagDischarge1 = LagDischarge1 ^ (-0.4),
      PowLagDischarge3 = LagDischarge3 ^ (-0.4),
      PowLagDischarge6 = LagDischarge6 ^ (-0.4),
      PowLagDischarge10 = LagDischarge10 ^ (-0.4),
      PowLagDischarge12 = LagDischarge12 ^ (-0.4),    # BEST PERFORMER
      PowLagDischarge24 = LagDischarge24 ^ (-0.4),
      PowLagDischarge36 = LagDischarge36 ^ (-0.4),
      PowLagDischarge48 = LagDischarge48 ^ (-0.4),
      PowLagDischarge72 = LagDischarge72 ^ (-0.4),
      PowInflows = Inflows ^ (-0.4),
      PowLagInflows12 = LagInflows12 ^ (-0.4),
      PowLagInflows24 = LagInflows24 ^ (-0.4),
      PowLagInflows48 = LagInflows48 ^ (-0.4),
      PowLagInflows72 = LagInflows72 ^ (-0.4),
      
      # Rolling Averages (by # of days)
      RollingPowDischarge0.5 = zoo::rollmean(PowDischarge, 24 * 0.5, fill = NA, align = "right"),
      RollingPowDischarge1   = zoo::rollmean(PowDischarge, 24 * 1, fill = NA, align = "right"),
      RollingPowDischarge2   = zoo::rollmean(PowDischarge, 24 * 2, fill = NA, align = "right"),
      RollingPowDischarge4   = zoo::rollmean(PowDischarge, 24 * 4, fill = NA, align = "right"),
      RollingPowDischarge7   = zoo::rollmean(PowDischarge, 24 * 7, fill = NA, align = "right"),
      RollingPowDischarge10  = zoo::rollmean(PowDischarge, 24 * 10, fill = NA, align = "right"),   # BEST PERFORMER
      RollingPowDischarge14  = zoo::rollmean(PowDischarge, 24 * 14, fill = NA, align = "right"),
      RollingPowInflows1     = zoo::rollmean(PowInflows, 24 * 1, fill = NA, align = "right"),
      RollingPowInflows2     = zoo::rollmean(PowInflows, 24 * 2, fill = NA, align = "right"),
      RollingPowInflows7     = zoo::rollmean(PowInflows, 24 * 7, fill = NA, align = "right"),
   ) %>% 
   
   # =======================================================================================
   # PART 2: FLOW-REGIME FEATURES (MARIETTA ≈ NATURAL FLOW CONDITIONS)
   # =======================================================================================
   
   arrange(DateTime) %>%
   mutate(
      
      # Define the natural flow regime
      InflowsPercentile = percent_rank(Inflows),
      NaturalRegime = case_when(
         Marietta_percentile < 0.2 ~ "Stress",   # True hydrologic stress
         Marietta_percentile > 0.8 ~ "Flush",    # High natural flows
         TRUE ~ "Normal"),
      
      # Stress accumulation (exponential decay during non-stress periods)
      stress_accumulation = {
         decay_rate <- 0.95
         stress_acc <- numeric(n())
         stress_acc[1] <- ifelse(NaturalRegime[1] == "Stress", 1, 0)
         
         for(i in 2:length(stress_acc)) {
            if(NaturalRegime[i] == "Stress") {
               stress_acc[i] <- stress_acc[i-1] * decay_rate + 1
            } else if(NaturalRegime[i] == "Flush") {
               stress_acc[i] <- 0
            } else {
               stress_acc[i] <- stress_acc[i-1] * decay_rate
            }
         }
         stress_acc
      },
      
      # Hours since last flushing event
      hours_since_flush = cumsum(ifelse(NaturalRegime == "Flush", 0, 1)),
      
      # Binary indicators
      Stressed = Natural_regime == "Stress",
      Flushed = Natural_regime == "Flush"
      
   ) %>%
   
   # =======================================================================================
   # PART 3: LATENT FLOW FEATURES (CONOWINGO ≠ SUSTAINED FLOW AT MOUTH ON SHORT TIMESCALES)
   # =======================================================================================
   # Key Insight: when the natural (Marietta) flows are less than the FERC requirement, 
   # the dam operators are allowed to release less than FERC.

   mutate(
      
      # Simple Latent Flow
      
      # Regime-Dependent Latent Flow
      
      # Multi-timescale Latent Flow
      
   ) 


# Clean up the model data for normalization
model_data <- model_data %>%
   na.omit() %>%                                            # Remove NAs that arose from calculations
   mutate(SalinitySeason = case_when(
      Month %in% c(3, 4, 12) ~ 'LowSeason',                 # Median salinity 0.10 - 0.11
      Month %in% c(5, 6, 7) ~ 'RisingSeason',               # Median salinity 0.11 - 0.14
      Month %in% c(8, 9, 10, 11) ~ 'HighSeason',            # Median salinity 0.14 - 0.16
   )) %>%
   mutate(SalinitySeason = as.factor(SalinitySeason)) %>%   # Make season factor variable
   relocate(Discharge, 
            Tide, 
            starts_with(c('Lag', 
                          'Pow', 
                          'Rolling')), 
            .after = Salinity) %>%                          # Organize all of the columns
   relocate(FERC, SalinitySeason, .after = Inflows)
   
# Normalize Predictors and Add to model_data
preds_to_normalize <- colnames(model_data)[10: ncol(model_data)]

# Apply the normalization function
normalized_predictors <- normalize_multiple_predictors(model_data, preds_to_normalize)
model_data <- normalized_predictors$data
norm_params <- normalized_predictors$parameters

################ Model Development with Increasing Complexity ######################

### BASIC MODELS: COMPARISON OF TRANSFORMATIONS ###
### Which base transformation performs the best?

## Model 1a: Basic
model1a <- lm(Salinity ~ Norm_Discharge + Norm_Tide, data = model_data)

## Model 1b: Power law, -0.35
model1b <- lm(Salinity ~ Norm_PowDischarge1 + Norm_Tide, data = model_data)

## Model 1c: Power Law, -0.4
model1c <- lm(Salinity ~ Norm_PowDischarge2 + Norm_Tide, data = model_data)        # BEST PERFORMING

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

## Model 2d: 10-hr lag with power law
model2d <- lm(Salinity ~ Norm_PowLagDischarge210 + Norm_Tide, data = model_data)

## Model 2e: 12-hr lag with power law
model2e <- lm(Salinity ~ Norm_PowLagDischarge212 + Norm_Tide, data = model_data) # BEST PERFORMING

## Model 2f: 24-hr lag with power law
model2f <- lm(Salinity ~ Norm_PowLagDischarge224 + Norm_Tide, data = model_data)

## Model 2g: 36-hr lag with power law
model2g <- lm(Salinity ~ Norm_PowLagDischarge236 + Norm_Tide, data = model_data)

models <- list(model2a, model2b, model2c, model2d, model2e, model2f, model2g)
model_names <- c('1hour', '3hour', '6hour', '10hour', '12hour', '24hour', '36hour')

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

### TESTING ROLLING AVERAGES OF DISCHARGE ###
### Which rolling average is the best?
### MODEL3B turns out to be the best

## Model 3a: 12-hr rolling average with power law
model3a <- lm(Salinity ~ Norm_RollingPowDischarge20.5 + Norm_Tide, data = model_data)

## Model 3b: 1 day rolling average with power law
model3b <- lm(Salinity ~ Norm_RollingPowDischarge21 + Norm_Tide, data = model_data)

## Model 3c: 1.5 day rolling average with power law
model3c <- lm(Salinity ~ Norm_RollingPowDischarge21.5 + Norm_Tide, data = model_data)

## Model 3d: 2 day rolling average with power law
model3d <- lm(Salinity ~ Norm_RollingPowDischarge22 + Norm_Tide, data = model_data)

## Model 3e: 3 day rolling average with power law
model3e <- lm(Salinity ~ Norm_RollingPowDischarge23 + Norm_Tide, data = model_data)

## Model 3f: 4 day rolling average with power law
model3f <- lm(Salinity ~ Norm_RollingPowDischarge24 + Norm_Tide, data = model_data)

## Model 3g: 7 day rolling average with power law
model3g <- lm(Salinity ~ Norm_RollingPowDischarge27 + Norm_Tide, data = model_data)

## Model 3h: 10 day rolling average with power law
model3h <- lm(Salinity ~ Norm_RollingPowDischarge210 + Norm_Tide, data = model_data) # BEST PERFORMING

## Model 3i: 14 day rolling average with power law
model3i <- lm(Salinity ~ Norm_RollingPowDischarge214 + Norm_Tide, data = model_data)

models <- list(model3a, model3b, model3c, model3d, model3e, model3f, model3g, model3h, model3i)
model_names <- c('0.5day', '1day', '1.5day', '2day', '3day', '4day', '7day', '10day', '14day')

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


### COMBINED DISCHARGE MODELS ###
### What are the best combinations of discharge predictors?

## Model 4a: Best Raw Discharge Transformation + Best Lag
model4a <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge212 + Norm_Tide,
              data = model_data)

## Model 4b: Best raw discharge transformation + best rolling average
model4b <- lm(Salinity ~ Norm_PowDischarge2 + Norm_RollingPowDischarge210 + Norm_Tide,
              data = model_data)

## Model 4c: Best lag + best rolling average
model4c <- lm(Salinity ~ Norm_PowLagDischarge212 + Norm_RollingPowDischarge210 + Norm_Tide,
              data = model_data)                                                            # BEST PERFORMING

## Model 4d: Best raw discharge + best lag + best rolling average
model4d <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge212 + 
                 Norm_RollingPowDischarge210 + Norm_Tide, data = model_data)

models <- list(model4a, model4b, model4c, model4d)
model_names <- c('Raw+Lag', 'Raw+Rolling', 'Lag+Rolling', 'Raw+Lag+Rolling')

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


### SEASONALITY AND INTERACTIONS ###
### Adding seasonal effects and other interactions to the best from 5: Model5a
### Best of this section:

## Model 6a: Add seasonality
model6a <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + Norm_Tide + 
                 SalinitySeason, data = model_data)

## Model 5b: Add Tide- 3hr Lagged Discharge Interaction
model5b <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + 
                 Norm_Tide + Norm_Tide:PowLagDischarge23 +
                 SalinitySeason, data = model_data)

## Model 5c: Add Tide-12-hr rolling discharge interaction
model5c <- lm(Salinity ~ Norm_PowDischarge2 + Norm_PowLagDischarge23 + 
                 Norm_RollingPowDischarge212 + 
                 Norm_Tide + Norm_Tide:RollingPowDischarge212 +
                 SeasonSalinity, data = model_data)

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


test <- get_predictions(model6a, model_data)
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

