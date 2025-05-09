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

library(easypackages)
libraries('dplyr', 'lubridate', 'tidyverse', 'ggplot2', 'cmdstanr', 'zoo')

# Read in final hourly data
data <- read.csv('Data/Tidied/HourlyDataFinal.csv', 
                 colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
data <- data %>%
   mutate(DateTime = as_datetime(DateTime)) %>%           # Make dates class datetime
   filter(DateTime < as_datetime('2024-11-01 00:00:00'))  # Keep only dates before 
  
####################### Prepare all data for modeling ##########################

# Susquehanna Morphological Characteristics
d = 9.9 * 1.609 * 1000          # dam's distance from the mouth in meters (~9.9 miles)
depth = 6                       # average depth of the river from the dam to the mouth in meters
width = 1600                    # average width of the river from the dam to the mouth in meters
area = depth * width            # average cross-sectional area of the river below the dam  (m^2)

# Define Salinity threshold
salinity_threshold = 1          # practical salt units (PSU), equivalent to parts per thousand

# Define window-length for rolling mean
rolling_window = 6              # 6 hour rolling window

# Create data for modeling
model_data <- data %>%
   mutate(Exceedance = ifelse(Salinity > salinity_threshold, 1, 0)) %>%         # Create an Exceedance Variable
   filter(!is.na(Salinity)) %>%                                                 # Keep only timestamps with measured salinity
   mutate(Normalized_Discharge = normalize(Discharge),                          # Normalize Conowingo Discharge data
          Normalized_Salinity = normalize(Salinity),                            # Normalize measured HdG salinity data
          Normalized_Tide = normalize(Fitted_HdG),                              # Normalize fitted HdG tide data
          Normalized_Rolling_Discharge = normalize(zoo::rollmean(Discharge, 
                                                            rolling_window, 
                                                            fill = NA,
                                                            align = "right")))  # Compute rolling average of Conowingo discharge and normalize

############# Run Simple Logistic Model to Inform Bayesian Priors ##############
initial_model <- glm(Exceedance ~ Normalized_Discharge + 
                    Normalized_Tide + 
                    Normalized_Salinity + 
                    Normalized_Rolling_Discharge + 
                    Inflows + 
                    FERC, 
                 family = binomial(link = "logit"), data = model_data)

# Summary of coefficients
summary(initial_model)

# Extract coefficients and standard errors
coefs <- coef(initial_model)
se <- sqrt(diag(vcov(initial_model)))

# Inform Bayesian priors
prior_means <- coefs  # Set mean of priors to point estimates
prior_sds <- se * 2   # Set SD of priors to twice the standard error
print(prior_means)
print(prior_sds)





#################### Run the Bayesian Regression with Stan #####################
# Prepare Stan inputs
stan_data <- list(
   N = length(valid_idx),
   y = data$exceedance[valid_idx],
   Discharge = data$Discharge[valid_idx],
   Tide = data$Tide[valid_idx],
   Salinity = data$Salinity[valid_idx],
   Inflows = data$Inflows[valid_idx],       # used for soft prior
   ferc_6hr = data$ferc_6hr[valid_idx],       # soft constraint on 6-hour average
   Discharge_6hr = data$Discharge_6hr[valid_idx],  # known part of total 6-hr discharge
   Salinity_mu = mean(data$Salinity, na.rm = TRUE),
   Salinity_sd = sd(data$Salinity, na.rm = TRUE),
   Tide_mu = mean(data$Tide, na.rm = TRUE),
   Tide_sd = sd(data$Tide, na.rm = TRUE),
   FERC_prior_sd = 300,     # Strength of the FERC prior
   Inflows_prior_sd = 500  # Strength of the Marietta prior, how much we trust it
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




#### Formulate Predictive Relationship as Shortage Objective Function to Minimize ####



##################### Comparison to Old FERC Requirement #######################

