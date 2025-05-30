################################################################################
# Written by Ethan Heidtman, April 2025

# This script uses modeled tidal data, observed discharge data for the Conowingo 
# Dam, and observed/modeled salinity data at Havre de Grace to first develop a 
# predictive relationship for salinity near the Havre de Grace Drinking water
# intake. The predictive relationship is then formulated into an objective function
# that represents a shortage index, the amount of time/probability that the Dam's
# releases are not enough to dilute salt below the safe threshold.


############################ LOAD FUNCTIONS, PACKAGES, AND DATA ############################

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
#library(logistf)
library(bayesplot)
library(posterior)
library(mgcv)

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

####################### MODEL DATA PREPARATION PIPELINE ##########################

# Salinity threshold
salinity_threshold = 0.5                                             # practical salt units (PSU), equivalent to parts per thousand

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
      LagDischarge12 = lag(Discharge, 12),
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
      PowLagInflows48 = LagInflows48 ^ (-0.4),        # BEST PERFORMER
      PowLagInflows72 = LagInflows72 ^ (-0.4),
      
      # Rolling Averages (by # of days)
      RollingPowDischarge0.5 = zoo::rollmean(PowDischarge, 24 * 0.5, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge1   = zoo::rollmean(PowDischarge, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge2   = zoo::rollmean(PowDischarge, 24 * 2, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge4   = zoo::rollmean(PowDischarge, 24 * 4, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge7   = zoo::rollmean(PowDischarge, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge10  = zoo::rollmean(PowDischarge, 24 * 10, fill = NA, align = "right", na.rm = TRUE),   # BEST PERFORMER
      RollingPowDischarge14  = zoo::rollmean(PowDischarge, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
      RollingPowInflows1     = zoo::rollmean(PowInflows, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
      RollingPowInflows2     = zoo::rollmean(PowInflows, 24 * 2, fill = NA, align = "right", na.rm = TRUE),      # BEST PERFORMER
      RollingPowInflows7     = zoo::rollmean(PowInflows, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
      RollingPowInflows10    = zoo::rollmean(PowInflows, 24 * 10, fill = NA, align = "right", na.rm = TRUE)
   ) %>% 
   
   # =======================================================================================
   # PART 2: BASIC FLOW-REGIME FEATURES (MARIETTA ≈ NATURAL FLOW CONDITIONS)
   # =======================================================================================
   
   arrange(DateTime) %>%
   mutate(
      
      # Define the natural flow regime
      InflowsPercentile = percent_rank(Inflows),
      BasicRegime = case_when(
         InflowsPercentile < 0.2 ~ "Low",   # True hydrologic stress
         InflowsPercentile > 0.8 ~ "High",    # High natural flows
         TRUE ~ "Normal"),
      
   ) %>%
   
   # =======================================================================================
   # PART 3: COMPREHENSIVE STRESS-CLASSIFICATION SYSTEM
   # =======================================================================================

   mutate(
      
      # Define stress-thresholds
      MariettaStressThreshold = quantile(Inflows, 0.2, na.rm = TRUE),
      ConowingoStressThreshold = quantile(Discharge, 0.2, na.rm = TRUE),
      
      # Binary Stress Indicators
      MariettaStressed = Inflows < MariettaStressThreshold,
      ConowingoStressed = Discharge < ConowingoStressThreshold,
      BelowFERC = Discharge < FERC,
      
      # Stress Intensity
      MariettaStressIntensity = pmax(0, (MariettaStressThreshold - Inflows) / MariettaStressThreshold),
      ConowingoStressIntensity = pmax(0, (ConowingoStressThreshold - Discharge) / ConowingoStressThreshold),
      FERCStressIntensity = pmax(0, (FERC - Discharge) / FERC)
      
   ) %>%
   
   # Calculate running stress-accumulation metrics
   mutate(
      
      # Consecutive hours of stress (reset when stress ends)
      ConsecutiveStressHours_Marietta = sequence(rle(MariettaStressed)$lengths) * MariettaStressed,
      ConsecutiveStressHours_Conowingo = sequence(rle(ConowingoStressed)$lengths) * ConowingoStressed,
      ConsecutiveBelowFERC = sequence(rle(BelowFERC)$lengths) * BelowFERC,
      
      # Rolling sum of stress hours over different windows
      StressHours_7day_Marietta = zoo::rollsum(as.numeric(MariettaStressed), 24 * 7, fill = NA, align = "right", na.rm = TRUE),
      StressHours_14day_Marietta = zoo::rollsum(as.numeric(MariettaStressed), 24 * 14, fill = NA, align = "right", na.rm = TRUE),
      StressHours_30day_Marietta = zoo::rollsum(as.numeric(MariettaStressed), 24 * 30, fill = NA, align = "right", na.rm = TRUE),
      
      StressHours_7day_Conowingo = zoo::rollsum(as.numeric(ConowingoStressed), 24 * 7, fill = NA, align = "right" , na.rm = TRUE),
      StressHours_14day_Conowingo = zoo::rollsum(as.numeric(ConowingoStressed), 24 * 14, fill = NA, align = "right", na.rm = TRUE),
      StressHours_30day_Conowingo = zoo::rollsum(as.numeric(ConowingoStressed), 24 * 30, fill = NA, align = "right", na.rm = TRUE),
      
      # Cumulative stress intensity over time windows
      CumulativeStress_7day_Marietta = zoo::rollsum(MariettaStressIntensity, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
      CumulativeStress_14day_Marietta = zoo::rollsum(MariettaStressIntensity, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
      CumulativeStress_30day_Marietta = zoo::rollsum(MariettaStressIntensity, 24 * 30, fill = NA, align = "right", na.rm = TRUE),
      
      # Days since last major flow event (flow > 80th percentile)
      HighFlowThreshold = quantile(Inflows, 0.8, na.rm = TRUE),
      IsHighFlow = Inflows > HighFlowThreshold,
      DaysSinceHighFlow = NA_real_
      
   ) %>%
   
   # Calculate days since high flow (requires a loop-like operation)
   group_by(1) %>%  # Dummy grouping to ensure proper ordering
   mutate(
      # This creates a counter that resets every time there's a high flow event
      HighFlowGroupID = cumsum(IsHighFlow),
      DaysSinceHighFlow = if_else(IsHighFlow, 0, 
                                  (row_number() - max(row_number()[IsHighFlow & HighFlowGroupID == max(HighFlowGroupID[IsHighFlow])])) / 24)
   ) %>%
   ungroup() %>%
   select(-HighFlowGroupID) %>%  # Remove temporary variable
   
   # =======================================================================================
   # STRESS-BASED FLOW REGIME CLASSIFICATION (COMPREHENSIVE, BASED ON MULTIPLE INDICATORS)
   # =======================================================================================
   
   mutate(
      
      StressLevel = case_when(
         
         # # CRITICAL STRESS: High cumulative stress & long consecutive stress & long since last high flow
         # (CumulativeStress_14day_Marietta > quantile(CumulativeStress_14day_Marietta, 0.75, na.rm = TRUE)) &
         # (ConsecutiveStressHours_Marietta > 24 * 2) &  # 2+ days consecutive
         # (DaysSinceHighFlow > 10) ~ "Critical",
         
         # HIGH STRESS: Moderate cumulative stress and some duration
         (CumulativeStress_7day_Marietta > quantile(CumulativeStress_7day_Marietta, 0.7, na.rm = TRUE)) &
         (ConsecutiveStressHours_Marietta > 24 * 1) ~ "High", # 1+ day consecutive
         
         # MODERATE STRESS: Some stress indicators are present
         (CumulativeStress_7day_Marietta > quantile(CumulativeStress_7day_Marietta, 0.5, na.rm = TRUE)) |
         (ConsecutiveStressHours_Marietta > 6) |  # 6+ hours consecutive
         (StressHours_7day_Marietta > 24 * 2) ~ "Moderate",  # 2+ days in past week
         
         # NO STRESS: Flush period, recent high flow
         (DaysSinceHighFlow <= 2) | 
         (BasicRegime == "High") ~ "Flush",
         
         # NORMAL CONDITIONS: Everything else
         TRUE ~ 'Normal'
         
      ),
      
      # Binary indicators for model use
      # IsCriticalStress = StressLevel == "Critical",
      IsHighStress = StressLevel %in% c("Critical", "High"),
      IsModerateStress = StressLevel %in% c("Critical", "High", "Moderate"),
      IsFlush = StressLevel == "Flush",
      IsStressed = StressLevel %in% c("Critical", "High", "Moderate")  # Any stress
      
   ) %>%
   
   # =======================================================================================
   # PART 4: LATENT FLOW FEATURES (CONOWINGO ≠ SUSTAINED FLOW AT MOUTH ON SHORT TIMESCALES)
   # =======================================================================================
   # Key Insight: when the natural (Marietta) flows are less than the FERC requirement, 
   # the dam operators are allowed to release less than FERC.
   # We need to estimate the true sustained flow at the mouth

   mutate(
      
      # First check what the flow discrepancies look like
      FlowDiscrepancy = abs(Discharge - LagInflows48),
      HighThreshold = quantile(FlowDiscrepancy, 0.8, na.rm = TRUE),
      MedianThreshold = quantile(FlowDiscrepancy, 0.5, na.rm = TRUE),
      
      # ====== Simple Latent Flow (using the best lagged Marietta Inflows) ====== #
      SimpleLatent = case_when(
         # Large discrepancy, weight toward Marietta inflows
         FlowDiscrepancy > HighThreshold ~ 0.3 * PowDischarge + 0.7 * PowLagInflows48, 
         
         # Median Discrepancy, weight more evenly
         FlowDiscrepancy > MedianThreshold ~ 0.5 * PowDischarge + 0.5 * PowLagInflows48, 
         
         # Normal Operations
         TRUE ~ 0.7 * PowDischarge + 0.3 * PowLagInflows48
      ),
      
      # ====== Stress-Dependent Latent Flow (based on previous section) ======== #
      StressLatent = case_when(
         
         # # CRITICAL STRESS: the system is primed for saltwater intrusion, so we emphasize sustained natural flows
         # StressLevel == "Critical" ~ 
         #    pmin(0.15 * RollingPowDischarge10 + 0.85 * RollingPowInflows2,  # Heavily natural
         #         0.25 * PowLagDischarge12 + 0.75 * PowLagInflows48),        # Long-lag natural
         
         #  HIGH STRESS: moderately emphasize natural flows
         StressLevel == "High" ~ 
            0.3 * RollingPowDischarge10 + 0.7 * RollingPowInflows2,
         
         # MODERATE STRESS: slight preference for natural flows
         StressLevel == "Moderate" ~ 
            0.4 * PowLagDischarge12 + 0.6 * PowLagInflows48,
         
         # NO STRESS: flush period, operations are dominant
         StressLevel == "Flush" ~ 
            0.85 * PowLagDischarge12 + 0.15 * PowLagInflows48,
         
         # NORMAL: standard balanced weighting
         TRUE ~ 0.6 * PowLagDischarge12 + 0.4 * PowLagInflows48
         
      ),
      
      BestLatent = case_when(
         
         IsHighStress ~ 0.3 * PowLagDischarge12 + 0.7 * RollingPowInflows2,  # Best lag + best rolling
         IsFlush ~ 0.8 * PowLagDischarge12 + 0.2 * RollingPowInflows2,       # Operational emphasis
         TRUE ~ 0.6 * PowLagDischarge12 + 0.4 * RollingPowDischarge10        # Best performers
         
      )
   ) 
  

# Clean up the model data for normalization
model_data <- model_data %>%
     # Remove intermediate calculation variables to clean up
   select(-FlowDiscrepancy, -FlowDiscrepancy, -HighThreshold, 
          -MedianThreshold, -MariettaStressThreshold, -FlowDiscrepancy, -`1`,
          -ConowingoStressThreshold, -HighFlowThreshold, -IsHighFlow, -InflowsPercentile) %>%
   # na.omit() %>%                                            # Remove NAs that arose from calculations
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
            contains(c('Threshold', 
                     'Stress', 
                     'Since', 
                     'Consecutive', 
                     'Latent', 
                     'Is')),
            .after = Salinity) %>%                          # Organize all of the columns
   relocate(FERC, SalinitySeason, where(is.logical), 
            where(is.character), contains('Threshold'), .after = Inflows)
   
# Normalize Predictors and Add to model_data
preds_to_normalize <- colnames(model_data)[19 : ncol(model_data)] # Starting from the discharge column

# Apply the normalization function
normalized_predictors <- normalize_multiple_predictors(model_data, preds_to_normalize)
model_data <- normalized_predictors$data
norm_params <- normalized_predictors$parameters

################ MODEL DEVELOPMENT ######################

# Basic GAM: Smooths for key flow predictors, linear for stress and seasonal effects
model_gam1 <- gam(Salinity ~ 
                     s(Norm_PowLagDischarge12) + 
                     s(Norm_RollingPowInflows2) + 
                     Norm_Tide + 
                     IsHighStress + 
                     SalinitySeason,
                  data = model_data)

model_gam2 <- gam(Salinity ~ 
                     s(Norm_PowLagDischarge12, by=as.numeric(IsHighStress)) + 
                     s(Norm_RollingPowInflows2, by=as.numeric(IsHighStress)) + 
                     Norm_Tide + 
                     IsHighStress + 
                     SalinitySeason,
                  data = model_data)

model_gam3 <- gam(Salinity ~ 
                     s(Norm_PowLagDischarge12) + 
                     s(Norm_RollingPowInflows2) + 
                     s(Norm_StressHours_30day_Marietta) + 
                     Norm_Tide + 
                     IsHighStress + 
                     SalinitySeason,
                  data = model_data)
par(mfrow=c(1,2))
plot(model_gam3, residuals=TRUE, pch=19, cex=0.5)

models <- list(model_gam1, model_gam2, model_gam3)
model_names <- c('Spline1', 'Spline2', 'Spline3')
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

############################### MODEL EVALUATION ###############################

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

