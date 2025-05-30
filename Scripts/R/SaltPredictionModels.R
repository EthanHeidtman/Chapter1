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
library(viridis)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(cmdstanr)
library(svglite)
library(bayesplot)
library(posterior)
library(mgcv)
library(mcp)

# Read in final hourly data
data <- read.csv('Data/Tidied/HourlyDataFinal.csv', 
                 colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

data <- data %>%
   dplyr::select(-c(9, 10)) %>%                              # Remove extra columns
   mutate(DateTime = as_datetime(DateTime)) %>%              # Make dates class datetime
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00'))     # Keep only dates before 
   
####################### MODEL DATA PREPARATION PIPELINE ##########################

# Salinity threshold
salinity_threshold = 1.0                                     # practical salt units (PSU), equivalent to parts per thousand

# Create the model data
model_data <- data %>%
   filter(!is.na(Salinity)) %>%                              # Keep only times with available salinity data
   
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

######################### SIMPLE LINEAR MODEL DEVELOPMENT ############################

### TESTING DIFFERENT DISCHARGE LAGS ###
### Which lag transformation performs the best?
### using the power law transformation (Q ^ -0.4)

## Model 1a: 1-hr lag
model1a <- lm(Salinity ~ Norm_PowLagDischarge1 + Norm_Tide, data = model_data)

## Model 1b: 3-hr lag
model1b <- lm(Salinity ~ Norm_PowLagDischarge3 + Norm_Tide, data = model_data)

## Model 1c: 6-hr lag
model1c <- lm(Salinity ~ Norm_PowLagDischarge6 + Norm_Tide, data = model_data)

## Model 1d: 10-hr lag
model1d <- lm(Salinity ~ Norm_PowLagDischarge10 + Norm_Tide, data = model_data)

## Model 1e: 12-hr lag
model1e <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_Tide, data = model_data)

## Model 1f: 36-hr lag
model1f <- lm(Salinity ~ Norm_PowLagDischarge36 + Norm_Tide, data = model_data)

## Model 1g: 48-hr lag
model1g <- lm(Salinity ~ Norm_PowLagDischarge48 + Norm_Tide, data = model_data)

## Model 1h: 72-hr lag
model1h <- lm(Salinity ~ Norm_PowLagDischarge72 + Norm_Tide, data = model_data)

models <- list(model1a, model1b, model1c, model1d, model1e, model1f, model1g, model1h)
model_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('1hr', '3hr', '6hr', '10hr', '12hr', '36hr', '48hr', '72hr')
Models1EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names)

### TESTING DIFFERENT DISCHARGE ROLLING AVERAGES ###
### Which rolling average performs best? Again, using the -0.4 transformation

## Model 2a: 0.5-day rolling average 
model2a <- lm(Salinity ~ Norm_RollingPowDischarge0.5 + Norm_Tide, data = model_data)

## Model 2b: 1-day rolling average 
model2b <- lm(Salinity ~ Norm_RollingPowDischarge1 + Norm_Tide, data = model_data)

## Model 2c: 2-day rolling average 
model2c <- lm(Salinity ~ Norm_RollingPowDischarge2 + Norm_Tide, data = model_data)

## Model 2d: 4-day rolling average 
model2d <- lm(Salinity ~ Norm_RollingPowDischarge4 + Norm_Tide, data = model_data)

## Model 2e: 7-day rolling average 
model2e <- lm(Salinity ~ Norm_RollingPowDischarge7 + Norm_Tide, data = model_data)

## Model 2f: 10-day rolling average 
model2f <- lm(Salinity ~ Norm_RollingPowDischarge10 + Norm_Tide, data = model_data)

## Model 2g: 14-day rolling average 
model2g <- lm(Salinity ~ Norm_RollingPowDischarge14 + Norm_Tide, data = model_data)

models <- list(model2a, model2b, model2c, model2d, model2e, model2f, model2g)
models_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('0.5day', '1day', '2day', '4day', '7day', '10day', '14day')
Models2EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names)


### COMBINED DISCHARGE MODELS ###
### What are the best combinations of discharge predictors?

## Model 3a: Best Raw Discharge Transformation + Best Lag
model3a <- lm(Salinity ~ Norm_PowDischarge + Norm_PowLagDischarge12 + Norm_Tide,
              data = model_data)

## Model 3b: Best raw discharge transformation + best rolling average
model3b <- lm(Salinity ~ Norm_PowDischarge + Norm_RollingPowDischarge10 + 
                 Norm_Tide, data = model_data)

## Model 3c: Best lag + best rolling average BEST PERFORMER
model3c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + Norm_Tide,
              data = model_data)                                                            

## Model 3d: Best raw discharge + best lag + best rolling average
model3d <- lm(Salinity ~ Norm_PowDischarge + Norm_PowLagDischarge12 + 
                 Norm_RollingPowDischarge10 + Norm_Tide, data = model_data)

models <- list(model3a, model3b, model3c, model3d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('Raw+Lag', 'Raw+Rolling', 'Lag+Rolling', 'Raw+Lag+Rolling')
Models3EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names)


### TESTING VALUE OF LAGGED MARIETTA INFLOWS ###
### What lagged inflow time series is the best?
### Building from the best combined discharge model (model3c)

## Model 4a: 12-hr lag of inflows
model4a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_LagInflows12, data = model_data)            

## Model 4b: 24-hr lag of inflows
model4b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_LagInflows24, data = model_data)    

## Model 4c: 48-hr lag of inflows BEST PERFORMER
model4c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_LagInflows48, data = model_data)    

## Model 4d: 72-hr lag of inflows
model4d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_LagInflows72, data = model_data)    

models <- list(model4a, model4b, model4c, model4d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('12hr', '25hr', '48hr', '72hr')
Models4EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names)


### TESTING VALUE OF ROLLING AVERAGE INFLOWS ###
### What is the best performing rolling average inflows?
### Building from the best combined discharge modell (model3c)

## Model 5a: 1-day rolling average of inflows
model5a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows1, data = model_data)            

## Model 5b: 2-day rolling average of inflows BEST PERFORMER
model5b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2, data = model_data)    

## Model 5c: 7-day rolling average of inflows
model5c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows7, data = model_data)    

## Model 5d: 10-day rolling average of inflows
model5d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows10, data = model_data)    

models <- list(model5a, model5b, model5c, model5d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('1day', '2day', '7day', '10day')
Models5EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names)

### TESTING BASIC STRESS (AND SEASON) CLASSIFICATION ###
### What combinations of stress improve performance? and then what does adding seasons do?
### Adding to our best model so far, model5b

## Model 6a: Add moderate stress
model6a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsModerateStress, data = model_data) 

## Model6b: Add high stress BEST PERFORMER FOR STRESSES
model6b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress, data = model_data)    

## Model6c: Add both stresses
model6c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsModerateStress + IsHighStress, data = model_data)  

## Model 6d: Adding seasons to the best model (6b) BEST OVERALL PERFORMER FROM MODELS 6
model6d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason, data = model_data)    

models <- list(model6a, model6b, model6c, model6d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('Moderate', 'High', 'Moderate+High', 'Best+Season')
Models6EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names)


### What additional stress metrics improve the fit?
### Adding to best model from previous, model6d

## Model 7a: Consecutive Stress Hours @ Marietta (Inflows)
model7a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_ConsecutiveStressHours_Marietta, data = model_data)   

## Model 7b: Consecutive Stress Hours @ Conowingo (Discharge)
model7b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_ConsecutiveStressHours_Conowingo, data = model_data)   

## Model 7c: # of Stress Hours in the last 7 days @ Marietta (Inflows)
model7c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_StressHours_7day_Marietta, data = model_data)   

## Model 7d: # of Stress Hours in the last 14 days @ Marietta (Inflows)
model7d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_StressHours_14day_Marietta, data = model_data)   

## Model 7e: # of Stress Hours in the last 30 days @ Marietta (Inflows) BEST PERFORMER
model7e <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_StressHours_30day_Marietta, data = model_data)   

## Model 7f: Cumulative Stress in the last 7 days @ Marietta (Inflows)
model7f <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_CumulativeStress_7day_Marietta, data = model_data)   

## Model 7g: Cumulative Stress in the last 14 days @ Marietta (Inflows)
model7g <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_CumulativeStress_14day_Marietta, data = model_data)   

## Model 7h: Cumulative Stress in the last 30 days @ Marietta (Inflows)
model7h <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_CumulativeStress_30day_Marietta, data = model_data)  

models <- list(model7a, model7b, model7c, model7d, model7e, model7f, model7g, model7h)
model_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('ConsecMarietta', 'ConsecConowingo', 'MarStress7', 'MarStress14', 
                 'MarStress30', 'MarCumStress7', 'MarCumStress14', 'MarCumStress30')
Models7EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names)

### LATENT FLOW INTEGRATION ###
### How do the latent flow classifications improve the model?

## Model 8a: Adding simple latent flow formulation
model8a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason + 
                 Norm_StressHours_30day_Marietta + Norm_SimpleLatent, data = model_data)   

## Model 8b: Adding stress-dependent latent flow formulation
model8b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason + 
                 Norm_StressHours_30day_Marietta + Norm_StressLatent, data = model_data) 

## Model 8c: Adding best latent flow formulation
model8c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason + 
                 Norm_StressHours_30day_Marietta + Norm_BestLatent, data = model_data) 

models <- list(model8a, model8b, model8c)
models_types <- c('linear', 'linear', 'linear')
model_names <- c('SimpleLatent', 'StressLatent', 'BestLatent')
Models8EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names)


############################ GAM AND THRESHOLD-BASED MODELS ######################

# =======================================================================================
# Generalized Additive Models (GAM) to allow for nonlinear relationships among predictors
# =======================================================================================

### Prepare data for the GAM models
model_data <- model_data %>%
   mutate(
      # Log transformation with small constant to handle zeros
      LogSalinity = log(Salinity + 0.001),
      
      # Logit-like transformation (bounded between 0 and max observed)
      MaxSalinity = max(Salinity, na.rm = TRUE),
      BoundedSalinity = pmin(Salinity / MaxSalinity, 0.999),
      LogitSalinity = log(BoundedSalinity / (1 - BoundedSalinity)),
      
      # High salinity indicator
      IsHighSalinity = Salinity > salinity_threshold,
      
      # Interaction terms that might be important for extreme events
      DischargeXTide = Norm_PowLagDischarge12 * Norm_Tide,
      InflowsXStress = Norm_RollingPowInflows2 * as.numeric(IsHighStress)
      
   )

### Part 1: Basic GAM Model ###
gam1 <- gam(
   Salinity ~ 
      # Smoothed terms for the main flow predictors (k is the wiggliness)
      s(Norm_PowLagDischarge12, k = 10) + 
      s(Norm_RollingPowDischarge10, k = 10) +
      s(Norm_RollingPowInflows2, k = 10) +
      s(Norm_Tide, k = 8) + 
      
      # Linear terms for stress and season
      IsHighStress + 
      SalinitySeason +
      Norm_StressHours_30day_Marietta,
   
   data = model_data, 
   method = 'REML', # better for model selection
   family = Gamma(link = "log")  # Better for positive skewed data
)

### Part 2: GAM model with smoothed interaction terms and seasonality
gam2 <- gam(
   Salinity ~ 
      # Main smoothed predictors
      s(Norm_PowLagDischarge12, k = 12) +
      s(Norm_RollingPowInflows2, k = 12) +
      s(Norm_Tide, k = 8) +
      
      # Interaction Smooths (for capturing extreme events)
      s(Norm_PowLagDischarge12, Norm_RollingPowInflows2, k = 15) +
      s(Norm_PowLagDischarge12, Norm_Tide, k = 12) +
   
      # Cyclic smooth for seasonality (time of year)
      s(DayOfYear, bs = "cc", k = 12) +
   
      # Stress effects
      IsHighStress + 
      s(Norm_StressHours_30day_Marietta, k = 8),
   
   data = model_data,
   method = "REML",
   family = Gamma(link = "log")
)

### Part 3: GAM model with regime-dependent smooths
gam3 <- gam(
   Salinity ~ 
      # Regime-dependent smooths using 'by' parameter
      s(Norm_PowLagDischarge12, by = IsHighStress, k = 10) +
      s(Norm_PowLagDischarge12, by = I(1 - IsHighStress), k = 10) +
   
      s(Norm_RollingPowInflows2, by = IsHighStress, k = 10) +
      s(Norm_RollingPowInflows2, by = I(1 - IsHighStress), k = 10) +
   
      # Tide effects
      s(Norm_Tide, k = 8) +
      s(Norm_Tide, by = IsHighStress, k = 6) +
   
      # Seasonal and stress components
      s(DayOfYear, bs = "cc", k = 12) +
      s(Norm_StressHours_30day_Marietta, k = 8) +
   
      # Include latent flow
      s(Norm_BestLatent, k = 10) +
      
      # Base stress effect
      IsHighStress,
   
   data = model_data,
   method = "REML",
   family = Gamma(link = "log"),
   weights = ifelse(data$IsHighSalinity, 3, 1)  # Weight high salinity events more
)

### Part 4: GAM model with heavy weighting toward high-salinity events

# Create weights - much higher for extreme events
weights <- ifelse(data$Salinity > salinity_threshold, 
                  10,  # 10x weight for high salinity
                  1)

# Additional weight for very extreme events
extreme_threshold <- quantile(data$Salinity, 0.95, na.rm = TRUE)
weights <- ifelse(data$Salinity > extreme_threshold, 20, weights)

gam_weighted <- gam(
   Salinity ~ 
      # Main effects with increased complexity for extreme events
      s(Norm_PowLagDischarge12, k = 15) +
      s(Norm_RollingPowInflows2, k = 15) +
      s(Norm_BestLatent, k = 12) +
      
      # Interaction effects crucial for extremes
      s(Norm_PowLagDischarge12, Norm_RollingPowInflows2, k = 20) +
      s(Norm_PowLagDischarge12, Norm_StressHours_30day_Marietta, k = 15) +
      
      # Tide and seasonal effects
      s(Norm_Tide, k = 8) +
      s(DayOfYear, bs = "cc", k = 12) +
      
      # Stress effects
      IsHighStress +
      s(Norm_StressHours_30day_Marietta, k = 10) +
      s(Norm_ConsecutiveStressHours_Marietta, k = 8),
   
   data = model_data,
   method = "REML",
   family = Gamma(link = "log"),
   weights = weights
)


# =======================================================================================
# Threshold-Based or Change-Point Models to improve performance at high salinity
# =======================================================================================

### Threshold Model 1: simple discharge-based threshold

# First, find the optimal discharge threshold
discharge_values <- sort(unique(model_data$Norm_PowLagDischarge12))
discharge_range <- quantile(discharge_values, c(0.1, 0.9), na.rm = TRUE)

# Test thresholds in the middle range
test_thresholds <- seq(discharge_range[1], discharge_range[2], length.out = 20)

best_aic <- Inf
best_threshold <- NULL

for (threshold in test_thresholds) {
   
   # Create regime indicator
   data_temp <- model_data %>%
      mutate(LowDischargeRegime = Norm_PowLagDischarge12 < threshold)
   
   # Fit separate models for each regime
   tryCatch({
      
      model_low <- lm(
         Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowInflows2 + 
            Norm_Tide + IsHighStress + SalinitySeason + 
            Norm_StressHours_30day_Marietta,
         data = filter(data_temp, LowDischargeRegime)
      )
      
      model_high <- lm(
         Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowInflows2 + 
            Norm_Tide + IsHighStress + SalinitySeason + 
            Norm_StressHours_30day_Marietta,
         data = filter(data_temp, !LowDischargeRegime)
      )
      
      # Calculate combined AIC (approximation)
      total_aic <- AIC(model_low) + AIC(model_high)
      
      if (total_aic < best_aic) {
         best_aic <- total_aic
         best_threshold <- threshold
      }
      
   }, error = function(e) {
      # Skip if model fails to fit
   })
}

# Fit final model with best threshold
data_final <- model_data %>%
   mutate(LowDischargeRegime = Norm_PowLagDischarge12 < best_threshold)

model_low_final <- lm(
   Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowInflows2 + 
      Norm_Tide + IsHighStress + SalinitySeason + 
      Norm_StressHours_30day_Marietta,
   data = filter(data_final, LowDischargeRegime)
)

model_high_final <- lm(
   Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowInflows2 + 
      Norm_Tide + IsHighStress + SalinitySeason + 
      Norm_StressHours_30day_Marietta,
   data = filter(data_final, !LowDischargeRegime)
)

# Gather results 
threshold1 <- list(
   low_regime = model_low_final,
   high_regime = model_high_final,
   threshold = best_threshold,
   data = data_final
)


### Threshold Model 2: Stress-based regime switching
model_normal <- lm(
   Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowInflows2 + 
      Norm_Tide + SalinitySeason + Norm_BestLatent,
   data = filter(model_data, !IsHighStress)
)

model_stress <- lm(
   Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowInflows2 + 
      Norm_Tide + SalinitySeason + Norm_StressHours_30day_Marietta + 
      Norm_BestLatent + Norm_ConsecutiveStressHours_Marietta,
   data = filter(model_data, IsHighStress)
)

threshold2 <- list(
   normal_regime = model_normal,
   stress_regime = model_stress,
   data = model_data
)


############################ HIERARCHICAL BAYESIAN MODELS ########################

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












