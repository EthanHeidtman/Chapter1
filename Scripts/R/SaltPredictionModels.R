################################################################################
# Written by Ethan Heidtman, April 2025

# This script uses modeled tidal data, observed discharge data for the Conowingo 
# Dam, and observed/modeled salinity data at Havre de Grace to first develop a 
# predictive relationship for salinity near the Havre de Grace Drinking water
# intake. The predictive relationship is then formulated into an objective function
# that represents a shortage index, the amount of time/probability that the Dam's
# releases are not enough to dilute salt below the safe threshold.


############################ LOAD FUNCTIONS, PACKAGES, AND DATA ############################

# Source all necessary functions, searching subdirectories as well
lapply(list.files(path = 'Scripts/Functions', pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source)

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
   # PART 0: BASIC TIDE FEATURES
   # =======================================================================================
   
   mutate(
      # Lagged Tide Features
      LagTide1 = lag(Tide, 1),
      LagTide2 = lag(Tide, 2),
      LagTide4 = lag(Tide, 4),
      
      # Tide Rate Features
      TideRate = c(NA, diff(Tide) / as.numeric(diff(DateTime), units = "hours")),
      TideRate = zoo::rollmean(TideRate, k = 3, fill = NA, align = "center"),   # Smooth the tidal rate
      
      # Flood vs Ebb Tide
      # Positive velocity = flood tide (incoming, brings salt)
      # Negative velocity = ebb tide (outgoing, flushes salt)
      TidePhase = case_when(
         TideRate > 0.01 ~ 'Flood',
         TideRate < - 0.01 ~ 'Ebb',
         TRUE ~ 'Slack'
      ),
      
      # Tidal Duration Metrics (consecutive hours of flood tide)
      FloodIndicator = ifelse(TidePhase == 'Flood', 1, 0),
      ConsecFloodHours = ave(FloodIndicator, cumsum(FloodIndicator == 0), FUN = cumsum),
      ConsecFloodHours = ifelse(FloodIndicator == 0, 0, ConsecFloodHours),
      
      # Tidal Range Metrics
      TideRange6 = rollapply(Tide, width = 6, 
                             FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                             fill = NA, align = "right"),
      TideRange12 = rollapply(Tide, width = 12,
                              FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                              fill = NA, align = "right"),
      TideRange24 = rollapply(Tide, width = 24,
                              FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
                              fill = NA, align = "right"),
      
      # Tide-Flow Interactions
      discharge24 = zoo::rollmean(Discharge, k = 24, fill = NA, align = "right"),
      low_flow_threshold = quantile(discharge24, 0.25, na.rm = TRUE),
      is_low_flow = discharge24 < low_flow_threshold,
      LowFlowTideRange = ifelse(is_low_flow, TideRange12, 0),
      
      # Weighted Tidal Range Metric
      flow_weight = 1 / (discharge24 / median(discharge24, na.rm = TRUE)),
      WeightedTideRange12 = TideRange12 * pmin(flow_weight, 5), # cap at 5x weight
   ) %>%
   
   select(-flow_weight, -is_low_flow, -low_flow_threshold, -discharge24, -FloodIndicator) %>% # Remove unnecessary variables
   
   # =======================================================================================
   # PART 1: BASIC DISCHARGE FEATURES
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
      PowLagDischarge12 = LagDischarge12 ^ (-0.4),    
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
      RollingPowDischarge0.5 = zoo::rollmean(PowDischarge, 24 * 0.5, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge1   = zoo::rollmean(PowDischarge, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge2   = zoo::rollmean(PowDischarge, 24 * 2, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge4   = zoo::rollmean(PowDischarge, 24 * 4, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge7   = zoo::rollmean(PowDischarge, 24 * 7, fill = NA, align = "right", na.rm = TRUE),
      RollingPowDischarge10  = zoo::rollmean(PowDischarge, 24 * 10, fill = NA, align = "right", na.rm = TRUE),  
      RollingPowDischarge14  = zoo::rollmean(PowDischarge, 24 * 14, fill = NA, align = "right", na.rm = TRUE),
      RollingPowInflows1     = zoo::rollmean(PowInflows, 24 * 1, fill = NA, align = "right", na.rm = TRUE),
      RollingPowInflows2     = zoo::rollmean(PowInflows, 24 * 2, fill = NA, align = "right", na.rm = TRUE),     
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
         InflowsPercentile < 0.2 ~ "Low",     # True hydrologic stress
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
         #  HIGH STRESS: moderately emphasize natural flows
         StressLevel == "High" ~ 
            0.3 * RollingPowDischarge14 + 0.7 * RollingPowInflows2,
         
         # MODERATE STRESS: slight preference for natural flows
         StressLevel == "Moderate" ~ 
            0.4 * PowLagDischarge72 + 0.6 * PowLagInflows48,
         
         # NO STRESS: flush period, operations are dominant
         StressLevel == "Flush" ~ 
            0.85 * PowLagDischarge72 + 0.15 * PowLagInflows48,
         
         # NORMAL: standard balanced weighting
         TRUE ~ 0.6 * PowLagDischarge72 + 0.4 * PowLagInflows48
      ),
      
      BestLatent = case_when(
         IsHighStress ~ 0.3 * PowLagDischarge72 + 0.7 * RollingPowInflows2,  # Best lag + best rolling
         IsFlush ~ 0.8 * PowLagDischarge72 + 0.2 * RollingPowInflows2,       # Operational emphasis
         TRUE ~ 0.6 * PowLagDischarge72 + 0.4 * RollingPowDischarge14        # Best performers
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
            contains('Tide'), 
            starts_with(c('Lag', 
                          'Pow', 
                          'Rolling')),
            contains(c('Threshold', 
                     'Stress', 
                     'Since', 
                     'Consec', 
                     'Latent', 
                     'Is')),
            .after = Salinity) %>%                          # Organize all of the columns
   relocate(FERC, SalinitySeason, where(is.logical), 
            where(is.character), contains('Threshold'), .after = Inflows)
   
# Normalize Predictors and Add to model_data
preds_to_normalize <- colnames(model_data)[20 : ncol(model_data)] # Starting from the discharge column

# Apply the normalization function
normalized_predictors <- normalize_multiple_predictors(model_data, preds_to_normalize)
model_data <- normalized_predictors$data
norm_params <- normalized_predictors$parameters

######################### SIMPLE LINEAR MODEL DEVELOPMENT ############################

# Define predictor categories and their candidates
predictor_config <- list(
   
   # Tide predictors (will always include the best one in subsequent models)
   tide = c("Norm_Tide", "Norm_TideRate", 'Norm_LagTide1', 'Norm_LagTide2', 'Norm_LagTide4', 
            'Norm_TideRange6', 'Norm_TideRange12', 'Norm_TideRange24', 'Norm_LowFlowTideRange',
            'Norm_WeightedTideRange12'),
   
   # Discharge predictors (test systematically)
   discharge_lag = c("Norm_PowLagDischarge1", "Norm_PowLagDischarge3", "Norm_PowLagDischarge6", 
                     "Norm_PowLagDischarge10", "Norm_PowLagDischarge12", "Norm_PowLagDischarge24",
                     "Norm_PowLagDischarge36", "Norm_PowLagDischarge48", "Norm_PowLagDischarge72"),
   
   discharge_rolling = c("Norm_RollingPowDischarge0.5", "Norm_RollingPowDischarge1", 
                         "Norm_RollingPowDischarge2", "Norm_RollingPowDischarge4",
                         "Norm_RollingPowDischarge7", "Norm_RollingPowDischarge10", 
                         "Norm_RollingPowDischarge14"),
   
   # Inflow predictors
   inflow_lag = c("Norm_LagInflows12", "Norm_LagInflows24", "Norm_LagInflows48", "Norm_LagInflows72"),
   
   inflow_rolling = c("Norm_RollingPowInflows1", "Norm_RollingPowInflows2", 
                      "Norm_RollingPowInflows7", "Norm_RollingPowInflows10"),
   
   # Latent flow features
   latent_flow = c("Norm_SimpleLatent", "Norm_StressLatent", "Norm_BestLatent"),
   
   # Stress indicators
   stress_binary = c("IsModerateStress", "IsHighStress", "IsFlush", "IsStressed"),
   stress_continuous = c("Norm_StressHours_7day_Marietta", "Norm_StressHours_14day_Marietta", 
                         "Norm_StressHours_30day_Marietta", "Norm_StressHours_7day_Conowingo",
                         "Norm_StressHours_14day_Conowingo", "Norm_StressHours_30day_Conowingo",
                         "Norm_CumulativeStress_7day_Marietta", "Norm_CumulativeStress_14day_Marietta",
                         "Norm_CumulativeStress_30day_Marietta", "DaysSinceHighFlow"),
   
   # Seasonal/temporal
   temporal = c("SalinitySeason", "DayOfYear")
   
)

# Define performance criteria with updated weights
performance_criteria <- list(
   weights = c(
      # High salinity event metrics (60% of total weight)
      "high_sal_detection" = 0.25,      # Detection capability 
      "high_sal_accuracy" = 0.20,       # Accuracy of high salinity predictions
      "high_sal_reliability" = 0.15,    # Reliability (false alarm control)
      
      # Overall model performance (35% of total weight)
      "overall_performance" = 0.25,     # General model fit
      "model_stability" = 0.10,         # Consistent performance across conditions
      
      # Model characteristics (5% of total weight)
      "parsimony" = 0.05                # Model complexity penalty
   ),
   
   thresholds = list(
      min_high_sal_count = 3,           
      high_salinity_threshold = 0.3,    
      acceptable_far = 0.30,            
      min_hit_rate = 0.40               
   )
)

linear_model_results <- linear_model_builder(model_data, salinity_threshold)
# plots <- generate_model_diagnostics(model = results[['model']], model_name = 'Best Linear Model', data = model_data)
# plots$plots$performance
# plots$plots$high_salinity
# plots$plots$correlations
# plots$plots$temporal
# plots$plots$residuals
# plots$statistics

gam_model_results <- gam_model_builder(data = model_data, linear_model_results$model, response_var = 'Salinity', salinity_threshold)



############################ GAM AND THRESHOLD-BASED MODELS ######################

# =======================================================================================
# Generalized Additive Models (GAM) to allow for nonlinear relationships among predictors
# =======================================================================================

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




test <- get_predictions(results[["model"]], model_data)
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


p1 <- ggplot(test, aes(x = date_time)) +
   geom_line(aes(y = observed, color = 'Observed'), na.rm = TRUE, linewidth = 0.5) + 
   geom_line(aes(y = predicted, color = 'Predicted'), na.rm = TRUE, linewidth = 1.1) + 
   geom_point(data = filter(test, observed >= 1), aes(y = observed, color = 'Above Threshold'), na.rm = TRUE, size = 2) +
   scale_color_manual(name = NULL, values = c('Observed' = 'black', 'Predicted' = 'blue', 'Above Threshold' = 'red')) + 
   scale_x_datetime(limits = c(as_datetime('2015-02-28'), as_datetime('2015-12-31')), date_labels = '%b-%Y') + 
   theme_bw() + 
   labs(x = 'Date', y = 'Salinity (ppt)', title = paste('2015 Best Model:\n', results[["formula"]])) + 
   #labs(x = 'Date', y = 'Salinity (ppt)', title = '2016 Best Model: Salinity ~ 4hrTideLag + 2WeekRollingDischarge + 10DayRollingInflow') +
   ylim(0, 0.4) + 
   theme(plot.title = element_text(size = 16),
         legend.text = element_text(size = 14), 
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 14)) 
ggsave('2015.png', p1, path = '~/Downloads', dpi = 700, height = 8, width = 14)
ggsave('2016.png', p1, path = '~/Downloads', dpi = 700, height = 8, width = 14)


quick_correlation <- function(data, pred1 = "Norm_RollingPowDischarge14", pred2 = "Norm_StressHours_30day_Marietta") {
   cor_val <- cor(data[[pred1]], data[[pred2]], use = "complete.obs")
   cat(sprintf("Correlation between %s and %s: %.3f\n", pred1, pred2, cor_val))
   return(cor_val)
}

quick_correlation(model_data)

correlation_results <- analyze_predictor_correlation(model_data)
print(correlation_results$plots$scatter_salinity)

plots <- create_cleaner_scatter(model_data)
print(plots$hexbin)           # Shows data density
print(plots$extreme_events)   # Highlights high salinity events
print(plots$october_2016)     # Focuses on Oct 2016
print(plots$contour_surface)  # Shows salinity surface


