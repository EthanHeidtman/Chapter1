
# Source necessary functions
dirs <- c("Scripts/Functions/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Load necessary packages
library(here)        # For directory referencing
library(tidyverse)   # For data manipulation
library(dplyr)       # For data manipulation
library(zoo)         # For rolling computation
library(lubridate)   # For datetime related functions

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
            where(is.character), contains('Threshold'), .after = Inflows) %>%
   mutate_if(is.logical, as.factor) # Make logicals factors for GAM modeling

# Normalize Predictors and Add to model_data
preds_to_normalize <- colnames(model_data)[20 : ncol(model_data)] # Starting from the discharge column

# Apply the normalization function
normalized_predictors <- normalize_multiple_predictors(model_data, preds_to_normalize)
model_data <- normalized_predictors$data
norm_params <- normalized_predictors$parameters

# Write output files
outputs <- list(model_data, norm_params)
file_names <- c('FinalModelData', 'FinalNormalizationParams')
write_qs_files(outputs, 'Data/Tidied/Final', file_names)

# Clear global environment
rm(list = ls())

