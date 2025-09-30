# =============================================================================
# Script Name:    HistoricalMinimumFlowsFurve.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)
library(zoo)
library(viridis)
source('Scripts/Modeling/SalinityModel.R')
source('Scripts/Plots/LRPlottingSuite.R')
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# DATA_PATH = 'Outputs/Experiments/Phase2_LogisticRegression/all_results.rds'
# OUTPUT_PATH = 'Outputs/Experiments/Phase2_LogisticRegression'
# PLOT_PATH = 'Outputs/Plots/Phase2_LogisticRegression'
# FERC_PATH = 'Data/Tidied/Processed/FERCFlowRequirement.csv'

# DATA_PATH = 'Outputs/Experiments/Phase2_WeightedLR/all_results.rds'
# OUTPUT_PATH = 'Outputs/Experiments/Phase2_WeightedLR'
# PLOT_PATH = 'Outputs/Plots/Phase2_WeightedLR'
# FERC_PATH = 'Data/Tidied/Processed/FERCFlowRequirement.csv'

DATA_PATH = 'Outputs/Experiments/Phase2_GAM/all_results.rds'
OUTPUT_PATH = 'Outputs/Experiments/Phase2_GAM'
PLOT_PATH = 'Outputs/Plots/Phase2_GAM'
FERC_PATH = 'Data/Tidied/Processed/FERCFlowRequirement.csv'

# Read in the FERC curve
ferc <- read.csv(FERC_PATH)
ferc <- ferc %>%
   dplyr::select(2, 'Discharge', 'Discharge_log') %>%
   rename(DayOfYear = Day,
          FERC = Discharge)

# Read in all model outputs
lr <- readRDS(DATA_PATH)

# Gather the models for each of the 9 logistic model runs for predictors RollingPowInflows and PowDischarge
final_models <- lapply(
   Filter(function(run) grepl("RollingPowInflows_PowDischarge", run$folder_name, fixed = TRUE), lr),
   `[[`, "model"
)
# Gather the outputs 9 logistic model runs for predictors RollingPowInflows and PowDischarge
final_data <- Filter(
   function(run) grepl("RollingPowInflows_PowDischarge", run$folder_name, fixed = TRUE),
   lr
)

# Combine all data into one large dataframe (long format)
final_data <- map_dfr(final_data, function(run) {
   run$data %>%
      mutate(threshold = unname(run$threshold_value[1]))
})

final_data <- final_data %>%
   mutate(threshold = round(threshold, digits = 2)) %>% # round thresholds to 2 digits
   left_join(., ferc, by = 'DayOfYear') %>%
   relocate(FERC, .after = DayOfYear_cos) %>%
   na.omit(exceedance_probability)


p1 <- all_threshold_exceedance_probs(final_data)
ggsave(paste0(PLOT_PATH, '/AllThresholdsAllYears.png'), p1, width = 14, height = 10, dpi = 600)

p2 <- all_threshold_window_plot(final_data)
ggsave(paste0(PLOT_PATH, '/AllThresholds2016.png'), p2, width = 14, height = 10, dpi = 600)


calculate_minimum_flow_requirements <- function(DATA_PATH, 
                                                predictor_combo = "RollingPowInflows_PowDischarge",
                                                in_var = 'RollingLogInflows',
                                                out_var = 'LogDischarge',
                                                ferc = ferc, 
                                                risk_tolerance = 0.10,
                                                temporal_aggregation = "day", 
                                                discharge_resolution = 200,
                                                ci_level = 0.95) {
   
   # Load required libraries
   library(dplyr)
   library(lubridate)
   library(purrr)
   
   # Read in all model outputs
   results <- readRDS(DATA_PATH)
   
   # Gather the models for specific predictor combination
   final_models <- lapply(
      Filter(function(run) grepl(predictor_combo, run$folder_name, fixed = TRUE), results),
      `[[`, "model"
   )
   
   # Gather the data for specific predictor combination  
   final_data_list <- Filter(
      function(run) grepl(predictor_combo, run$folder_name, fixed = TRUE),
      results
   )
   
   # Combine all data into one large dataframe 
   final_data <- map_dfr(final_data_list, function(run) {
      run$data %>%
         mutate(threshold = unname(run$threshold_value[1]))
   })
   
   final_data <- final_data %>%
      mutate(threshold = round(threshold, digits = 2))  # round thresholds to 2 digits
   
   # Join FERC data if provided
   if (!is.null(ferc)) {
      final_data <- final_data %>%
         left_join(., ferc, by = 'DayOfYear') %>%
         relocate(FERC, .after = DayOfYear_cos)
   }
   
   # Remove NAs
   final_data <- final_data %>%
      na.omit(exceedance_probability)
   
   # Calculate full discharge range once (same for all thresholds)
   full_discharge_range <- seq(
      from = min(final_data[[out_var]], na.rm = TRUE),
      to = max(final_data[[out_var]], na.rm = TRUE), 
      length.out = discharge_resolution
   )
   
   # Function to find safe discharge range (min and max)
   find_safe_discharge_range <- function(model, target_inflow, model_vars, 
                                         predictor_values, discharge_range,
                                         risk_tolerance, ci_level) {
      
      # Create prediction grid with target inflow and varying discharge
      pred_grid <- data.frame(
         in_var = rep(target_inflow, length(discharge_range)),
         out_var = discharge_range
      )
      
      # Add other predictor values (e.g., DayOfYear_sin/cos, or other variables)
      for (v in setdiff(model_vars, names(pred_grid))) {
         if (v %in% names(predictor_values)) {
            pred_grid[[v]] <- predictor_values[[v]]
         }
      }
      
      # Predict exceedance probabilities with standard errors for CI
      pred_results <- predict(model, newdata = pred_grid, type = "link", se.fit = TRUE)
      pred_probs <- plogis(pred_results$fit)  # Convert logit to probability
      
      # Calculate confidence intervals
      alpha <- 1 - ci_level
      z_value <- qnorm(1 - alpha/2)
      
      # CI on logit scale, then transform to probability scale
      lower_logit <- pred_results$fit - z_value * pred_results$se.fit
      upper_logit <- pred_results$fit + z_value * pred_results$se.fit
      pred_probs_lower <- plogis(lower_logit)
      pred_probs_upper <- plogis(upper_logit)
      
      # Find discharges meeting risk tolerance
      valid_idx <- which(pred_probs <= risk_tolerance)
      valid_idx_lower <- which(pred_probs_lower <= risk_tolerance)
      valid_idx_upper <- which(pred_probs_upper <= risk_tolerance)
      
      # Extract min and max safe discharge from main prediction
      min_safe <- if(length(valid_idx) > 0) min(discharge_range[valid_idx]) else NA_real_
      max_safe <- if(length(valid_idx) > 0) max(discharge_range[valid_idx]) else NA_real_
      
      # Extract min and max from CI bounds
      min_safe_lower <- if(length(valid_idx_lower) > 0) min(discharge_range[valid_idx_lower]) else NA_real_
      max_safe_lower <- if(length(valid_idx_lower) > 0) max(discharge_range[valid_idx_lower]) else NA_real_
      
      min_safe_upper <- if(length(valid_idx_upper) > 0) min(discharge_range[valid_idx_upper]) else NA_real_
      max_safe_upper <- if(length(valid_idx_upper) > 0) max(discharge_range[valid_idx_upper]) else NA_real_
      
      # Return results
      return(list(
         min_safe_flow = min_safe,
         max_safe_flow = max_safe,
         median_safe_flow = if(length(valid_idx) > 0) median(discharge_range[valid_idx]) else NA_real_,
         min_safe_flow_lower_ci = min_safe_lower,
         max_safe_flow_lower_ci = max_safe_lower,
         min_safe_flow_upper_ci = min_safe_upper,
         max_safe_flow_upper_ci = max_safe_upper
      ))
   }
   
   # Get unique thresholds and create model lookup
   unique_thresholds <- unique(final_data$threshold)
   model_lookup <- setNames(final_models, sapply(final_data_list, function(x) round(x$threshold_value[1], 2)))
   
   # Calculate results for each threshold
   results_list <- map_dfr(unique_thresholds, function(thresh) {
      
      # Get data and model for this threshold
      thresh_data <- final_data %>% filter(threshold == thresh)
      model <- model_lookup[[as.character(thresh)]]
      model_vars <- all.vars(formula(model))[-1]
      
      # Calculate temporal grouping
      if (temporal_aggregation == "day") {
         temporal_summary <- thresh_data %>%
            group_by(DayOfYear) %>%
            summarise(
               mean_inflow = mean(.data[[in_var]], na.rm = TRUE),
               # Store the actual day's seasonal values (should be constant within day)
               DayOfYear_sin = first(DayOfYear_sin),
               DayOfYear_cos = first(DayOfYear_cos),
               n_total_obs = n(),
               n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
               typical_safe_flow = ifelse(n_safe_obs > 0,
                                          mean(.data[[out_var]][exceedance_probability <= risk_tolerance], na.rm = TRUE),
                                          NA_real_),
               # Get mean of other predictors for this day
               across(all_of(setdiff(model_vars, c(in_var, out_var, "DayOfYear_sin", "DayOfYear_cos"))), 
                      ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
               .groups = "drop"
            )
         
      } else if (temporal_aggregation == "month") {
         # Convert DayOfYear to month
         thresh_data$Month <- month(as.Date(thresh_data$DayOfYear, origin = "2023-12-31"))
         
         temporal_summary <- thresh_data %>%
            group_by(Month) %>%
            summarise(
               mean_inflow = mean(.data[[in_var]], na.rm = TRUE),
               # For monthly, take mean of seasonal variables
               DayOfYear_sin = mean(DayOfYear_sin, na.rm = TRUE),
               DayOfYear_cos = mean(DayOfYear_cos, na.rm = TRUE),
               n_total_obs = n(),
               n_safe_obs = sum(exceedance_probability <= risk_tolerance, na.rm = TRUE),
               typical_safe_flow = ifelse(n_safe_obs > 0,
                                          mean(.data[[out_var]][exceedance_probability <= risk_tolerance], na.rm = TRUE),
                                          NA_real_),
               # Get mean of other predictors for this month
               across(all_of(setdiff(model_vars, c(in_var, out_var, "DayOfYear_sin", "DayOfYear_cos"))), 
                      ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
               .groups = "drop"
            )
      } else {
         stop("temporal_aggregation must be 'day' or 'month'")
      }
      
      # Calculate safe discharge range for each time period
      discharge_results <- temporal_summary %>%
         rowwise() %>%
         mutate(
            # Prepare predictor values for this time period
            pred_vals = list({
               vals <- list(
                  DayOfYear_sin = DayOfYear_sin,
                  DayOfYear_cos = DayOfYear_cos
               )
               # Add other predictors with mean_ prefix
               other_vars <- setdiff(model_vars, c(in_var, out_var, "DayOfYear_sin", "DayOfYear_cos"))
               for (v in other_vars) {
                  mean_col <- paste0("mean_", v)
                  if (mean_col %in% names(cur_data())) {
                     vals[[v]] <- cur_data()[[mean_col]]
                  }
               }
               vals
            }),
            # Find safe discharge range
            discharge_range_results = list(find_safe_discharge_range(
               model = model,
               target_inflow = mean_inflow,
               model_vars = model_vars,
               predictor_values = pred_vals,
               discharge_range = full_discharge_range,
               risk_tolerance = risk_tolerance,
               ci_level = ci_level
            ))
         ) %>%
         ungroup() %>%
         # Extract results from list column
         mutate(
            min_safe_flow = map_dbl(discharge_range_results, ~ .x$min_safe_flow),
            max_safe_flow = map_dbl(discharge_range_results, ~ .x$max_safe_flow),
            median_safe_flow = map_dbl(discharge_range_results, ~ .x$median_safe_flow),
            min_safe_flow_lower_ci = map_dbl(discharge_range_results, ~ .x$min_safe_flow_lower_ci),
            max_safe_flow_lower_ci = map_dbl(discharge_range_results, ~ .x$max_safe_flow_lower_ci),
            min_safe_flow_upper_ci = map_dbl(discharge_range_results, ~ .x$min_safe_flow_upper_ci),
            max_safe_flow_upper_ci = map_dbl(discharge_range_results, ~ .x$max_safe_flow_upper_ci)
         ) %>%
         select(-pred_vals, -discharge_range_results, -mean_inflow, 
                -starts_with("mean_"), -DayOfYear_sin, -DayOfYear_cos)
      
      # Add threshold
      discharge_results$threshold <- thresh
      
      return(discharge_results)
   })
   
   # Add date information and reformat based on temporal aggregation
   if (temporal_aggregation == "day") {
      results_list <- results_list %>%
         mutate(
            date = as.Date(DayOfYear, origin = "2023-12-31"),
            month = factor(months(date, abbreviate = TRUE), 
                           levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
            method = "Model Prediction"
         ) %>%
         select(threshold, DayOfYear, 
                min_safe_flow, max_safe_flow, median_safe_flow,
                min_safe_flow_lower_ci, max_safe_flow_lower_ci,
                min_safe_flow_upper_ci, max_safe_flow_upper_ci,
                typical_safe_flow, n_safe_obs, n_total_obs, 
                date, month, method) %>%
         arrange(threshold, DayOfYear)
      
   } else {  # monthly
      results_list <- results_list %>%
         mutate(
            month_name = factor(month.name[Month], levels = month.name),
            method = "Model Prediction"
         ) %>%
         select(threshold, Month, 
                min_safe_flow, max_safe_flow, median_safe_flow,
                min_safe_flow_lower_ci, max_safe_flow_lower_ci,
                min_safe_flow_upper_ci, max_safe_flow_upper_ci,
                typical_safe_flow, n_safe_obs, n_total_obs,
                month_name, method) %>%
         arrange(threshold, Month)
   }
   
   return(results_list)
}



daily_flows <- calculate_minimum_flow_requirements(DATA_PATH, ferc = ferc, temporal_aggregation = 'day', discharge_resolution = 2000)
daily_flows <- inner_join(daily_flows, ferc, by = c('DayOfYear')) %>% 
   relocate(FERC, Discharge_log, .after = DayOfYear) %>% rename(LogFERC = Discharge_log)
ggplot(daily_flows %>% filter(threshold %in% c(0.13, 0.14, 0.15, 0.16, 0.19, 0.28))) + 
   #geom_ribbon(aes(x = DayOfYear, ymin = min_safe_flow_lower, ymax = min_safe_flow_upper, fill = factor(threshold)), alpha = 0.3) + 
   geom_line(aes(x = DayOfYear, y = min_safe_flow, color = factor(threshold)), na.rm = TRUE) + 
   geom_line(aes(x = DayOfYear, y = LogFERC, color = 'LogFERC'), na.rm = TRUE, linewidth = 1.5) + 
   # scale_y_continuous(trans = 'log10',
   #                    breaks = scales::log_breaks(base = 10),
   #                    # small ticks at every integer multiple of 10^n (no labels)
   #                    minor_breaks = {
   #                       rng  <- range(daily_flows$min_safe_flow, na.rm = TRUE)
   #                       pwr  <- floor(log10(rng[1])):ceiling(log10(rng[2]))
   #                       as.numeric(outer(1:9, 10^pwr))                  # 1–9 × each decade
   #                    }) + 
   scale_color_brewer(name = 'Threshold (psu)',
                      palette = 'Dark2') +
   # scale_fill_brewer(name = '95th % Confidence Interval',
   #                   palette = 'Dark2') +
   labs(x = 'Day of Year',
        y = 'Minimum Safe Flow (cubic m/s)',
        title = 'Minimum Safe Flows by Salinity Exceedance Threshold') + 
   theme_bw() + 
   theme(plot.title = element_text(size = 16, face = 'bold'),
         axis.title = element_text(size = 14, face = 'bold'),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14, face = 'bold'))



min_flows <- calculate_minimum_flow_requirements_with_ci(
   data_path = DATA_PATH,
   #folder_pattern = "threshq",  
   predictor_combo = "RollingLogInflows_LogDischarge",  
   inflow_col = "RollingLogInflows",
   discharge_col = "LogDischarge", 
   date_col = "Date",
   risk_tolerance = 0.10,  
   temporal_aggregation = "day",  
   discharge_resolution = 200, 
   ci_level = 0.90
)

threshold_curves_df <- create_threshold_dataframe(min_flows)
   
