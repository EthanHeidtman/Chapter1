# =============================================================================
# Script Name:    GetRollingModelResults.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Gathers the outputs from the covariance experiment runs created
#                 by RunWindowExperiments.R. 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)
source('Scripts/Utilities/ExperimentHelpers.R')
source('Scripts/Utilities/SavePlots.R')
source('Scripts/Plots/ModelScreeningPlots.R')
source('Scripts/Plots/MultiPanelOverviewPlot.R')

DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
FERC_PATH = 'Data/Tidied/Processed/FERCFlowRequirement.csv'
OUTPUT_PATH = 'Outputs/Experiments/RollingWindowModeling'
PLOT_PATH = 'Outputs/Plots/Phase2_RollingWindowModel'

# Read in final cleaned model data
data <- read.csv(DATA_PATH)
data <- data %>%
   mutate(
      DateTime = parse_date_time(DateTime, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   arrange(DateTime) %>%
   distinct(DateTime, .keep_all = TRUE) %>%
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) 

# Read in FERC minimum flow requirement 
ferc <- read.csv(FERC_PATH)
ferc <- ferc %>%
   dplyr::select(-1) %>%
   rename(DayOfYear = Day)

# Get distribution screening results
dist_results <- load_results('DistributionScreening')
dist_predictions <- unnest_results(dist_results, experiment_col = 'experiment_name')
dist_predictions <- dist_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>% 
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
   drop_na(exceedance_probability)

dist_data <- left_join(dist_predictions, data, by = "DateTime")
dist_data <- dist_data %>%
   relocate(Year, Month, Day, Salinity, .after = DateTime) %>%
   relocate(33, 34, 35, .after = Salinity)
dist_data <- dist_data %>%
   mutate(actual_exceedance = Salinity > salinity_threshold)
rm(dist_results, dist_predictions)

distribution_results <- plot_model_performance(dist_data, group_label = "Distribution Family")

filenames <- c('DistributionMetrics.png', 'DistributionProbabilisticMetrics.png', 'DistributionCalibration.png')
plots <- list(distribution_results$key_metrics_plot, distribution_results$prob_metrics_plot, distribution_results$calibration_plot)
save_plots(plots, pathname = PLOT_PATH, filenames)


make_exceedance_plot <- function(df,
                                 xvar = "Norm_InflowDeficit",
                                 yvar = "Norm_PowDischarge",
                                 keep_range = c("2016-10-01", "2016-10-31"),
                                 n_per_class = 20000,
                                 breaks = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.99),
                                 nx = 200, ny = 200,
                                 seed = 123) {
   set.seed(seed)
   
   # Ensure logical
   df$actual_exceedance <- as.logical(df$actual_exceedance)
   
   # Keep mask for forced range
   keep_idx <- rep(FALSE, nrow(df))
   if (!is.null(keep_range) && length(keep_range) == 2) {
      start_dt <- as.POSIXct(as.Date(keep_range[1]))
      end_dt   <- as.POSIXct(as.Date(keep_range[2])) + 86399
      keep_idx <- df$DateTime >= start_dt & df$DateTime <= end_dt
   }
   
   # Pool (exclude forced-inclusion)
   df_pool <- df[!keep_idx, , drop = FALSE]
   df_pool <- df_pool[is.finite(df_pool[[xvar]]) &
                         is.finite(df_pool[[yvar]]) &
                         !is.na(df_pool$actual_exceedance), , drop = FALSE]
   
   # Compute per-class sample sizes
   class_counts <- df_pool %>%
      count(actual_exceedance, name = "n_rows") %>%
      mutate(n_sample = pmin(n_rows, n_per_class))
   
   # Balanced sampling
   sampled <- df_pool %>%
      inner_join(class_counts, by = "actual_exceedance") %>%
      group_by(actual_exceedance) %>%
      slice_sample(n = unique(class_counts$n_sample)) %>%
      ungroup() %>%
      select(-n_rows, -n_sample)
   
   # Forced keep data
   keep_data <- df[keep_idx, , drop = FALSE]
   keep_data <- keep_data[is.finite(keep_data[[xvar]]) &
                             is.finite(keep_data[[yvar]]) &
                             !is.na(keep_data$actual_exceedance), , drop = FALSE]
   
   # Final scatter data
   df_plot <- dplyr::distinct(dplyr::bind_rows(sampled, keep_data))
   
   # Interpolation data
   df_interp <- df[, c(xvar, yvar, "exceedance_probability")]
   df_interp <- df_interp[
      is.finite(df_interp[[xvar]]) &
         is.finite(df_interp[[yvar]]) &
         is.finite(df_interp[["exceedance_probability"]]),
      , drop = FALSE
   ]
   
   # Grid
   xo <- seq(min(df_interp[[xvar]]), max(df_interp[[xvar]]), length.out = nx)
   yo <- seq(min(df_interp[[yvar]]), max(df_interp[[yvar]]), length.out = ny)
   
   # Interpolation
   interp_fit <- akima::interp(
      x = df_interp[[xvar]],
      y = df_interp[[yvar]],
      z = df_interp[["exceedance_probability"]],
      xo = xo, yo = yo,
      duplicate = "mean",
      extrap = FALSE
   )
   
   interp_df <- expand.grid(x = interp_fit$x, y = interp_fit$y)
   interp_df$z <- as.vector(interp_fit$z)
   interp_df <- interp_df[is.finite(interp_df$z), , drop = FALSE]
   
   # Plot
   ggplot() +
      geom_contour_filled(
         data = interp_df,
         aes(x = x, y = y, z = z),
         breaks = breaks,
         alpha = 0.6
      ) +
      geom_contour(
         data = interp_df,
         aes(x = x, y = y, z = z),
         breaks = breaks,
         linewidth = 0.25,
         color = "black"
      ) +
      geom_point(
         data = df_plot,
         aes_string(x = xvar, y = yvar, color = "actual_exceedance"),
         size = 0.6, alpha = 0.7
      ) +
      scale_color_manual(
         values = c("FALSE" = "blue", "TRUE" = "red"),
         name = "Actual Exceed."
      ) +
      labs(x = xvar, y = yvar, fill = "Pred. prob") +
      theme_minimal(base_size = 13)
}



make_exceedance_plot(
   dist_data %>% filter(distribution_family == 'gpd'),
   keep_range = c("2016-10-01", "2016-10-31"),
   n_per_class = 20000,
   xvar = 'DayOfYear',
   yvar = 'Norm_InflowDeficit'
)

# Get threshold screening results
threshold_results <- load_results('ThresholdScreening')
threshold_predictions <- unnest_results(threshold_results, experiment_col = 'experiment_name')
threshold_predictions <- threshold_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>% 
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
   drop_na(exceedance_probability)

threshold_data <- left_join(threshold_predictions, data, by = 'DateTime')
threshold_data <- threshold_data %>%
   relocate(Year, Month, Day, Salinity, .after = DateTime) %>%
   relocate(29, 30, 31, .after = Salinity)
threshold_data <- threshold_data %>%
   mutate(actual_exceedance = Salinity > salinity_threshold)

rm(threshold_results, threshold_predictions)

# Generate threshold plots
threshold_results <- plot_model_performance(threshold_data, group_var = 'salinity_threshold', group_label = 'Salinity Threshold')
threshold_oct2016 <- plot_time_period_analysis(threshold_data, "2016-10-05", "2016-10-25", title_suffix = "October 2016 Intrusion Event", group_var = 'salinity_threshold')
threshold_multipanel2 <- plot_multi_panel_overview(threshold_data, start_date = "2016-10-05", end_date = "2016-10-25", group_var = 'salinity_threshold', single_group = '0.2')
threshold_multipanel3 <- plot_multi_panel_overview(threshold_data, start_date = "2016-10-05", end_date = "2016-10-25", group_var = 'salinity_threshold', single_group = '0.3')
threshold_multipanel4 <- plot_multi_panel_overview(threshold_data, start_date = "2016-10-05", end_date = "2016-10-25", group_var = 'salinity_threshold', single_group = '0.4')
threshold_multipanel6 <- plot_multi_panel_overview(threshold_data, start_date = "2016-10-05", end_date = "2016-10-25", group_var = 'salinity_threshold', single_group = '0.6')
threshold_multipanel75 <- plot_multi_panel_overview(threshold_data, start_date = "2016-10-05", end_date = "2016-10-25", group_var = 'salinity_threshold', single_group = '0.75')
threshold_multipanel1.0 <- plot_multi_panel_overview(threshold_data, start_date = "2016-10-05", end_date = "2016-10-25", group_var = 'salinity_threshold', single_group = '1')

plots <- list(
   threshold_results$key_metrics_plot,
   threshold_results$prob_metrics_plot,
   threshold_results$calibration_plot,
   threshold_oct2016,
   threshold_multipanel2,
   threshold_multipanel3,
   threshold_multipanel4,
   threshold_multipanel6,
   threshold_multipanel75,
   threshold_multipanel1.0
)

filenames <- c(
   'ThresholdMetrics.png',
   'ThresholdProbabilisticMetrics.png',
   'ThresholdCalibration.png',
   'ThresholdOctober2016.png',
   'ThresholdMultiPanel2.png',
   'ThresholdMultiPanel3.png',
   'ThresholdMultiPanel4.png',
   'ThresholdMultiPanel6.png',
   'ThresholdMultiPanel75.png',
   'ThresholdMultiPanel1.0.png'
)

save_plots(plots, pathname = PLOT_PATH, filenames, height = 8, width = 10)


# Get window size screening results
window_results <- load_covariance_results('WindowSizeScreening')
window_predictions <- unnest_covariance_results(window_results, experiment_col = 'experiment_name')
window_predictions <- window_predictions %>%
   mutate(
      DateTime = parse_date_time(timestamp, orders = c("Ymd HMS", "Ymd"))
   ) %>%
   select(-timestamp) %>%
   relocate(DateTime) %>% 
   mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
   drop_na(exceedance_probability)

window_data <- left_join(window_predictions, data, by = 'DateTime')
window_data <- window_data %>%
   relocate(Year, Month, Day, Salinity, .after = DateTime) %>%
   relocate(29, 30, 31, .after = Salinity)


create_static_flow_schedule <- function(dist_data, 
                                        current_ferc_schedule,
                                        target_exceedance_prob = 0.05) {
   
   # Get the range of days with salinity data
   salinity_days <- dist_data %>%
      summarise(
         min_doy = min(DayOfYear, na.rm = TRUE),
         max_doy = max(DayOfYear, na.rm = TRUE)
      )
   
   cat("Salinity data available for days", salinity_days$min_doy, "to", salinity_days$max_doy, "\n")
   
   # Calculate typical exceedance probability for days with data
   daily_risk <- dist_data %>%
      group_by(DayOfYear) %>%
      summarise(
         median_exceedance_prob = median(exceedance_probability, na.rm = TRUE),
         q75_exceedance_prob = quantile(exceedance_probability, 0.75, na.rm = TRUE),
         q95_exceedance_prob = quantile(exceedance_probability, 0.95, na.rm = TRUE),
         n_observations = n(),
         .groups = 'drop'
      ) %>%
      mutate(
         # How much does typical risk exceed target?
         risk_excess_median = pmax(0, median_exceedance_prob - target_exceedance_prob),
         risk_excess_conservative = pmax(0, q75_exceedance_prob - target_exceedance_prob),
         
         # Flow adjustment needed (as multiplier of current FERC requirement)
         flow_multiplier_median = 1 + (risk_excess_median / target_exceedance_prob) * 0.5,
         flow_multiplier_conservative = 1 + (risk_excess_conservative / target_exceedance_prob) * 0.5,
         
         # New flow requirements
         current_ferc_flow = current_ferc_schedule[DayOfYear],
         new_flow_median = current_ferc_flow * flow_multiplier_median,
         new_flow_conservative = current_ferc_flow * flow_multiplier_conservative,
         
         # Additional flow beyond current FERC
         additional_flow_median = new_flow_median - current_ferc_flow,
         additional_flow_conservative = new_flow_conservative - current_ferc_flow
      )
   
   # Create complete 365-day schedule
   complete_schedule <- data.frame(DayOfYear = 1:365) %>%
      left_join(daily_risk, by = "DayOfYear") %>%
      mutate(
         # For days without salinity data, use original FERC
         has_salinity_data = !is.na(median_exceedance_prob),
         final_flow_median = ifelse(has_salinity_data, new_flow_median, current_ferc_schedule[DayOfYear]),
         final_flow_conservative = ifelse(has_salinity_data, new_flow_conservative, current_ferc_schedule[DayOfYear]),
         final_additional_flow_median = ifelse(has_salinity_data, additional_flow_median, 0),
         final_additional_flow_conservative = ifelse(has_salinity_data, additional_flow_conservative, 0),
         data_source = ifelse(has_salinity_data, "MODEL_BASED", "ORIGINAL_FERC")
      )
   
   return(complete_schedule)
}

static_update <- create_static_flow_schedule(dist_data = dist_data %>% filter(distribution_family == 'gpd'), 
                                             current_ferc_schedule = ferc$Discharge)

ggplot(static_update, aes(x = DayOfYear)) + 
   geom_line(aes(y = current_ferc_flow), na.rm = TRUE, color = 'black') + 
   geom_line(aes(y = final_flow_conservative), na.rm = TRUE, color = 'blue')




