# =============================================================================
# Script Name:    08_HoldoutValidation.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Evaluates GAM models trained on 2007-2022 against completely
#                 held-out 2023-2024 data. Mirrors the prediction logic of
#                 05_EvaluateDailyModels.R but uses unseen data. Computes a
#                 persistence baseline for comparison.
#
#                 Key detail: shift_predictors_by_k is applied to holdout_raw
#                 at each lag k, exactly as in script 02, so column names and
#                 lag structure match what each GAM was trained on.
#
#                 High-salinity subset metrics are suppressed when n < 
#                 MIN_HIGH_SAL_N to avoid numerically unstable statistics from
#                 near-zero event samples in the holdout period.
#
#                 Inputs:
#                   - Data/Tidied/Final/Daily/HoldoutData2023_2024.qs
#                   - Outputs/Experiments/Models/DailyGAM/Gam_{k}.qs
#                   - Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag{k}.qs
#                   - Data/Tidied/Final/Daily/FinalDataScreened_lag{k}.qs
#
#                 Outputs:
#                   - Outputs/Plots/HoldoutValidation/
#                   - Outputs/Validation/Holdout/
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================

library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tidyr)
library(mgcv)
library(svglite)
library(lubridate)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Utilities/ShiftPredictors.R')
source('Scripts/Plots/MultiPanelModelPlot.R')
source('Scripts/Utilities/ComputeGamPerformance.R')
source('Scripts/Plots/GamEvalPlots.R')
source('Scripts/Plots/SimpleModels/ModelEvaluationPlots.R')

# =============================================================================
# CONFIGURATION
# =============================================================================

lead_times         <- seq(1, 30, 1)
HOLDOUT_YEARS      <- c(2023, 2024)
HIGH_SAL_THRESHOLD <- 0.14
MIN_HIGH_SAL_N     <- 20      # suppress high-sal metrics below this sample size
PLOT_LAGS          <- c(14, 7, 3)

base_dir   <- "Outputs/Plots/HoldoutValidation"
output_dir <- "Outputs/Validation/Holdout"

if (!dir.exists(base_dir))   dir.create(base_dir,   recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =============================================================================
# LOAD DATA AND RF RESULTS
# =============================================================================

holdout_raw <- read_qs_files('Data/Tidied/Final/Daily/HoldoutData2023_2024.qs')

cat("Holdout data years:", paste(sort(unique(holdout_raw$Year)), collapse = ", "), "\n")
cat("Rows:", nrow(holdout_raw), "\n\n")

screened_data <- list()
rf_results    <- list()

for (k in lead_times) {
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
}

# =============================================================================
# PREDICTION LOOP
# =============================================================================

top_vars_by_k   <- list()
gam_predictions <- list()
predictors_used <- list()
models          <- list()

for (k in lead_times) {
   
   lag_name <- paste0("lag", k)
   cat("\n=== Processing lag", k, "===\n")
   
   # Shift holdout predictors by k days - mirrors script 02 exactly
   daily_data_k <- shift_predictors_by_k(holdout_raw, k = k)
   
   # Predictor group structure from screened training data
   screened_k <- screened_data[[lag_name]]
   
   salinity_cluster            <- screened_k %>% dplyr::select(contains('Salinity'))
   sustained_discharge_cluster <- screened_k %>% dplyr::select('Salinity', contains(c('RollingDischarge', 'RollingAnomaly')))
   flushing_discharge_cluster  <- screened_k %>% dplyr::select('Salinity', contains(c('MaxDischarge', 'ExceedFlux')))
   tide_cluster                <- screened_k %>% dplyr::select('Salinity', contains(c('TideRange', 'TideMean')))
   wind_cluster                <- screened_k %>% dplyr::select('Salinity', contains(c('RollingWindAlong', 'RollingWindCross')))
   
   group_list_k <- list(
      Salinity           = salinity_cluster,
      SustainedDischarge = sustained_discharge_cluster,
      FlushingDischarge  = flushing_discharge_cluster,
      Tide               = tide_cluster,
      Wind               = wind_cluster
   )
   
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df   = rf_results[[lag_name]]$importance,
      group_dfs       = group_list_k,
      n_top           = 1,
      importance_col  = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   top_vars <- unname(vapply(top_vars_by_k[[lag_name]], function(x) x$Variable, character(1)))
   predictors_used[[lag_name]] <- top_vars
   cat("Predictors:", paste(top_vars, collapse = ", "), "\n")
   
   # WindDir construction - mirrors script 05 exactly
   model_data_k <- daily_data_k %>%
      dplyr::select(c(1:"Salinity", all_of(top_vars))) %>%
      {
         if (any(grepl("Along", top_vars))) {
            wind_var <- top_vars[grepl("Along", top_vars)][1]
            daily_data_k <<- daily_data_k %>%
               mutate(WindDir = factor(
                  ifelse(.data[[wind_var]] >= 0, "UpEstuary", "DownEstuary")
               ))
         } else if (any(grepl("Cross", top_vars))) {
            wind_var <- top_vars[grepl("Cross", top_vars)][1]
            daily_data_k <<- daily_data_k %>%
               mutate(WindDir = factor(
                  ifelse(.data[[wind_var]] >= 0, "RightBank", "LeftBank")
               ))
         }
         .
      }
   
   valid_rows <- complete.cases(model_data_k[, top_vars])
   cat("Valid rows (incl. burn-in):", sum(valid_rows), "/", nrow(model_data_k), "\n")
   
   gam_file  <- paste0('Outputs/Experiments/Models/DailyGAM/Gam_', k, '.qs')
   model_obj <- read_qs_files(gam_file)
   models[[paste0('Lag', k)]] <- model_obj
   
   pred          <- rep(NA_real_, nrow(model_data_k))
   pred_response <- NULL
   
   pred[valid_rows] <- tryCatch({
      
      if (!is.null(model_obj$gam_object)) {
         
         transform_info   <- model_obj$transform_info
         family_type      <- transform_info$family
         manual_transform <- transform_info$manual_transform
         
         pred_response <- predict(
            model_obj$gam_object,
            newdata = daily_data_k[valid_rows, ],
            type    = "response"
         )
         
         if (family_type == "gaussian" && manual_transform == "log") {
            sigma_sq <- transform_info$sigma_sq
            pred_out <- exp(pred_response + sigma_sq / 2)
            if (any(pred_out > 10, na.rm = TRUE) || any(is.infinite(pred_out))) {
               warning(sprintf("Model lag%d has extreme/infinite predictions on holdout", k))
            }
            pred_out
         } else if (family_type == "gaussian" && manual_transform == "sqrt") {
            pred_response^2
         } else {
            pred_response
         }
         
      } else if (!is.null(model_obj$final_fit)) {
         predict(model_obj$final_fit, new_data = daily_data_k[valid_rows, ])$.pred
      } else {
         stop("Model object missing both gam_object and final_fit")
      }
      
   }, error = function(e) {
      warning(sprintf("Failed to predict with model lag%d: %s", k, e$message))
      rep(NA_real_, sum(valid_rows))
   })
   
   daily_data_k[[paste0(k, 'DayForecast')]] <- pred
   gam_predictions[[lag_name]]              <- daily_data_k
   
   rm(model_data_k, model_obj, pred, pred_response,
      top_vars, group_list_k, valid_rows, screened_k)
}

# =============================================================================
# MERGE ALL PREDICTIONS AND RESTRICT TO HOLDOUT YEARS
# =============================================================================

all_data <- gam_predictions[[paste0("lag", lead_times[1])]]

for (i in 2:length(lead_times)) {
   k        <- lead_times[i]
   lag_name <- paste0("lag", k)
   
   pred_cols <- gam_predictions[[lag_name]] %>%
      dplyr::select(DateTime, starts_with(paste0(k, 'DayForecast')))
   
   all_data <- all_data %>%
      left_join(pred_cols, by = "DateTime")
}

all_data <- all_data %>% filter(Year %in% HOLDOUT_YEARS)

cat("\nHoldout evaluation rows:", nrow(all_data), "\n")
cat("High-salinity observations (>", HIGH_SAL_THRESHOLD, "):",
    sum(all_data$Salinity > HIGH_SAL_THRESHOLD, na.rm = TRUE), "\n\n")

predictors_summary <- data.frame(
   LeadTime   = lead_times,
   Predictors = sapply(paste0("lag", lead_times),
                       function(x) paste(predictors_used[[x]], collapse = ", "))
)

# =============================================================================
# PERFORMANCE METRICS
# High-sal metrics set to NA when n < MIN_HIGH_SAL_N
# =============================================================================

performance_metrics <- calculate_performance_metrics(
   data               = all_data,
   lead_times         = lead_times,
   salinity_threshold = HIGH_SAL_THRESHOLD
)

# Report how many lags had insufficient high-sal samples
if ("HighSal_N" %in% names(performance_metrics)) {
   n_suppressed <- sum(performance_metrics$HighSal_N < MIN_HIGH_SAL_N, na.rm = TRUE)
   cat("High-sal metrics suppressed for", n_suppressed, "lags (n <", MIN_HIGH_SAL_N, ")\n\n")
   
   performance_metrics <- performance_metrics %>%
      mutate(across(
         starts_with("HighSal_") & !ends_with("_N"),
         ~ ifelse(HighSal_N < MIN_HIGH_SAL_N, NA_real_, .x)
      ))
}

# =============================================================================
# PERSISTENCE BASELINE
# 2022 burn-in retained in sal_series so lag lookups into early 2023 are valid
# =============================================================================

cat("Computing persistence baseline...\n")

sal_series <- holdout_raw %>%
   dplyr::select(DateTime, Salinity) %>%
   mutate(DateTime = as.Date(DateTime))

persistence_metrics <- map_dfr(lead_times, function(k) {
   
   obs <- all_data %>%
      dplyr::select(DateTime, Salinity) %>%
      mutate(DateTime = as.Date(DateTime))
   
   lagged <- sal_series %>%
      mutate(DateTime = DateTime + k) %>%
      rename(persistence = Salinity)
   
   joined <- obs %>%
      left_join(lagged, by = "DateTime") %>%
      filter(!is.na(Salinity) & !is.na(persistence))
   
   n    <- nrow(joined)
   rmse <- sqrt(mean((joined$Salinity - joined$persistence)^2))
   mae  <- mean(abs(joined$Salinity - joined$persistence))
   rsq  <- cor(joined$Salinity, joined$persistence)^2
   bias <- mean(joined$persistence - joined$Salinity)
   
   high_idx  <- joined$Salinity > HIGH_SAL_THRESHOLD
   high_n    <- sum(high_idx)
   high_rmse <- if (high_n >= MIN_HIGH_SAL_N) {
      sqrt(mean((joined$Salinity[high_idx] - joined$persistence[high_idx])^2))
   } else NA_real_
   
   tibble(lag = k, model = "persistence",
          n = n, rmse = rmse, mae = mae, rsq = rsq, bias = bias,
          high_n = high_n, high_rmse = high_rmse)
})

# Reshape GAM metrics for binding with persistence
gam_metrics_long <- performance_metrics %>%
   dplyr::select(LeadTime, RMSE, MAE, R2, Bias) %>%
   rename(lag = LeadTime, rmse = RMSE, mae = MAE, rsq = R2, bias = Bias) %>%
   mutate(model = "gam")

all_metrics <- bind_rows(
   gam_metrics_long,
   persistence_metrics %>% dplyr::select(lag, model, rmse, mae, rsq, bias)
)

# =============================================================================
# SKILL SCORE: SS = 1 - RMSE_model / RMSE_persistence
# Positive = model beats persistence; 0 = equivalent; negative = worse
# =============================================================================

skill_scores <- gam_metrics_long %>%
   dplyr::select(lag, rmse_gam = rmse) %>%
   left_join(
      persistence_metrics %>% dplyr::select(lag, rmse_persistence = rmse),
      by = "lag"
   ) %>%
   mutate(skill_score = 1 - (rmse_gam / rmse_persistence))

# =============================================================================
# FIGURES
# =============================================================================

# --- Overall RMSE: GAM vs persistence ---
p_rmse_comparison <- all_metrics %>%
   ggplot(aes(x = lag, y = rmse, color = model, linetype = model)) +
   geom_line(linewidth = 0.8) +
   geom_point(size = 2) +
   scale_color_manual(
      values = c("gam" = "#2166ac", "persistence" = "#d73027"),
      labels = c("gam" = "GAM", "persistence" = "Persistence")
   ) +
   scale_linetype_manual(
      values = c("gam" = "solid", "persistence" = "dashed"),
      labels = c("gam" = "GAM", "persistence" = "Persistence")
   ) +
   labs(x = "Lead time (days)", y = "RMSE (psu)",
        title = "Holdout RMSE: GAM vs persistence",
        subtitle = paste("Evaluation period:", paste(HOLDOUT_YEARS, collapse = "\u2013")),
        color = NULL, linetype = NULL) +
   theme_bw(base_size = 11) +
   theme(legend.position = "bottom")

# --- Skill score ---
p_skill <- skill_scores %>%
   ggplot(aes(x = lag, y = skill_score)) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
   geom_line(color = "#2166ac", linewidth = 0.8) +
   geom_point(color = "#2166ac", size = 2) +
   labs(x = "Lead time (days)",
        y = "Skill score (1 - RMSEmodel / RMSEpersistence)",
        title = "Model skill relative to persistence",
        subtitle = "Positive = GAM outperforms persistence") +
   theme_bw(base_size = 11)

p_skill_combined <- p_rmse_comparison / p_skill
ggsave(file.path(base_dir, 'HoldoutSkillVsPersistence.png'),
       p_skill_combined, width = 7, height = 8, dpi = 600)
ggsave(file.path(base_dir, 'HoldoutSkillVsPersistence.svg'),
       p_skill_combined, width = 7, height = 8, device = svglite)

# --- Standard performance plots (overall only; high-sal suppressed) ---
p_rmse <- plot_performance_by_leadtime(
   performance_metrics, metric = "RMSE",
   x_label = 'Lead Time (days)', y_label = 'RMSE (psu)')
ggsave(file.path(base_dir, 'RMSE_OverK.png'), p_rmse, width = 12, height = 8, dpi = 600)

p_r2 <- plot_performance_by_leadtime(
   performance_metrics, metric = "R2",
   x_label = 'Lead Time (days)', y_label = 'R2')
ggsave(file.path(base_dir, 'R2_OverK.png'), p_r2, width = 12, height = 8, dpi = 600)

p_mae <- plot_performance_by_leadtime(
   performance_metrics, metric = "MAE",
   x_label = 'Lead Time (days)', y_label = 'MAE (psu)')
ggsave(file.path(base_dir, 'MAE_OverK.png'), p_mae, width = 12, height = 8, dpi = 600)

p_bias <- plot_performance_by_leadtime(
   performance_metrics, metric = "Bias",
   x_label = 'Lead Time (days)', y_label = 'Bias (psu)')
ggsave(file.path(base_dir, 'Bias_OverK.png'), p_bias, width = 12, height = 8, dpi = 600)

p_nse <- plot_performance_by_leadtime(
   performance_metrics, metric = "NSE",
   x_label = 'Lead Time (days)', y_label = 'NSE')
ggsave(file.path(base_dir, 'NSE_OverK.png'), p_nse, width = 12, height = 8, dpi = 600)

# --- Time series panels for selected lags ---
plot <- plot_salinity_forecast_panels(
   data       = all_data,
   date_range = c(paste0(min(HOLDOUT_YEARS), '-01-01'),
                  paste0(max(HOLDOUT_YEARS), '-12-31')),
   models     = paste0(PLOT_LAGS, 'DayForecast'),
   title      = "GAM holdout validation: 2023-2024"
)
ggsave(file.path(base_dir, 'HoldoutForecastPanels.png'), plot, height = 10, width = 12, dpi = 600)
ggsave(file.path(base_dir, 'HoldoutForecastPanels.svg'),
       plot, height = 14, width = 18, device = svglite)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

outputs <- list(all_metrics, all_data, skill_scores, predictors_summary)
file_names <- c('HoldoutMetrics', 'HoldoutPredictions', 'HoldoutSkillScores', 'HoldoutPredictorsUsed')
write_qs_files(outputs, output_dir, file_names)


# =============================================================================
# CONSOLE SUMMARY
# =============================================================================

cat("\n=== HOLDOUT VALIDATION COMPLETE ===\n")
cat("Evaluation years:", paste(HOLDOUT_YEARS, collapse = ", "), "\n")
cat("Lead times evaluated:", length(lead_times), "\n")
cat("High-sal threshold:", HIGH_SAL_THRESHOLD, "psu\n")
cat("High-sal min sample for metrics:", MIN_HIGH_SAL_N, "\n\n")

cat("Skill scores (GAM vs persistence):\n")
print(skill_scores, n = Inf)

cat("\nPlots saved to:   ", base_dir, "\n")
cat("Outputs saved to: ", output_dir, "\n")

rm(list = ls())