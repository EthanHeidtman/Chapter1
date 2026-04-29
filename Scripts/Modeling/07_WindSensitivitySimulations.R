# =============================================================================
# GAM Wind Scenario Sensitivity — Multi-Lead-Time Loop
#
# Loops over specified lead times, runs chained U wind scenarios for each,
# saves individual plots to Outputs/Plots/SensitivitySimulations/UWind/,
# then produces a cross-lead-time summary sensitivity plot.
# =============================================================================


library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(tidyr)
library(mgcv)
library(svglite)
library(ggplot2)

# Source necessary functions 
source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/GetTopVarImp.R')
source('Scripts/Plots/SimpleModels/ModelEvaluationPlots.R')
source('Scripts/Plots/MultiPanelModelPlot.R')
source('Scripts/Utilities/ComputeGamPerformance.R')
source('Scripts/Plots/GamEvalPlots.R')

# Define lead times that were run
lead_times <- seq(1, 30, 1)

# Initialize lists to store results
screened_data <- list()
rf_results <- list()
top_vars_by_k <- list()
gam_predictions <- list()
predictors_used <- list()
models <- list()

# Read in results and screened data
for(k in lead_times) {
   # Read screened data
   screened_data[[paste0("lag", k)]] <- read_qs_files(
      paste0('Data/Tidied/Final/Daily/FinalDataScreened_lag', k, '.qs')
   )
   
   # Read RF results
   rf_results[[paste0("lag", k)]] <- read_qs_files(
      paste0('Outputs/Experiments/Models/DailyRF/RFDailyScreening_lag', k, '.qs')
   )
   
}

# Loop through each k to generate predictions and keep all data
for(k in lead_times) {
   
   lag_name <- paste0("lag", k)
   
   # Get the screened data for this k (FULL dataset)
   daily_data_k <- screened_data[[lag_name]]
   
   # Select variables
   salinity_cluster <- daily_data_k %>% dplyr::select(c(contains('Salinity')))
   rolling_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('RollingDischarge', 'LagDischarge'))))
   flushing_discharge_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('ExceedFlux', 'Flush', 'MaxDischarge'))))
   tide_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains('Tide')))
   wind_cluster <- daily_data_k %>% dplyr::select(c('Salinity', contains(c('RollingU', 'RollingV', 'Gust', 'Wind', 'LagU', 'LagV'))))
   
   group_list_k <- list(
      salinity = salinity_cluster,
      rolling_discharge = rolling_discharge_cluster,
      flushing_discharge = flushing_discharge_cluster,
      tide = tide_cluster,
      wind = wind_cluster
   )
   
   top_vars_by_k[[lag_name]] <- get_top_vars_by_group(
      importance_df = rf_results[[lag_name]]$importance,
      group_dfs = group_list_k,
      n_top = list(
         salinity = 1, 
         rolling_discharge = 1, 
         flushing_discharge = 1, 
         tide = 1, 
         wind = 1
      ),
      importance_col = "IncMSE_OOB",
      show_importance = TRUE
   )
   
   # Top predictors
   top_vars <- unname(vapply(top_vars_by_k[[lag_name]], function(x) x$Variable, character(1)))
   predictors_used[[lag_name]] <- top_vars
   
   # Build model input
   model_data_k <- daily_data_k %>%
      dplyr::select(c(1:"Salinity", all_of(top_vars))) %>%
      { 
         if (any(grepl("V", top_vars))) {
            wind_var <- top_vars[grepl("V", top_vars)][1]
            mutate(., WindDir = factor(ifelse(.data[[wind_var]] < 0, "North", "South")))
            
         } else if (any(grepl("U", top_vars))) {
            wind_var <- top_vars[grepl("U", top_vars)][1]
            mutate(., WindDir = factor(ifelse(.data[[wind_var]] < 0, "East", "West")))
            
         } else {
            .
         }
      }
   
   # Mask rows with complete predictors
   valid_rows <- complete.cases(model_data_k[, top_vars])
   
   # Load GAM model
   gam_file <- paste0('Outputs/Experiments/Models/DailyGAM/Gam_', k, '.qs')
   model_obj <- read_qs_files(gam_file)
   
   models[[paste0('Lag', k)]] <- model_obj
   
   # Predict on valid rows
   pred <- rep(NA_real_, nrow(model_data_k))
   
   pred[valid_rows] <- tryCatch({
      
      if (!is.null(model_obj$gam_object)) {
         
         transform_info <- model_obj$transform_info
         family_type <- transform_info$family
         manual_transform <- transform_info$manual_transform
         
         pred_response <- predict(
            model_obj$gam_object,
            newdata = model_data_k[valid_rows, ],
            type = "response"
         )
         
         # Back-transform if needed
         if (family_type == "gaussian" && manual_transform == "log") {
            sigma_sq <- transform_info$sigma_sq
            pred_out <- exp(pred_response + sigma_sq/2)
            
            if (any(pred_out > 10, na.rm = TRUE) || any(is.infinite(pred_out))) {
               warning(sprintf("Model lag%d has extreme/infinite predictions", k))
            }
            
            pred_out
            
         } else if (family_type == "gaussian" && manual_transform == "sqrt") {
            pred_response^2
            
         } else {
            pred_response
         }
         
      } else if (!is.null(model_obj$final_fit)) {
         
         predict(
            model_obj$final_fit,
            new_data = model_data_k[valid_rows, ]
         )$.pred
         
      } else {
         stop("Model object missing both gam_object and final_fit")
      }
      
   }, error = function(e) {
      warning(sprintf("Failed to predict with model lag%d: %s", k, e$message))
      rep(NA_real_, sum(valid_rows))
   })
   
   # Attach predictions
   daily_data_k[[paste0(k, 'DayForecast')]] <- pred
   
   gam_predictions[[lag_name]] <- daily_data_k
   
   # Cleanup
   rm(model_data_k, model_obj, pred, pred_response, transform_info,
      top_vars, group_list_k, valid_rows)
}

# Merge all predictions
all_data <- gam_predictions[[paste0("lag", lead_times[1])]]
for(i in 2:length(lead_times)) {
   k <- lead_times[i]
   lag_name <- paste0("lag", k)
   
   # Select only datetime and the new prediction column
   pred_cols <- gam_predictions[[lag_name]] %>%
      dplyr::select(DateTime, starts_with(paste0(k, 'DayForecast')))
   
   # Join by datetime
   all_data <- all_data %>%
      left_join(pred_cols, by = "DateTime")
}

# =============================================================================
# USER INPUTS
# =============================================================================

YEAR            <- 2016
FLUSH_THRESHOLD <- 500
PLOT_START      <- as.Date("2016-09-01")
PLOT_END        <- as.Date("2016-11-30")
PERTURB_START   <- as.Date("2016-09-09")   # 30 days before event onset
PERTURB_END     <- as.Date("2016-10-09")   # event onset
EVENT_START     <- as.Date("2016-10-09")
EVENT_END       <- as.Date("2016-10-24")

# Fine-grained positive shifts: +0.25 to +2.0 m/s
# Only positive (less easterly / more westerly) — stronger easterly excluded
# because even -1 m/s pushes rolling mean outside training range
SHIFTS       <- seq(0.25, 2.0, by = 0.25)
SHIFT_LABELS <- sprintf("+%.2f m/s", SHIFTS)

OUT_DIR <- "Outputs/Plots/SensitivitySimulations/UWind"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD RAW DATA ONCE
# =============================================================================

raw_df <- as.data.frame(read_qs_files('Data/Tidied/Final/Daily/FinalModelData.qs')) %>%
   filter(Year == YEAR) %>%
   arrange(DateTime)

raw_u <- raw_df$U
raw_q <- raw_df$Discharge

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

extract_window <- function(nm) {
   as.integer(regmatches(nm, regexpr("[0-9]+$", nm)))
}

compute_wind_preds <- function(u, var_names) {
   out <- list()
   for (nm in var_names) {
      w        <- extract_window(nm)
      out[[nm]] <- zoo::rollmean(u, w, fill = NA, align = "right")
   }
   as.data.frame(out)
}

compute_discharge_preds <- function(q, var_names) {
   out <- list()
   for (nm in var_names) {
      w <- extract_window(nm)
      if (grepl("RollingDischarge", nm)) {
         out[[nm]] <- zoo::rollmean(q, w, fill = NA, align = "right")
      } else if (grepl("ExceedFlux|FlushFlux", nm)) {
         exceed    <- pmax(0, q - FLUSH_THRESHOLD)
         out[[nm]] <- pmin(zoo::rollsum(exceed, w, fill = NA, align = "right"), 40000)
      } else if (grepl("MaxDischarge", nm)) {
         out[[nm]] <- zoo::rollapply(q, w, max, fill = NA, align = "right")
      }
   }
   as.data.frame(out)
}

# =============================================================================
# SCENARIO DEFINITIONS (shared across all lead times)
# =============================================================================

in_window <- function(dates) {
   as.Date(dates) >= PERTURB_START & as.Date(dates) < PERTURB_END
}

scenarios <- c(
   list(list(label = "Observed", group = "Observed",
             modifier = function(u, d) u)),
   lapply(seq_along(SHIFTS), function(i) {
      s <- SHIFTS[i]
      list(
         label    = SHIFT_LABELS[i],
         group    = "Weaker easterly",
         modifier = local({
            shift <- s
            function(u, d) {
               u_new <- u
               u_new[in_window(d)] <- u_new[in_window(d)] + shift
               u_new
            }
         })
      )
   })
)

all_labels <- sapply(scenarios, `[[`, "label")

# Plot styling — blue gradient
shift_colors    <- colorRampPalette(c("#c6dbef", "#08519c"))(length(SHIFTS))
scenario_colors <- c(setNames("#002030",    "Simulated (obs. wind)"),
                     setNames(shift_colors, SHIFT_LABELS))
scenario_lwd    <- c(setNames(1.0,          "Simulated (obs. wind)"),
                     setNames(rep(0.65, length(SHIFTS)), SHIFT_LABELS))
scenario_lty    <- c(setNames("dashed",     "Simulated (obs. wind)"),
                     setNames(rep("solid", length(SHIFTS)), SHIFT_LABELS))

# =============================================================================
# MAIN LOOP
# =============================================================================

summary_rows <- list()

for (LEAD_TIME in lead_times) {
   
   k       <- LEAD_TIME
   gam_obj <- models[[paste0("Lag", k)]]$gam_object
   cat(sprintf("\n=== Running Lag%d ===\n", k))
   
   # ---- Detect predictor names -------------------------------------------
   gam_cols    <- names(gam_obj$model)
   pred_cols_k <- gam_cols[-1]
   base_names  <- sub(paste0("_", k, "$"), "", pred_cols_k)
   
   is_discharge <- grepl("RollingDischarge|ExceedFlux|FlushFlux|MaxDischarge", base_names)
   is_wind      <- grepl("Rolling[UV]", base_names)
   is_lag_sal   <- grepl("LagSalinity|Salinity", base_names)
   is_winddir   <- base_names == "WindDir"
   
   discharge_base <- base_names[is_discharge]
   wind_base      <- base_names[is_wind]
   other_base     <- base_names[!is_discharge & !is_wind & !is_lag_sal & !is_winddir]
   
   # Skip lead times where model uses V wind — we are perturbing U only
   wind_component <- unique(sub("Rolling([UV]).*", "\\1", wind_base))
   if (length(wind_component) == 0 || !all(wind_component == "U")) {
      cat(sprintf("  Skipping Lag%d: model uses %s wind, not U — cannot perturb U here\n",
                  k, paste(wind_base, collapse = ", ")))
      next
   }
   
   train_min_u  <- min(gam_obj$model[[paste0(wind_base[1], "_", k)]], na.rm = TRUE)
   train_max_u  <- max(gam_obj$model[[paste0(wind_base[1], "_", k)]], na.rm = TRUE)
   wind_window  <- extract_window(wind_base[1])
   roll_obs     <- zoo::rollmean(raw_u, wind_window, fill = NA, align = "right")
   window_roll_u <- mean(roll_obs[in_window(raw_df$DateTime)], na.rm = TRUE)
   
   cat(sprintf("  Wind predictor: %s  Training range: [%.2f, %.2f]\n",
               wind_base[1], train_min_u, train_max_u))
   
   # ---- build_pred_df ----------------------------------------------------
   build_pred_df <- function(raw_df, u_modified) {
      wind_df <- compute_wind_preds(u_modified, wind_base)
      disc_df <- if (length(discharge_base) > 0)
         compute_discharge_preds(raw_q, discharge_base)
      else
         data.frame()
      base_df <- bind_cols(
         raw_df %>% select(DateTime, Salinity, all_of(other_base)),
         wind_df,
         disc_df
      )
      all_base <- c(other_base, wind_base, discharge_base)
      shifted  <- base_df %>%
         mutate(across(all_of(all_base), ~ dplyr::lag(.x, k),
                       .names = paste0("{.col}_", k)))
      lag_sal_col            <- paste0("LagSalinity_", k)
      shifted[[lag_sal_col]] <- dplyr::lag(base_df$Salinity, k)
      if (length(wind_base) >= 1) {
         shifted_wind_col <- paste0(wind_base[1], "_", k)
         wind_levels      <- levels(gam_obj$model$WindDir)
         shifted <- shifted %>%
            mutate(WindDir = factor(
               ifelse(.data[[shifted_wind_col]] < 0, "East", "West"),
               levels = wind_levels))
      }
      shifted
   }
   
   # ---- run_chain --------------------------------------------------------
   run_chain <- function(pred_df) {
      n            <- nrow(pred_df)
      preds        <- rep(NA_real_, n)
      lag_sal_col  <- paste0("LagSalinity_", k)
      pred_history <- rep(NA_real_, n)
      for (i in seq_len(n)) {
         row <- pred_df[i, , drop = FALSE]
         if (i > k && !is.na(pred_history[i - k]))
            row[[lag_sal_col]] <- pred_history[i - k]
         gam_pred_cols <- pred_cols_k[pred_cols_k %in% names(row)]
         if (any(is.na(row[, gam_pred_cols]))) { pred_history[i] <- NA_real_; next }
         pred            <- predict(gam_obj, newdata = row, type = "response")
         pred_history[i] <- pred
         preds[i]        <- pred
      }
      preds
   }
   
   # ---- Run scenarios ----------------------------------------------------
   results <- raw_df %>% select(DateTime, Salinity)
   for (sc in scenarios) {
      u_mod               <- sc$modifier(raw_u, raw_df$DateTime)
      pred_df             <- build_pred_df(raw_df, u_mod)
      results[[sc$label]] <- run_chain(pred_df)
   }
   
   # ---- Collect summary --------------------------------------------------
   obs_max <- max(results$Observed, na.rm = TRUE)
   for (i in seq_along(SHIFTS)) {
      lbl    <- SHIFT_LABELS[i]
      sc_max <- max(results[[lbl]], na.rm = TRUE)
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
         LeadTime  = k,
         Scenario  = lbl,
         Shift     = SHIFTS[i],
         Obs_Max   = obs_max,
         Scen_Max  = sc_max,
         Reduction = obs_max - sc_max
      )
   }
   
   # ---- Individual plot --------------------------------------------------
   plot_labels <- c("Simulated (obs. wind)", all_labels[-1])
   
   results_plot <- results %>% rename(`Simulated (obs. wind)` = Observed)
   observed_long <- raw_df %>%
      select(DateTime, Salinity) %>%
      filter(as.Date(DateTime) >= PLOT_START, as.Date(DateTime) <= PLOT_END)
   results_long <- results_plot %>%
      pivot_longer(-DateTime, names_to = "Scenario", values_to = "Salinity") %>%
      mutate(Scenario = factor(Scenario, levels = plot_labels)) %>%
      filter(as.Date(DateTime) >= PLOT_START, as.Date(DateTime) <= PLOT_END)
   
   p <- ggplot() +
      # annotate("rect",
      #          xmin = as.POSIXct(max(PERTURB_START, PLOT_START)),
      #          xmax = as.POSIXct(PERTURB_END),
      #          ymin = -Inf, ymax = Inf, fill = "#9ecae1", alpha = 0.10) +
      # annotate("rect",
      #          xmin = as.POSIXct(EVENT_START), xmax = as.POSIXct(EVENT_END),
      #          ymin = -Inf, ymax = Inf, fill = "#238b45", alpha = 0.06) +
      # geom_vline(xintercept = as.POSIXct(EVENT_START),
      #            linetype = "dotted", color = "#002030", linewidth = 0.6) +
      # annotate("text", x = as.POSIXct(EVENT_START), y = Inf,
      #          label = "Oct 9\nevent onset", hjust = 1.1, vjust = 1.3,
      #          size = 3.2, colour = "#002030") +
      # geom_vline(xintercept = as.POSIXct(EVENT_END),
      #            linetype = "dotted", color = "#238b45", linewidth = 0.6) +
      # annotate("text", x = as.POSIXct(EVENT_END), y = Inf,
      #          label = "Oct 24\nobs. flush", hjust = -0.1, vjust = 1.3,
      #          size = 3.2, colour = "#238b45") +
      geom_line(data = results_long,
                aes(x = DateTime, y = Salinity,
                    color = Scenario, linewidth = Scenario, linetype = Scenario)) +
      geom_line(data = observed_long, aes(x = DateTime, y = Salinity),
                color = "#f58220", linewidth = 1.1, alpha = 0.9) +
      geom_hline(yintercept = 0.5, color = "#002030",
                 linetype = "dashed", linewidth = 0.5) +
      annotate("text", x = as.POSIXct(PLOT_START), y = 0.52,
               label = "EPA Secondary Standard (0.5 psu)",
               hjust = 0, vjust = 0, size = 3.2, colour = "#002030") +
      scale_color_manual(values = scenario_colors, breaks = plot_labels) +
      scale_linewidth_manual(values = scenario_lwd, breaks = plot_labels, guide = "none") +
      scale_linetype_manual(values = scenario_lty, breaks = plot_labels, guide = "none") +
      guides(color = guide_legend(
         override.aes = list(linewidth = unname(scenario_lwd[plot_labels]),
                             linetype  = unname(scenario_lty[plot_labels])), ncol = 1)) +
      scale_x_datetime(date_labels = "%b %d", date_breaks = "2 weeks",
                       limits = as.POSIXct(c(PLOT_START, PLOT_END))) +
      scale_y_continuous(name = "Salinity (psu)") +
      labs(x = "Date (2016)", color = "Scenario",
           title    = sprintf("GAM Lag%d — Wind Scenario Sensitivity, October 2016", k)) +
      theme_bw() +
      theme(plot.title    = element_text(size = 13, face = "bold", color = "#002030"),
            plot.subtitle = element_text(size = 8.5, color = "#555555"),
            axis.title    = element_text(size = 12, face = "bold", color = "#002030"),
            axis.text.x   = element_text(size = 9, angle = 15, hjust = 1),
            axis.text.y   = element_text(size = 10),
            panel.border  = element_rect(colour = "#002030", fill = NA, linewidth = 1),
            legend.position  = "right",
            legend.title     = element_text(size = 10, face = "bold"),
            legend.text      = element_text(size = 9),
            legend.key.width = unit(1.5, "cm"))
   
   ggsave(file.path(OUT_DIR, sprintf("UWind_Lag%02d_Oct2016.png", k)),
          p, width = 13, height = 6, dpi = 300)
   cat(sprintf("  Saved: UWind_Lag%02d_Oct2016.png\n", k))
}

# =============================================================================
# SUMMARY SENSITIVITY PLOT
# =============================================================================

summary_df <- bind_rows(summary_rows)

# Use a continuous color scale for shift magnitude
p_summary <- ggplot(summary_df,
                    aes(x = LeadTime, y = Reduction,
                        color = Shift, group = Scenario)) +
   geom_hline(yintercept = 0, color = "#aaaaaa", linewidth = 0.4) +
   geom_line(linewidth = 0.7) +
   geom_point(size = 2) +
   scale_color_gradient(low = "#c6dbef", high = "#08519c",
                        name = "U wind shift\n(m/s, less easterly)") +
   scale_x_continuous(breaks = lead_times, name = "Forecast Lead Time (days)") +
   scale_y_continuous(name = "Max Salinity Reduction (psu)\n(Observed baseline − Scenario)") +
   labs(
      title    = "Wind Scenario Sensitivity Across Lead Times",
      subtitle = "October 2016 event  |  Reduction = observed max − scenario max\nHigher = more sensitive to wind perturbation"
   ) +
   theme_bw() +
   theme(
      plot.title    = element_text(size = 13, face = "bold", color = "#002030"),
      plot.subtitle = element_text(size = 9, color = "#555555"),
      axis.title    = element_text(size = 11, face = "bold", color = "#002030"),
      axis.text     = element_text(size = 10),
      panel.border  = element_rect(colour = "#002030", fill = NA, linewidth = 1),
      legend.position  = "right",
      legend.title     = element_text(size = 10, face = "bold"),
      legend.text      = element_text(size = 9)
   )

ggsave(file.path(OUT_DIR, "UWind_Sensitivity_Summary.png"),
       p_summary, width = 10, height = 6, dpi = 300)
cat("\nSaved: UWind_Sensitivity_Summary.png\n")

p_summary

# Clear global environment
rm(list = ls())
