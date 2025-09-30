# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(ggplot2)
library(gridExtra)
library(plotly)
library(dplyr)
library(viridis)
library(purrr)
library(stringr)
library(cowplot)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

# Function to parse folder names and extract threshold/predictor info
parse_folder_info <- function(folder_path) {
   folder_name <- basename(folder_path)
   
   # Capture quantile, actual value, and predictors in one go
   m <- stringr::str_match(
      folder_name,
      "_threshq([0-9.]+)_([0-9.]+)_(.+)$"
   )
   # m[ ,2] = quantile, m[ ,3] = actual value, m[ ,4] = predictors
   
   list(
      folder      = folder_path,
      threshold   = as.numeric(m[ ,2]),
      actual      = as.numeric(m[ ,3]),
      predictors  = m[ ,4],
      folder_name = folder_name
   )
}

# Function to load model data from directories
load_threshold_data <- function(base_path, folder_pattern, predictor_combo = NULL) {
   # find matching directories
   all_dirs <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
   matching_dirs <- all_dirs[stringr::str_detect(basename(all_dirs), folder_pattern)]
   
   if (length(matching_dirs) == 0) {
      stop("No directories found matching pattern: ", folder_pattern)
   }
   
   # parse folder metadata
   folder_info <- purrr::map_dfr(matching_dirs, parse_folder_info)
   
   # optional predictor filtering
   if (!is.null(predictor_combo)) {
      folder_info <- folder_info[folder_info$predictors == predictor_combo, ]
      if (nrow(folder_info) == 0) {
         stop("No directories found with predictor combination: ", predictor_combo)
      }
   }
   
   loaded <- vector("list", length = nrow(folder_info))
   
   for (i in seq_len(nrow(folder_info))) {
      data_path  <- file.path(folder_info$folder[i], "model_data_with_predictions.rds")
      model_path <- file.path(folder_info$folder[i], "logistic_model.rds")
      
      if (!file.exists(data_path)) {
         warning("Data file not found: ", data_path)
         next
      }
      if (!file.exists(model_path)) {
         warning("Model file not found: ", model_path)
         next
      }
      
      # load both objects
      model_data  <- readRDS(data_path)
      logistic_fit <- readRDS(model_path)
      
      # store tibble + model + metadata together
      loaded[[i]] <- list(
         data        = model_data,
         model       = logistic_fit,
         threshold   = folder_info$threshold[i],
         actual      = folder_info$actual[i],
         predictors  = folder_info$predictors[i],
         folder_name = folder_info$folder_name[i]
      )
   }
   
   # remove failed loads
   loaded <- purrr::compact(loaded)
   if (length(loaded) == 0) {
      stop("No valid data/model pairs could be loaded.")
   }
   
   # sort by threshold
   loaded <- loaded[order(vapply(loaded, function(x) x$threshold, numeric(1)))]
   
   loaded
}

# =============================================================================
# MULTI-PANEL THRESHOLD GRID
# =============================================================================
create_threshold_grid_from_dirs <- function(data_path, folder_pattern, 
                                            predictor_combo = NULL,
                                            pred1_col = "predictor1", 
                                            pred2_col = "predictor2",
                                            prob_col = "predicted_prob",
                                            actual_col = "actual_exceedance") {
   
   # Load data from directories
   #data_list <- load_threshold_data(base_path, folder_pattern, predictor_combo)
   data_list <- readRDS(data_path)
   
   # Filter for specific predictor combination if provided
   if (!is.null(predictor_combo)) {
      data_list <- Filter(function(run) {
         pred_vars <- run$metadata$predictor_vars
         # Create the combination string from the predictor_vars vector
         combo_string <- paste(pred_vars, collapse = "_")
         return(grepl(predictor_combo, combo_string, fixed = TRUE))
      }, data_list)
   }
   
   library(ggplot2)
   library(dplyr)
   library(stringr)
   library(patchwork)
   library(gridExtra)
   library(grid)
   
   create_panel <- function(entry,
                            pred1_col,
                            pred2_col,
                            prob_breaks = seq(0, 1, by = 0.1)) {
      
      df    <- entry$data
      model <- entry$model
      threshold <- entry$metadata$threshold_quantile
      actual    <- entry$metadata$threshold_value
      predictors <- entry$metadata$predictor_vars
      
      # safety check
      if (!all(c(pred1_col, pred2_col) %in% names(df))) {
         stop("Missing predictor columns in data frame.")
      }
      
      # grid for interpolation
      grid_df <- expand.grid(
         x = seq(min(df[[pred1_col]], na.rm = TRUE),
                 max(df[[pred1_col]], na.rm = TRUE), length.out = 100),
         y = seq(min(df[[pred2_col]], na.rm = TRUE),
                 max(df[[pred2_col]], na.rm = TRUE), length.out = 100)
      )
      names(grid_df) <- c(pred1_col, pred2_col)
      
      # Fill other model vars with mean if needed
      model_vars <- all.vars(formula(model))[-1]
      for (v in setdiff(model_vars, names(grid_df))) {
         grid_df[[v]] <- mean(df[[v]], na.rm = TRUE)
      }
      
      # Optional: handle DayOfYear features if present
      if ("DayOfYear" %in% c(pred1_col, pred2_col, model_vars)) {
         if (!"DayOfYear_sin" %in% names(grid_df) && "DayOfYear_sin" %in% names(df))
            grid_df$DayOfYear_sin <- sin(2 * pi * grid_df$DayOfYear / 365.25)
         if (!"DayOfYear_cos" %in% names(grid_df) && "DayOfYear_cos" %in% names(df))
            grid_df$DayOfYear_cos <- cos(2 * pi * grid_df$DayOfYear / 365.25)
      }
      
      # predict exceedance probability
      grid_df$pred_prob <- predict(model, newdata = grid_df, type = "response")
      
      # Plot
      ggplot() +
         geom_contour_filled(
            data = grid_df,
            aes_string(x = pred1_col, y = pred2_col, z = "pred_prob"),
            breaks = prob_breaks,
            alpha = 0.5
         ) +
         scale_fill_viridis_d(name = "Exceedance\nProbability", drop = FALSE) +
         geom_contour(
            data = grid_df,
            aes_string(x = pred1_col, y = pred2_col, z = "pred_prob"),
            breaks = prob_breaks,
            color = "black", linewidth = 0.3, alpha = 0.8
         ) +
         geom_point(
            data = df,
            aes_string(x = pred1_col, y = pred2_col,
                       color = "as.factor(actual_exceedance)"),
            size = 1.2, alpha = 0.5
         ) +
         scale_color_manual(
            values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
            name   = "Observed",
            labels = c("Below", "Above")
         ) +
         labs(
            title = sprintf("Quantile: %.2f  |  Actual: %.2f", threshold, actual),
            x = str_replace_all(pred1_col, "_", " "),
            y = str_replace_all(pred2_col, "_", " ")
         ) +
         theme_minimal() +
         theme(
            plot.title   = element_text(hjust = 0.5, size = 12, face = 'bold'),
            axis.title   = element_text(size = 10),
            legend.position = "none"
         )
   }
   
   
   panels <- map(
      data_list,
      ~{
         # split predictors string into two names
         preds <- str_split(.x$metadata$predictor_vars, "_", simplify = TRUE)
         pred1 <- preds[1]
         pred2 <- preds[2]
         
         create_panel(.x,
                      pred1_col = pred1,
                      pred2_col = pred2,
                      prob_breaks = seq(0, 1, by = 0.1))
      }
   )
   
   # Calculate grid dimensions
   n_panels <- length(panels)
   n_cols <- ceiling(sqrt(n_panels))
   n_rows <- ceiling(n_panels / n_cols)
   
   legend_plot <- {
      # Use first entry to create legend
      df <- data_list[[1]]
      df_data <- df$data
      
      # Get predictor names from metadata
      pred_vars <- df$metadata$predictor_vars
      pred1_col <- pred_vars[1]
      pred2_col <- pred_vars[2]
      
      # small grid for the contour legend 
      grid_df <- expand.grid(
         x = seq(min(df_data[[pred1_col]], na.rm = TRUE),
                 max(df_data[[pred1_col]], na.rm = TRUE), length.out = 25),
         y = seq(min(df_data[[pred2_col]], na.rm = TRUE),
                 max(df_data[[pred2_col]], na.rm = TRUE), length.out = 25)
      )
      names(grid_df) <- c(pred1_col, pred2_col)
      grid_df$pred_prob <- seq(0, 1, length.out = nrow(grid_df))  # dummy gradient
      
      ggplot() +
         # Contour fill for probability legend
         geom_contour_filled(
            data = grid_df,
            aes(x = .data[[pred1_col]], y = .data[[pred2_col]], z = pred_prob),
            breaks = seq(0, 1, by = 0.1)
         ) +
         scale_fill_viridis_d(
            name = "Predicted Probability",
            drop = FALSE
         ) +
         
         # Points for observed exceedance legend
         geom_point(
            data = df_data,
            aes(x = .data[[pred1_col]], y = .data[[pred2_col]],
                color = factor(.data$actual_exceedance)),
            alpha = 0.0    # invisible, just for legend keys
         ) +
         scale_color_manual(
            values = c("FALSE" = "blue", "TRUE" = "red"),
            name   = "Observed",
            labels = c("Below Threshold", "Above Threshold")
         ) +
         
         theme_void() +
         theme(
            legend.position = "right",
            legend.box = "vertical",
            legend.direction = 'vertical', 
            legend.title = element_text(size = 14, face = 'bold'),
            legend.text = element_text(size = 13)
         )
   }
   
   g <- ggplotGrob(legend_plot)
   
   legend_index <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
   legend <- g$grobs[[legend_index]]
   
   title_grob <- textGrob(
      "Logistic Regression Contours",
      gp = gpar(fontsize = 16, fontface = "bold"),
      just = "left"
   )
   
   panel_grid <- arrangeGrob(
      grobs = panels,
      ncol = n_cols
   )
   
   # Combine panels + legend in 2 columns
   main_grid <- arrangeGrob(
      panel_grid,      # left
      legend,          # right
      ncol = 2,
      widths = c(0.85, 0.15)  # adjust relative widths
   )
   
   # Stack title above the main grid
   grid_plot <- arrangeGrob(
      title_grob,
      main_grid,
      ncol = 1,
      heights = c(0.05, 0.95)  # title vs main plot
   )
   
   return(grid_plot)
}

# =============================================================================
# Logistic Regression Performance Plots
# =============================================================================
# Adapted function for single model performance evaluation
plot_logistic_performance <- function(data, 
                                      model_name = "Logistic Model",
                                      prob_threshold = 0.5,
                                      salinity_threshold = NULL) {
   
   # Extract threshold from data if not provided
   if (is.null(salinity_threshold)) {
      # Infer threshold from data - find minimum salinity where actual_exceedance is TRUE
      salinity_threshold <- data %>%
         filter(actual_exceedance == TRUE) %>%
         pull(Salinity) %>%
         min(na.rm = TRUE)
   }
   
   # Calculate basic confusion matrix metrics
   confusion_metrics <- data %>%
      summarise(
         # True/False Positives/Negatives
         TP = sum(actual_exceedance == TRUE & exceedance_probability > prob_threshold, na.rm = TRUE),
         TN = sum(actual_exceedance == FALSE & exceedance_probability <= prob_threshold, na.rm = TRUE),
         FP = sum(actual_exceedance == FALSE & exceedance_probability > prob_threshold, na.rm = TRUE),
         FN = sum(actual_exceedance == TRUE & exceedance_probability <= prob_threshold, na.rm = TRUE),
         
         # Derived metrics
         precision = ifelse(TP + FP > 0, TP / (TP + FP), 0),
         sensitivity = ifelse(TP + FN > 0, TP / (TP + FN), 0),  # Recall
         specificity = ifelse(TN + FP > 0, TN / (TN + FP), 0),
         f1_score = ifelse(precision + sensitivity > 0, 2 * (precision * sensitivity) / (precision + sensitivity), 0),
         accuracy = (TP + TN) / (TP + TN + FP + FN),
         
         # False rates
         false_positive_rate = ifelse(TN + FP > 0, FP / (TN + FP), 0),
         false_negative_rate = ifelse(TP + FN > 0, FN / (TP + FN), 0),
         
         # Probabilistic metrics
         brier_score = mean((exceedance_probability - as.numeric(actual_exceedance))^2, na.rm = TRUE),
         log_loss = -mean(
            as.numeric(actual_exceedance) * log(pmax(exceedance_probability, 1e-15)) + 
               (1 - as.numeric(actual_exceedance)) * log(pmax(1 - exceedance_probability, 1e-15)),
            na.rm = TRUE
         )
      ) %>%
      mutate(model = model_name)
   
   # Calculate calibration data
   calibration_data <- data %>%
      mutate(prob_bin = cut(exceedance_probability, 
                            breaks = seq(0, 1, 0.05), 
                            include.lowest = TRUE)) %>%
      group_by(prob_bin) %>%
      summarise(
         predicted_prob = mean(exceedance_probability, na.rm = TRUE),
         observed_freq = mean(as.numeric(actual_exceedance), na.rm = TRUE),
         n_obs = n(),
         .groups = "drop"
      ) %>%
      filter(n_obs >= 10)  # Only bins with sufficient observations
   
   # Plot 1: Key metrics
   key_metrics_plot <- function() {
      key_metrics <- confusion_metrics %>%
         select(precision, sensitivity, f1_score, false_positive_rate, false_negative_rate) %>%
         pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
         mutate(metric_label = case_when(
            metric == 'false_positive_rate' ~ 'False Positive Rate',
            metric == 'false_negative_rate' ~ 'False Negative Rate',
            metric == 'sensitivity' ~ 'Recall/Sensitivity',
            metric == 'precision' ~ 'Precision',
            metric == "f1_score" ~ "F1 Score"
         )) %>%
         mutate(metric_label = factor(metric_label,
                                      levels = c('False Positive Rate', 'False Negative Rate', 
                                                 'Recall/Sensitivity', 'Precision', 'F1 Score')))
      
      ggplot(key_metrics, aes(x = metric_label, y = value, fill = metric_label)) +
         geom_col(width = 0.7) +
         geom_text(aes(label = paste0(round(value * 100, 1), "%")),
                   vjust = -0.3, size = 4, fontface = "bold") +
         scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
         scale_fill_manual(
            values = c("False Positive Rate" = "darkred",
                       'False Negative Rate' = 'orange',
                       "Recall/Sensitivity" = "steelblue",
                       "Precision" = "darkgreen", 
                       "F1 Score" = "purple")
         ) +
         labs(title = paste("Classification Performance:", model_name),
              subtitle = paste("Confusion matrix metrics at", prob_threshold, "probability threshold"),
              x = "Metric", y = "Performance") +
         theme_bw(base_size = 11) +
         theme(legend.position = "none",
               axis.text.x = element_text(angle = 45, hjust = 1))
   }
   
   # Plot 2: Probabilistic performance metrics  
   prob_metrics_plot <- function() {
      prob_data <- data.frame(
         metric = c("Accuracy", "Brier Score", "Log Loss"),
         value = c(confusion_metrics$accuracy, confusion_metrics$brier_score, confusion_metrics$log_loss),
         better = c("Higher", "Lower", "Lower")
      )
      
      ggplot(prob_data, aes(x = metric, y = value, fill = better)) +
         geom_col(width = 0.6) +
         geom_text(aes(label = round(value, 3)), vjust = -0.3, size = 4, fontface = "bold") +
         scale_fill_manual(values = c("Higher" = "darkgreen", "Lower" = "darkred")) +
         labs(title = "Additional Performance Metrics",
              subtitle = "Accuracy (higher better), Brier Score & Log Loss (lower better)",
              x = "Metric", y = "Score") +
         theme_bw(base_size = 11) +
         theme(legend.position = "none")
   }
   
   # Plot 3: Calibration plot
   calibration_plot <- function() {
      ggplot(calibration_data, aes(x = predicted_prob, y = observed_freq)) +
         geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.7, size = 1) +
         geom_point(aes(size = n_obs), color = "steelblue", alpha = 0.7) +
         geom_smooth(method = "loess", se = TRUE, color = "darkred", size = 1.2) +
         scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
         scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
         labs(
            title = "Model Calibration",
            subtitle = "Perfect calibration follows diagonal line. Points sized by # observations.",
            x = "Predicted Probability",
            y = "Observed Frequency",
            size = "# Observations"
         ) +
         theme_bw(base_size = 11) +
         theme(legend.position = "bottom")
   }
   
   # Plot 4: Probability distributions
   prob_dist_plot <- function() {
      plot_data <- data %>%
         mutate(outcome = ifelse(actual_exceedance, "Exceedance", "No Exceedance"))
      
      ggplot(plot_data, aes(x = exceedance_probability, fill = outcome)) +
         geom_histogram(alpha = 0.7, position = "identity", bins = 50) +
         geom_vline(xintercept = prob_threshold, linetype = "dashed", color = "red", size = 1) +
         scale_fill_manual(values = c("No Exceedance" = "lightblue", "Exceedance" = "coral")) +
         labs(title = "Predicted Probability Distribution",
              subtitle = paste("Red line shows decision threshold (", prob_threshold, ")"),
              x = "Predicted Probability",
              y = "Count",
              fill = "Actual Outcome") +
         theme_bw(base_size = 11) +
         theme(legend.position = "bottom")
   }
   
   # Generate all plots
   key_plot <- key_metrics_plot()
   prob_plot <- prob_metrics_plot()
   cal_plot <- calibration_plot()
   dist_plot <- prob_dist_plot()
   
   # Combine plots
   combined_plot <- (key_plot | prob_plot) / (cal_plot | dist_plot) + 
      plot_layout(heights = c(1, 1))
   
   return(list(
      combined_plot = combined_plot,
      key_metrics_plot = key_plot, 
      prob_metrics_plot = prob_plot,
      calibration_plot = cal_plot,
      distribution_plot = dist_plot,
      metrics_table = confusion_metrics,
      calibration_data = calibration_data
   ))
}


# =============================================================================
# Single Model Time Series Plot
# =============================================================================
plot_logistic_timeseries <- function(data, 
                                     start_date, 
                                     end_date, 
                                     model_name = "Logistic Model",
                                     salinity_threshold = NULL) {
   
   # Extract threshold from data if not provided
   if (is.null(salinity_threshold)) {
      salinity_threshold <- data %>%
         filter(actual_exceedance == TRUE) %>%
         pull(Salinity) %>%
         min(na.rm = TRUE)
   }
   
   # Filter data for specified time period
   period_data <- data %>%
      filter(Date >= as.POSIXct(start_date) & 
                Date <= as.POSIXct(end_date))
   
   # Generate title
   title_text <- paste0(model_name, " - ", 
                        format(as.Date(start_date), "%b %Y"), " to ",
                        format(as.Date(end_date), "%b %Y"))
   
   # Create the plot
   ggplot(period_data, aes(x = Date)) +
      # Predicted probability
      geom_line(aes(y = exceedance_probability), color = "steelblue", size = 0.8) +
      
      # Raw salinity (rescaled for secondary axis)
      geom_line(aes(y = Salinity / max(Salinity, na.rm = TRUE)), 
                color = "grey60", linetype = "solid", alpha = 0.7) +
      
      # Threshold line
      geom_hline(yintercept = salinity_threshold / max(period_data$Salinity, na.rm = TRUE), 
                 color = "red", linetype = "dashed", alpha = 0.8) +
      
      # True exceedances
      geom_point(
         data = period_data %>% filter(actual_exceedance == TRUE),
         aes(y = Salinity / max(Salinity, na.rm = TRUE)),
         color = "red", size = 1.5, alpha = 0.8
      ) +
      
      scale_y_continuous(
         name = "Exceedance Probability",
         limits = c(0, 1),
         sec.axis = sec_axis(~ . * max(period_data$Salinity, na.rm = TRUE), 
                             name = "Salinity (psu)")
      ) +
      
      labs(
         x = "Date",
         title = title_text,
         subtitle = paste0("Blue: predicted probability | Grey: observed salinity | Red points: actual exceedances (threshold: ", 
                           round(salinity_threshold, 2), ")")
      ) +
      theme_bw(base_size = 12) +
      theme(
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 45, hjust = 1)
      )
}

# =============================================================================
# Matrix Contour Plot
# =============================================================================
create_matrix_plot <- function(data, model, x_var, y_var, prob_breaks = seq(0, 1, by = 0.1)) {
   library(ggnewscale)
   
   model_vars <- all.vars(formula(model))[-1]
   
   # Smart sample
   sampled_data <- data %>%
      mutate(is_oct_2016 = (Year == 2016 & Month == 10)) %>%
      group_by(is_oct_2016) %>%
      sample_n(
         size = ifelse(first(is_oct_2016), 
                       min(n(), sum(data$Year == 2016 & data$Month == 10)), 
                       min(n(), 5000)),
         replace = FALSE
      ) %>%
      ungroup() %>%
      select(-is_oct_2016)
   
   # Prediction grid
   grid_df <- expand.grid(
      x = seq(min(sampled_data[[x_var]], na.rm = TRUE), 
              max(sampled_data[[x_var]], na.rm = TRUE), length.out = 100),
      y = seq(min(sampled_data[[y_var]], na.rm = TRUE), 
              max(sampled_data[[y_var]], na.rm = TRUE), length.out = 100)
   )
   names(grid_df) <- c(x_var, y_var)
   
   for (v in model_vars) {
      if (!v %in% names(grid_df)) {
         grid_df[[v]] <- mean(data[[v]], na.rm = TRUE)
      }
   }
   
   if ("DayOfYear" %in% c(x_var, y_var) || "DayOfYear" %in% model_vars) {
      if (!"DayOfYear_sin" %in% names(grid_df)) {
         grid_df$DayOfYear_sin <- sin(2 * pi * grid_df$DayOfYear / 365.25)
      }
      if (!"DayOfYear_cos" %in% names(grid_df)) {
         grid_df$DayOfYear_cos <- cos(2 * pi * grid_df$DayOfYear / 365.25)
      }
   }
   
   grid_df$pred_prob <- predict(model, newdata = grid_df, type = "response")
   
   
   # Plot with *fixed probability breaks*
   p <- ggplot() +
      # Contour fill (first fill scale)
      geom_contour_filled(
         data = grid_df,
         aes_string(x = x_var, y = y_var, z = "pred_prob"),
         breaks = prob_breaks,
         alpha = 0.5
      ) +
      scale_fill_viridis_d(
         name = "Exceedance\nProbability",
         drop = FALSE
      ) +
      
      # Contour lines
      geom_contour(
         data = grid_df,
         aes_string(x = x_var, y = y_var, z = "pred_prob"),
         breaks = prob_breaks,
         color = "black",
         linewidth = 0.3
      ) +
      
      # Regular points (not Oct 2016)
      geom_point(
         data = subset(sampled_data, !(Year == 2016 & Month == 10)),
         aes(x = !!sym(x_var), y = !!sym(y_var), color = as.factor(actual_exceedance)),
         size = 1.5, alpha = 0.8
      ) +
      scale_color_manual(
         values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
         name = "Actual Exceedance",
         labels = c("No", "Yes")
      ) +
      
      # Start a new fill scale for Oct 2016 points
      ggnewscale::new_scale_fill() +
      
      # Oct 2016 points with exceedance color + black outline
      geom_point(
         data = subset(sampled_data, Year == 2016 & Month == 10),
         aes(x = !!sym(x_var), y = !!sym(y_var), fill = as.factor(actual_exceedance)),
         shape = 21, size = 1.5, stroke = 0.6, color = "black", alpha = 0.8
      ) +
      scale_fill_manual(
         values = c("FALSE" = "#2166ac", "TRUE" = "#d73027"),
         name = "Actual Exceedance",
         labels = c("No", "Yes")
      ) +
      
      theme_minimal() +
      theme(
         panel.grid.minor = element_blank(),
         panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
         panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5),
         axis.title = element_text(size = 10),
         axis.text = element_text(size = 8),
         legend.position = "bottom",
         legend.title = element_text(size = 11, face = "bold"),
         legend.text = element_text(size = 10)
      ) +
      labs(
         x = gsub("_", " ", x_var),
         y = gsub("_", " ", y_var)
      )
   
   
   return(p)
}


# =============================================================================
# Multi-panel Model Plot with Predictors
# =============================================================================
create_salinity_exceedance_plot <- function(data, 
                                            years = NULL,
                                            months = NULL, 
                                            threshold = 1.0,
                                            plot_title = NULL,
                                            predictor_vars = c('Norm_InflowDeficit', 'Norm_PowDischarge'),
                                            single_group = NULL) {
   
   # Default filtering: if no years/months specified, use all data
   filtered_data <- data
   
   # Apply year filter if specified
   if (!is.null(years)) {
      filtered_data <- filtered_data %>% filter(Year %in% years)
   }
   
   # Apply month filter if specified
   if (!is.null(months)) {
      filtered_data <- filtered_data %>% filter(Month %in% months)
   }
   
   # Default title if none provided
   if (is.null(plot_title)) {
      year_text <- if (is.null(years)) "All Years" else paste(years, collapse = ", ")
      month_text <- if (is.null(months)) "All Months" else paste(month.name[months], collapse = "/")
      plot_title <- paste0("Simple Logistic Regression (Threshold: ", threshold, 
                           ") - ", year_text, ", ", month_text)
   }
   
   # Panel 1: Predicted Exceedance Probability
   p1 <- ggplot(filtered_data, aes(x = Date, y = exceedance_probability)) +
      geom_line() +
      labs(
         title = "A) Predicted Exceedance Probability",
         y = "Probability",
         x = NULL
      ) +
      theme_minimal() + 
      theme(axis.ticks.x = element_blank()) + 
      theme(axis.text.x = element_blank())
   
   # Handle legend based on single_group parameter
   if (!is.null(single_group)) {
      p1 <- p1 + theme(legend.position = "none")
   } else {
      p1 <- p1 +
         theme(legend.position = "bottom", legend.title = element_blank()) +
         guides(color = guide_legend(nrow = 1))
   }
   
   # Panel 2: Observed Salinity
   # Create exceedance indicators based on threshold
   filtered_data$threshold_exceedance <- filtered_data$Salinity > threshold
   
   p2 <- ggplot(filtered_data, aes(x = Date, y = Salinity)) +
      geom_line(color = "darkgrey") +
      # Add threshold line
      geom_hline(yintercept = threshold, 
                 color = "red", linetype = "dashed", alpha = 0.7) +
      # Highlight exceedances (if actual_exceedance column exists)
      {if ("actual_exceedance" %in% names(filtered_data)) {
         geom_point(
            data = filtered_data %>% filter(actual_exceedance == TRUE),
            aes(y = Salinity), color = "red", size = 0.8, alpha = 0.8
         )
      }} +
      # Highlight threshold exceedances
      geom_point(data = filtered_data %>% filter(threshold_exceedance), 
                 aes(y = Salinity), color = 'darkred', size = 0.8, alpha = 0.8) +
      labs(
         title = paste0("B) Observed Salinity (Threshold: ", threshold, ")"),
         y = "Salinity (psu)",
         x = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "none") + 
      theme(axis.ticks.x = element_blank()) + 
      theme(axis.text.x = element_blank())
   
   # Panel 3: Predictor Variables
   predictor_plots <- list()
   
   # Variable labels mapping
   var_labels <- c(
      "Norm_InflowDeficit" = "Normalized Inflow Deficit",
      "Norm_PowDischarge" = "Normalized Discharge",
      "DayOfYear" = "Day of Year"
   )
   
   for (i in seq_along(predictor_vars)) {
      var_name <- predictor_vars[i]
      
      # Check if variable exists in data
      if (!var_name %in% names(filtered_data)) {
         warning(paste("Variable", var_name, "not found in data. Skipping."))
         next
      }
      
      y_label <- ifelse(var_name %in% names(var_labels), 
                        var_labels[var_name], var_name)
      
      p_pred <- ggplot(filtered_data, aes(x = Date, y = !!sym(var_name))) +
         geom_line(color = "darkgreen") +
         labs(
            title = paste0(LETTERS[i + 2], ") ", y_label),
            y = y_label,
            x = if (i == length(predictor_vars)) "Date" else NULL
         ) +
         theme_minimal() +
         theme(legend.position = "none") + 
         theme(axis.text.x = element_blank())
      
      predictor_plots[[i]] <- p_pred
   }
   
   # Remove NULL plots (from missing variables)
   predictor_plots <- predictor_plots[!sapply(predictor_plots, is.null)]
   predictor_plots[[2]] + theme(axis.text.x = element_text(size = 14, face = 'bold')) + 
      theme(axis.ticks.x = element_text(size = 12, face = 'bold'))
   
   # Combine all plots
   if (length(predictor_plots) == 0) {
      combined_plot <- p1 / p2
   } else if (length(predictor_plots) == 1) {
      combined_plot <- p1 / p2 / predictor_plots[[1]]
   } else if (length(predictor_plots) == 2) {
      combined_plot <- p1 / p2 / predictor_plots[[1]] / predictor_plots[[2]]
   } else {
      # For more than 2 predictors, arrange them in a grid
      pred_combined <- wrap_plots(predictor_plots, ncol = 2)
      combined_plot <- p1 / p2 / pred_combined
   }
   
   # Add overall title
   combined_plot <- combined_plot + 
      plot_annotation(title = plot_title,
                      theme = theme(
                         plot.title = element_text(size = 16, face = 'bold')
                      ))
   
   return(combined_plot)
}

# =============================================================================
# Plot all Threshold Exceedance Probabilities with Salinity
# =============================================================================
all_threshold_exceedance_probs <- function(data) {
   plot <- ggplot(final_data) + 
      geom_line(aes(x = Date, y = exceedance_probability,
                    color = factor(threshold)), na.rm = TRUE, size = 0.7) +
      geom_line(aes(x = Date, y = Salinity / max(Salinity, na.rm = TRUE),
                    color = "Observed Salinity"),
                linetype = 1, size = 1.5) +
      scale_y_continuous(
         name = "Exceedance Probability",
         limits = c(0, 1),
         sec.axis = sec_axis(~ . * max(final_data$Salinity, na.rm = TRUE),
                             name = "Salinity (psu)")
      ) +
      scale_color_manual(
         name = "Threshold (psu)",
         values = c(
            "Observed Salinity" = "red",
            setNames(
               viridis(length(unique(final_data$threshold))),
               as.character(sort(unique(final_data$threshold)))
            )
         )
      ) +
      labs(
         x = "Date",
         y = "Value",
         title = "Predicted Exceedance Probability and Observed Salinity by Threshold"
      ) +
      theme_bw() +
      theme(legend.title = element_text(face = "bold", size = 14),
            legend.position = 'bottom',
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = 'bold'),
            axis.text = element_text(size = 12), 
            plot.title = element_text(size = 16, face = 'bold'),
            strip.text = element_text(size = 14)) + 
      guides(color = guide_legend(
         override.aes = list(
            size = c(rep(1.5, length(unique(final_data$threshold))), 2),
            linetype = c(rep(1, length(unique(final_data$threshold))), 1)
         )
      )) + 
      facet_wrap(~Year, scales = 'free_x') + 
      scale_x_date(date_labels = '%b',
                   date_breaks = '2 months')
   
   return(plot) 
   
}

# =============================================================================
# Plot all thresholds with salinity on specific window
# =============================================================================
all_threshold_window_plot <- function(data) {
   plot <- ggplot(final_data) + 
      # exceedance probabilities for each threshold
      geom_line(aes(x = Date, y = exceedance_probability,
                    color = factor(threshold)), na.rm = TRUE, size = 0.7) +
      # observed salinity
      geom_line(aes(x = Date, y = Salinity / max(Salinity, na.rm = TRUE),
                    color = "Observed Salinity"),
                linetype = 1, size = 1.5) +
      scale_y_continuous(
         name = "Exceedance Probability",
         limits = c(0, 1),
         sec.axis = sec_axis(~ . * max(final_data$Salinity, na.rm = TRUE),
                             name = "Salinity (psu)")
      ) +
      scale_x_date(date_labels = '%b %Y',
                   date_breaks = '2 months',
                   limits = as.Date(c("2016-04-06", "2016-11-23"))) +
      scale_color_manual(
         name = "Threshold (psu)",
         values = c(
            "Observed Salinity" = "red",
            setNames(
               viridis(length(unique(final_data$threshold))),
               as.character(sort(unique(final_data$threshold)))
            )
         )
      ) +
      labs(
         x = "Date",
         y = "Value",
         title = "Predicted Exceedance Probability and Observed Salinity by Threshold"
      ) +
      theme_bw() +
      theme(legend.title = element_text(face = "bold", size = 14),
            legend.position = 'bottom',
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = 'bold'),
            axis.text = element_text(size = 12), 
            plot.title = element_text(size = 16, face = 'bold'),
            strip.text = element_text(size = 14)) + 
      guides(color = guide_legend(
         override.aes = list(
            size = c(rep(1.5, length(unique(final_data$threshold))), 2),
            linetype = c(rep(1, length(unique(final_data$threshold))), 1)
         )
      )) 
   
   return(plot) 
}
