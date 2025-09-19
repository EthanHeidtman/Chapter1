# =============================================================================
# Script Name:    FaceLogisticMatrixPlot.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    
# =============================================================================

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
# APPROACH 2: MULTI-PANEL THRESHOLD GRID  
# =============================================================================

create_threshold_grid_from_dirs <- function(base_path, folder_pattern, 
                                            predictor_combo = NULL,
                                            pred1_col = "predictor1", 
                                            pred2_col = "predictor2",
                                            prob_col = "predicted_prob",
                                            actual_col = "actual_exceedance") {
   
   # Load data from directories
   data_list <- load_threshold_data(base_path, folder_pattern, predictor_combo)
   
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
      # entry is a single element of data_list: list(data=..., model=..., threshold=..., etc.)
      df    <- entry$data
      model <- entry$model
      threshold <- entry$threshold
      actual    <- entry$actual
      predictors <- entry$predictors
      
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
            size = 1.2, alpha = 0.6
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
         preds <- str_split(.x$predictors[1], "_", simplify = TRUE)
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
      # Use one tibble to pull the predictors and sample for the point legend
      df_data <- data_list[[1]]$data
      df <- data_list[[1]]
      
      # split predictor names
      first_preds <- stringr::str_split_fixed(df$predictors[1],
                                              "_(?=[^_]+$)", n = 2)
      pred1_col <- first_preds[1]
      pred2_col <- first_preds[2]
      
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
            alpha = 1.0    # invisible, just for legend keys
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
# APPROACH 3: 3D SURFACE VISUALIZATION
# =============================================================================

create_3d_surface_from_dirs <- function(base_path, folder_pattern,
                                        predictor_combo = NULL,
                                        pred1_col = "predictor1",
                                        pred2_col = "predictor2", 
                                        prob_col = "exceedance_probability",
                                        actual_col = "actual_exceedance",
                                        surface_opacity = 0.7) {
   
   # Load data from directories
   data_list <- load_threshold_data(base_path, folder_pattern, predictor_combo)
   thresholds <- sapply(data_list, function(x) x$threshold[1])
   
   # Initialize 3D plot
   p <- plot_ly()
   
   # Color palette for different thresholds
   colors <- viridis(length(data_list), option = "plasma")
   
   # Add surface for each threshold
   for(i in 1:length(data_list)) {
      data <- data_list[[i]]
      threshold <- thresholds[i]
      
      # Create grid for surface (interpolate if necessary)
      # Assuming your data already has a regular grid structure
      pred1_vals <- sort(unique(data[[pred1_col]]))
      pred2_vals <- sort(unique(data[[pred2_col]]))
      
      # Create matrix of probabilities
      prob_matrix <- matrix(NA, nrow = length(pred1_vals), ncol = length(pred2_vals))
      
      for(j in 1:nrow(data)) {
         p1_idx <- which(pred1_vals == data[[pred1_col]][j])
         p2_idx <- which(pred2_vals == data[[pred2_col]][j]) 
         prob_matrix[p1_idx, p2_idx] <- data[[prob_col]][j]
      }
      
      # Add surface
      p <- p %>% add_surface(
         x = pred1_vals,
         y = pred2_vals,
         z = prob_matrix,
         name = paste("Threshold", threshold),
         opacity = surface_opacity,
         colorscale = list(c(0, colors[i]), c(1, colors[i])),
         showscale = (i == 1)  # Only show scale for first surface
      )
   }
   
   # Add observed exceedance points at base
   for(i in 1:length(data_list)) {
      data <- data_list[[i]]
      threshold <- thresholds[i]
      
      # Only plot exceedances (actual_col == 1)
      exceedances <- data[data[[actual_col]] == 1, ]
      
      if(nrow(exceedances) > 0) {
         p <- p %>% add_markers(
            x = exceedances[[pred1_col]],
            y = exceedances[[pred2_col]],
            z = rep(0, nrow(exceedances)),  # Project to base
            name = paste("Exceedances T=", threshold),
            marker = list(size = 3, color = colors[i], symbol = "circle"),
            showlegend = TRUE
         )
      }
   }
   
   # Layout
   p <- p %>% layout(
      scene = list(
         xaxis = list(title = str_replace_all(pred1_col, "_", " ")),
         yaxis = list(title = str_replace_all(pred2_col, "_", " ")),
         zaxis = list(title = "Predicted Probability")
      ),
      title = list(
         text = paste("3D Threshold Evolution:",
                      unique(sapply(data_list, function(x) x$predictors[1]))),
         x = 0.5
      )
   )
   
   return(p)
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

# Function to explore available directories
explore_directories <- function(base_path, pattern = "LR_thresh") {
   all_dirs <- list.dirs(base_path, recursive = TRUE, full.names = FALSE)
   matching_dirs <- all_dirs[str_detect(all_dirs, pattern)]
   
   if(length(matching_dirs) == 0) {
      cat("No directories found matching pattern:", pattern, "\n")
      return(invisible())
   }
   
   # Parse and summarize
   info <- map_dfr(file.path(base_path, matching_dirs), parse_folder_info)
   
   cat("Found", nrow(info), "matching directories:\n\n")
   
   # Group by predictor combination
   predictor_groups <- split(info, info$predictors)
   
   for(pred_combo in names(predictor_groups)) {
      cat("Predictor combination:", pred_combo, "\n")
      thresholds <- sort(predictor_groups[[pred_combo]]$threshold)
      cat("  Thresholds:", paste(thresholds, collapse = ", "), "\n\n")
   }
   
   return(info)
}



