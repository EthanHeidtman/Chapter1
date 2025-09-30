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

