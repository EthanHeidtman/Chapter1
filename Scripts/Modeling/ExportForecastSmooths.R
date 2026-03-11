# =============================================================================
# Script Name:    ExportForecastSmooths.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Exports GAM smooth lookup tables for a specified forecast
#                 horizon (lag k). Automatically detects the model formula
#                 and generates one CSV per smooth term based on observed
#                 training data ranges.
#
# Usage:
#   source("Scripts/ExportForecastSmooths.R")
#   export_forecast_smooths(k = 1,  models = models, output_dir = "...")
#   export_forecast_smooths(k = 14, models = models, output_dir = "...")
#
# Output files (written to output_dir/{k}DayForecast/):
#   smooth_discharge.csv     columns: discharge, smooth
#   smooth_wind_east.csv     columns: wind_[u|v], smooth   (if U wind present)
#   smooth_wind_west.csv     columns: wind_[u|v], smooth   (if U wind present)
#   smooth_wind_north.csv    columns: wind_[u|v], smooth   (if V wind present)
#   smooth_wind_south.csv    columns: wind_[u|v], smooth   (if V wind present)
#   smooth_tide.csv          columns: tide_range, smooth   (if tide present)
#   validation_points.csv    5-row cross-check: R predictions vs term assembly
#   smooth_metadata.csv      predictor names, windows, intercept, lag coef
# =============================================================================


# =============================================================================
# Helper: extract predictor names from a fitted GAM object
# =============================================================================

#' Parse a fitted GAM's smooth terms and parametric terms.
#'
#' Returns a named list:
#'   $discharge_var   character or NULL
#'   $wind_var        character or NULL
#'   $wind_type       "U" | "V" | NULL
#'   $wind_dir_var    character or NULL   (the WindDir factor column)
#'   $tide_var        character or NULL
#'   $lag_var         character or NULL   (parametric AR term)
#'   $smooth_names    character vector of smooth column names from type="terms"
parse_gam_structure <- function(gam_obj) {
   
   smooth_labels <- sapply(gam_obj$smooth, function(s) s$label)
   term_names    <- names(gam_obj$model)
   
   # --- Discharge ---
   discharge_var <- term_names[grepl("Discharge", term_names, ignore.case = TRUE)]
   discharge_var <- if (length(discharge_var) > 0) discharge_var[1] else NULL
   
   # --- Wind ---
   # Prefer U (east-west) over V (north-south); pick the one that appears in smooths
   wind_u_var <- term_names[grepl("^RollingU", term_names)]
   wind_v_var <- term_names[grepl("^RollingV", term_names)]
   
   if (length(wind_u_var) > 0) {
      wind_var  <- wind_u_var[1]
      wind_type <- "U"
   } else if (length(wind_v_var) > 0) {
      wind_var  <- wind_v_var[1]
      wind_type <- "V"
   } else {
      wind_var  <- NULL
      wind_type <- NULL
   }
   
   # --- WindDir factor ---
   wind_dir_var <- term_names[grepl("WindDir", term_names)]
   wind_dir_var <- if (length(wind_dir_var) > 0) wind_dir_var[1] else NULL
   
   # --- Tide ---
   tide_var <- term_names[grepl("Tide", term_names, ignore.case = TRUE)]
   tide_var <- if (length(tide_var) > 0) tide_var[1] else NULL
   
   # --- Lag salinity (parametric AR term) ---
   lag_var <- term_names[grepl("LagSalinity|Lag_Salinity|lagsal", term_names,
                               ignore.case = TRUE)]
   # Exclude the response column (first col is always Salinity)
   lag_var <- lag_var[lag_var != "Salinity"]
   lag_var <- if (length(lag_var) > 0) lag_var[1] else NULL
   
   # --- Smooth column names as returned by predict(..., type="terms") ---
   # These are the actual column names we need to index into
   nd_dummy <- gam_obj$model[1, , drop = FALSE]
   terms_mat <- predict(gam_obj, nd_dummy, type = "terms")
   smooth_cols <- colnames(terms_mat)
   
   list(
      discharge_var = discharge_var,
      wind_var      = wind_var,
      wind_type     = wind_type,
      wind_dir_var  = wind_dir_var,
      tide_var      = tide_var,
      lag_var       = lag_var,
      smooth_cols   = smooth_cols
   )
}


# =============================================================================
# Helper: find the smooth column name matching a predictor
# =============================================================================

find_smooth_col <- function(smooth_cols, pattern) {
   matches <- smooth_cols[grepl(pattern, smooth_cols, fixed = FALSE)]
   if (length(matches) == 0) NULL else matches[1]
}


# =============================================================================
# Helper: build a prediction grid holding all vars at mean except one
# =============================================================================

make_grid <- function(gam_obj, structure, vary_var, vary_values,
                      wind_dir_level = NULL) {
   
   # Start from training means for all numeric predictors
   nd <- as.data.frame(lapply(gam_obj$model, function(col) {
      if (is.numeric(col)) mean(col, na.rm = TRUE)
      else if (is.factor(col)) factor(levels(col)[1], levels = levels(col))
      else col[1]
   }))
   nd <- nd[rep(1, length(vary_values)), , drop = FALSE]
   rownames(nd) <- NULL
   
   # Plug in the varying predictor
   nd[[vary_var]] <- vary_values
   
   # Override wind direction if requested
   if (!is.null(wind_dir_level) && !is.null(structure$wind_dir_var)) {
      nd[[structure$wind_dir_var]] <- factor(
         wind_dir_level,
         levels = levels(gam_obj$model[[structure$wind_dir_var]])
      )
   }
   
   nd
}


# =============================================================================
# Main export function
# =============================================================================

#' Export smooth lookup tables for a given forecast horizon.
#'
#' @param k          Integer. Forecast lag in days (e.g. 1, 7, 14).
#' @param models     Named list. Must contain element \code{Lag{k}} with
#'                   sub-element \code{gam_object} (a fitted mgcv::gam).
#' @param output_dir Character. Root output directory. A subdirectory
#'                   \code{{k}DayForecast/} is created automatically.
#' @param n_grid     Integer. Number of grid points per smooth (default 500).
#' @param buffer     Numeric. Fractional buffer beyond observed min/max
#'                   (default 0.05 = 5%).
#' @param seed       Integer. Seed for validation point sampling.
#'
#' @return Invisibly returns a list of the exported data frames.

export_forecast_smooths <- function(k,
                                    models,
                                    output_dir,
                                    n_grid  = 500,
                                    buffer  = 0.05,
                                    seed    = 42) {
   
   # --- Retrieve model ---
   lag_name <- paste0("Lag", k)
   if (!lag_name %in% names(models)) {
      stop(sprintf("models$%s not found. Available: %s",
                   lag_name, paste(names(models), collapse = ", ")))
   }
   gam_obj <- models[[lag_name]]$gam_object
   if (is.null(gam_obj)) {
      stop(sprintf("models$%s$gam_object is NULL", lag_name))
   }
   
   # --- Output directory ---
   subdir <- file.path(output_dir, paste0(k, "DayForecast"))
   dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
   message(sprintf("\n=== Exporting smooths for %d-day forecast → %s ===", k, subdir))
   
   # --- Parse model structure ---
   st <- parse_gam_structure(gam_obj)
   message("  Detected predictors:")
   message("    discharge : ", st$discharge_var %||% "none")
   message("    wind      : ", st$wind_var %||% "none", " (type: ", st$wind_type %||% "none", ")")
   message("    wind dir  : ", st$wind_dir_var %||% "none")
   message("    tide      : ", st$tide_var %||% "none")
   message("    lag sal   : ", st$lag_var %||% "none")
   message("  Smooth columns: ", paste(st$smooth_cols, collapse = ", "))
   
   exported <- list()
   
   # -------------------------------------------------------------------------
   # 1. Discharge smooth
   # -------------------------------------------------------------------------
   if (!is.null(st$discharge_var)) {
      obs   <- gam_obj$model[[st$discharge_var]]
      grid  <- seq(min(obs) * (1 - buffer), max(obs) * (1 + buffer),
                   length.out = n_grid)
      nd    <- make_grid(gam_obj, st, st$discharge_var, grid)
      terms <- predict(gam_obj, nd, type = "terms")
      col   <- find_smooth_col(st$smooth_cols, st$discharge_var)
      
      if (!is.null(col)) {
         df <- data.frame(discharge = grid, smooth = terms[, col])
         path <- file.path(subdir, "smooth_discharge.csv")
         write.csv(df, path, row.names = FALSE)
         exported$discharge <- df
         message("  Wrote: smooth_discharge.csv  (", n_grid, " points, col: '", col, "')")
      } else {
         warning("  Could not find discharge smooth column in type='terms' output")
      }
   }
   
   # -------------------------------------------------------------------------
   # 2. Wind smooths (factor-by interaction: East/West or North/South)
   # -------------------------------------------------------------------------
   if (!is.null(st$wind_var) && !is.null(st$wind_dir_var)) {
      
      obs        <- gam_obj$model[[st$wind_var]]
      wind_grid  <- seq(min(obs) * (1 + buffer * sign(min(obs))),
                        max(obs) * (1 + buffer),
                        length.out = n_grid)
      
      # Determine direction levels from the factor in training data
      dir_levels <- levels(gam_obj$model[[st$wind_dir_var]])
      col_prefix <- if (st$wind_type == "U") "wind_u" else "wind_v"
      
      for (dir in dir_levels) {
         nd    <- make_grid(gam_obj, st, st$wind_var, wind_grid,
                            wind_dir_level = dir)
         terms <- predict(gam_obj, nd, type = "terms")
         
         # Column name pattern: s(RollingU30_1):WindDirEast etc.
         pattern <- paste0(st$wind_var, ".*", dir)
         col     <- find_smooth_col(st$smooth_cols, pattern)
         
         if (!is.null(col)) {
            df        <- data.frame(smooth = terms[, col])
            df[[col_prefix]] <- wind_grid
            df        <- df[, c(col_prefix, "smooth")]
            
            fname <- paste0("smooth_wind_", tolower(dir), ".csv")
            path  <- file.path(subdir, fname)
            write.csv(df, path, row.names = FALSE)
            exported[[paste0("wind_", tolower(dir))]] <- df
            message("  Wrote: ", fname, "  (col: '", col, "')")
         } else {
            warning(sprintf("  Could not find wind smooth column for direction '%s'", dir))
         }
      }
      
   } else if (!is.null(st$wind_var)) {
      # No WindDir factor — single wind smooth
      obs   <- gam_obj$model[[st$wind_var]]
      grid  <- seq(min(obs) * (1 + buffer * sign(min(obs))),
                   max(obs) * (1 + buffer), length.out = n_grid)
      nd    <- make_grid(gam_obj, st, st$wind_var, grid)
      terms <- predict(gam_obj, nd, type = "terms")
      col   <- find_smooth_col(st$smooth_cols, st$wind_var)
      
      if (!is.null(col)) {
         col_prefix <- if (st$wind_type == "U") "wind_u" else "wind_v"
         df   <- data.frame(smooth = terms[, col])
         df[[col_prefix]] <- grid
         df   <- df[, c(col_prefix, "smooth")]
         path <- file.path(subdir, "smooth_wind.csv")
         write.csv(df, path, row.names = FALSE)
         exported$wind <- df
         message("  Wrote: smooth_wind.csv  (col: '", col, "')")
      }
   }
   
   # -------------------------------------------------------------------------
   # 3. Tide smooth
   # -------------------------------------------------------------------------
   if (!is.null(st$tide_var)) {
      obs   <- gam_obj$model[[st$tide_var]]
      grid  <- seq(min(obs) * (1 - buffer), max(obs) * (1 + buffer),
                   length.out = n_grid)
      nd    <- make_grid(gam_obj, st, st$tide_var, grid)
      terms <- predict(gam_obj, nd, type = "terms")
      col   <- find_smooth_col(st$smooth_cols, st$tide_var)
      
      if (!is.null(col)) {
         df   <- data.frame(tide_range = grid, smooth = terms[, col])
         path <- file.path(subdir, "smooth_tide.csv")
         write.csv(df, path, row.names = FALSE)
         exported$tide <- df
         message("  Wrote: smooth_tide.csv  (col: '", col, "')")
      } else {
         warning("  Could not find tide smooth column in type='terms' output")
      }
   }
   
   # -------------------------------------------------------------------------
   # 4. Validation points (5-row cross-check)
   # -------------------------------------------------------------------------
   set.seed(seed)
   val_idx  <- sample(seq_len(nrow(gam_obj$model)), 5)
   val_data <- gam_obj$model[val_idx, , drop = FALSE]
   
   r_pred   <- predict(gam_obj, val_data, type = "response")
   r_terms  <- predict(gam_obj, val_data, type = "terms")
   
   intercept <- coef(gam_obj)["(Intercept)"]
   lag_coef  <- if (!is.null(st$lag_var)) coef(gam_obj)[st$lag_var] else 0
   lag_vals  <- if (!is.null(st$lag_var)) val_data[[st$lag_var]] else 0
   
   # Reconstruct from terms (sanity check)
   manual_pred <- intercept + lag_coef * lag_vals + rowSums(r_terms)
   
   val_df <- data.frame(r_prediction = r_pred, manual_prediction = manual_pred,
                        diff = r_pred - manual_pred)
   
   # Add predictor values for Python-side validation
   if (!is.null(st$lag_var))      val_df$lag_salinity <- val_data[[st$lag_var]]
   if (!is.null(st$discharge_var)) val_df$discharge   <- val_data[[st$discharge_var]]
   if (!is.null(st$wind_var))      val_df$wind        <- val_data[[st$wind_var]]
   if (!is.null(st$wind_dir_var))  val_df$wind_dir    <- as.character(val_data[[st$wind_dir_var]])
   if (!is.null(st$tide_var))      val_df$tide_range  <- val_data[[st$tide_var]]
   
   path <- file.path(subdir, "validation_points.csv")
   write.csv(val_df, path, row.names = FALSE)
   exported$validation <- val_df
   message("  Wrote: validation_points.csv")
   message("  Max |diff| = ", format(max(abs(val_df$diff)), scientific = TRUE))
   
   # -------------------------------------------------------------------------
   # 5. Metadata: predictor names, intercept, lag coef
   # -------------------------------------------------------------------------
   meta <- data.frame(
      lag_days        = k,
      discharge_var   = st$discharge_var   %||% NA_character_,
      wind_var        = st$wind_var        %||% NA_character_,
      wind_type       = st$wind_type       %||% NA_character_,
      wind_dir_var    = st$wind_dir_var    %||% NA_character_,
      tide_var        = st$tide_var        %||% NA_character_,
      lag_sal_var     = st$lag_var         %||% NA_character_,
      gam_intercept   = intercept,
      gam_lag_coef    = lag_coef,
      n_obs           = nrow(gam_obj$model),
      r_squared       = summary(gam_obj)$r.sq,
      stringsAsFactors = FALSE
   )
   
   path <- file.path(subdir, "smooth_metadata.csv")
   write.csv(meta, path, row.names = FALSE)
   exported$metadata <- meta
   message("  Wrote: smooth_metadata.csv")
   message(sprintf("  intercept=%.6f  lag_coef=%.6f  R²=%.4f",
                   intercept, lag_coef, summary(gam_obj)$r.sq))
   
   message(sprintf("=== Done: %d-day forecast ===\n", k))
   invisible(exported)
}


# =============================================================================
# Null coalescing operator (base R doesn't have one)
# =============================================================================
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Run for all forecasts
output_dir <- "~/Documents/Penn State/Projects/Chapter1/Outputs/ForecastSmooths"


# Export all lead times at once
for (k in seq(1, 30, 1)) {
  export_forecast_smooths(k = k, models = models, output_dir = output_dir)
}
