# =============================================================================
# Script Name:    ExportForecastSmooths.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Exports GAM smooth lookup tables for a specified forecast
#                 horizon (lag k). Handles rolling discharge, ExceedFlux7,
#                 wind (by=WindDir), and tide terms.
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# Helper: additive buffer on actual range (works for negative values)
# =============================================================================

buffered_seq <- function(x, n, buffer = 0.05, min_val = NULL, max_val = NULL) {
   rng  <- range(x, na.rm = TRUE)
   span <- rng[2] - rng[1]
   pad  <- span * buffer
   lo   <- rng[1] - pad
   hi   <- rng[2] + pad
   if (!is.null(min_val)) lo <- max(lo, min_val)
   if (!is.null(max_val)) hi <- min(hi, max_val)
   seq(lo, hi, length.out = n)
}

# =============================================================================
# Helper: parse GAM structure — detects all relevant terms
# =============================================================================

parse_gam_structure <- function(gam_obj) {
   
   term_names <- names(gam_obj$model)
   
   # Rolling discharge (RollingDischarge*)
   rolling_discharge_var <- term_names[grepl("^RollingDischarge|^LagDischarge",
                                             term_names, ignore.case = TRUE)]
   rolling_discharge_var <- if (length(rolling_discharge_var) > 0) 
      rolling_discharge_var[1] else NULL
   
   # Flushing discharge (ExceedFlux*)
   exceed_flux_var <- term_names[grepl("ExceedFlux", term_names, ignore.case = TRUE)]
   exceed_flux_var <- if (length(exceed_flux_var) > 0) exceed_flux_var[1] else NULL
   
   # DaysSinceFlush
   days_since_var <- term_names[grepl("DaysSinceFlush", term_names, ignore.case = TRUE)]
   days_since_var <- if (length(days_since_var) > 0) days_since_var[1] else NULL
   
   # Wind U (east-west component)
   wind_u_var <- term_names[grepl("^RollingU|^LagU", term_names)]
   wind_u_var <- if (length(wind_u_var) > 0) wind_u_var[1] else NULL
   
   # Wind V (north-south component) — fallback if no U
   wind_v_var <- term_names[grepl("^RollingV|^LagV", term_names)]
   wind_v_var <- if (length(wind_v_var) > 0) wind_v_var[1] else NULL
   
   # Resolve which wind is active
   if (!is.null(wind_u_var)) {
      wind_var  <- wind_u_var
      wind_type <- "U"
   } else if (!is.null(wind_v_var)) {
      wind_var  <- wind_v_var
      wind_type <- "V"
   } else {
      wind_var  <- NULL
      wind_type <- NULL
   }
   
   # WindDir factor
   wind_dir_var <- term_names[grepl("WindDir", term_names)]
   wind_dir_var <- if (length(wind_dir_var) > 0) wind_dir_var[1] else NULL
   
   # Tide
   tide_var <- term_names[grepl("Tide", term_names, ignore.case = TRUE)]
   tide_var <- if (length(tide_var) > 0) tide_var[1] else NULL
   
   # InFlushRegime factor (if present)
   flush_regime_var <- term_names[grepl("InFlushRegime", term_names)]
   flush_regime_var <- if (length(flush_regime_var) > 0) flush_regime_var[1] else NULL
   
   # Lag salinity (parametric)
   lag_var <- term_names[grepl("LagSalinity|Lag_Salinity|lagsal", 
                               term_names, ignore.case = TRUE)]
   lag_var <- lag_var[lag_var != "Salinity"]
   lag_var <- if (length(lag_var) > 0) lag_var[1] else NULL
   
   # Smooth column names from type="terms"
   nd_dummy  <- gam_obj$model[1, , drop = FALSE]
   terms_mat <- predict(gam_obj, nd_dummy, type = "terms")
   smooth_cols <- colnames(terms_mat)
   
   list(
      rolling_discharge_var = rolling_discharge_var,
      exceed_flux_var       = exceed_flux_var,
      days_since_var        = days_since_var,
      wind_var              = wind_var,
      wind_type             = wind_type,
      wind_dir_var          = wind_dir_var,
      flush_regime_var      = flush_regime_var,
      tide_var              = tide_var,
      lag_var               = lag_var,
      smooth_cols           = smooth_cols
   )
}

# =============================================================================
# Helper: find smooth column matching a pattern
# =============================================================================

find_smooth_col <- function(smooth_cols, pattern) {
   matches <- smooth_cols[grepl(pattern, smooth_cols, fixed = FALSE)]
   if (length(matches) == 0) NULL else matches[1]
}

# =============================================================================
# Helper: build prediction grid holding all vars at mean except one
# =============================================================================

make_grid <- function(gam_obj, vary_var, vary_values,
                      wind_dir_level = NULL, flush_regime_level = NULL) {
   
   nd <- as.data.frame(lapply(gam_obj$model, function(col) {
      if (is.numeric(col))      mean(col, na.rm = TRUE)
      else if (is.factor(col))  factor(levels(col)[1], levels = levels(col))
      else                      col[1]
   }))
   nd <- nd[rep(1, length(vary_values)), , drop = FALSE]
   rownames(nd) <- NULL
   
   nd[[vary_var]] <- vary_values
   
   if (!is.null(wind_dir_level)) {
      wind_dir_col <- names(nd)[grepl("WindDir", names(nd))]
      if (length(wind_dir_col) > 0) {
         nd[[wind_dir_col]] <- factor(wind_dir_level,
                                      levels = levels(gam_obj$model[[wind_dir_col]]))
      }
   }
   
   if (!is.null(flush_regime_level)) {
      regime_col <- names(nd)[grepl("InFlushRegime", names(nd))]
      if (length(regime_col) > 0) {
         nd[[regime_col]] <- factor(flush_regime_level,
                                    levels = levels(gam_obj$model[[regime_col]]))
      }
   }
   
   nd
}

# =============================================================================
# Main export function
# =============================================================================

export_forecast_smooths <- function(k,
                                    models,
                                    output_dir,
                                    n_grid = 500,
                                    buffer = 0.05,
                                    seed   = 42) {
   
   lag_name <- paste0("Lag", k)
   if (!lag_name %in% names(models)) {
      stop(sprintf("models$%s not found. Available: %s",
                   lag_name, paste(names(models), collapse = ", ")))
   }
   gam_obj <- models[[lag_name]]$gam_object
   if (is.null(gam_obj)) stop(sprintf("models$%s$gam_object is NULL", lag_name))
   
   subdir <- file.path(output_dir, paste0(k, "DayForecast"))
   dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
   message(sprintf("\n=== Exporting smooths: %d-day forecast → %s ===", k, subdir))
   
   st <- parse_gam_structure(gam_obj)
   
   message("  Detected terms:")
   message("    rolling discharge : ", st$rolling_discharge_var %||% "none")
   message("    exceed flux       : ", st$exceed_flux_var       %||% "none")
   message("    days since flush  : ", st$days_since_var        %||% "none")
   message("    wind              : ", st$wind_var  %||% "none",
           " (", st$wind_type %||% "none", ")")
   message("    wind dir          : ", st$wind_dir_var          %||% "none")
   message("    flush regime      : ", st$flush_regime_var      %||% "none")
   message("    tide              : ", st$tide_var              %||% "none")
   message("    lag salinity      : ", st$lag_var               %||% "none")
   message("  Smooth columns: ", paste(st$smooth_cols, collapse = ", "))
   
   exported <- list()
   
   # -------------------------------------------------------------------------
   # 1. Rolling discharge smooth
   # -------------------------------------------------------------------------
   if (!is.null(st$rolling_discharge_var)) {
      obs  <- gam_obj$model[[st$rolling_discharge_var]]
      grid <- buffered_seq(obs, n_grid, buffer, min_val = 0)
      nd   <- make_grid(gam_obj, st$rolling_discharge_var, grid)
      terms <- predict(gam_obj, nd, type = "terms")
      col <- find_smooth_col(st$smooth_cols, st$rolling_discharge_var)
      
      if (!is.null(col)) {
         df   <- data.frame(discharge = grid, smooth = terms[, col])
         path <- file.path(subdir, "smooth_rolling_discharge.csv")
         write.csv(df, path, row.names = FALSE)
         exported$rolling_discharge <- df
         message("  Wrote: smooth_rolling_discharge.csv  (col: '", col, "')")
      } else {
         warning("  Could not find rolling discharge smooth column")
      }
   }
   
   # -------------------------------------------------------------------------
   # 2. ExceedFlux smooth
   #    If by=InFlushRegime, export one CSV per regime level.
   #    If plain smooth, export single CSV.
   # -------------------------------------------------------------------------
   if (!is.null(st$exceed_flux_var)) {
      obs  <- gam_obj$model[[st$exceed_flux_var]]
      grid <- buffered_seq(obs, n_grid, buffer, min_val = 0, max_val = 40000)
      # Clip lower bound to 0 — ExceedFlux is non-negative by construction
      grid <- pmax(grid, 0)
      
      has_regime <- !is.null(st$flush_regime_var) &&
         st$flush_regime_var %in% names(gam_obj$model)
      
      if (has_regime) {
         regime_levels <- levels(gam_obj$model[[st$flush_regime_var]])
         
         for (lvl in regime_levels) {
            nd    <- make_grid(gam_obj, st$exceed_flux_var, grid,
                               flush_regime_level = lvl)
            terms <- predict(gam_obj, nd, type = "terms")
            
            # Column pattern: s(ExceedFlux7_k):InFlushRegimeFlushing etc.
            pattern <- paste0(st$exceed_flux_var, ".*", lvl)
            col     <- find_smooth_col(st$smooth_cols, pattern)
            
            # Fallback: plain ExceedFlux column if by= not found
            if (is.null(col)) {
               col <- find_smooth_col(st$smooth_cols, st$exceed_flux_var)
            }
            
            if (!is.null(col)) {
               df    <- data.frame(exceed_flux = grid, smooth = terms[, col])
               fname <- paste0("smooth_exceed_flux_", tolower(lvl), ".csv")
               path  <- file.path(subdir, fname)
               write.csv(df, path, row.names = FALSE)
               exported[[paste0("exceed_flux_", tolower(lvl))]] <- df
               message("  Wrote: ", fname, "  (col: '", col, "')")
            } else {
               warning(sprintf("  Could not find ExceedFlux smooth column for regime '%s'", lvl))
            }
         }
         
      } else {
         # Plain smooth — no regime interaction
         nd    <- make_grid(gam_obj, st$exceed_flux_var, grid)
         terms <- predict(gam_obj, nd, type = "terms")
         col   <- find_smooth_col(st$smooth_cols, st$exceed_flux_var)
         
         if (!is.null(col)) {
            df   <- data.frame(exceed_flux = grid, smooth = terms[, col])
            path <- file.path(subdir, "smooth_exceed_flux.csv")
            write.csv(df, path, row.names = FALSE)
            exported$exceed_flux <- df
            message("  Wrote: smooth_exceed_flux.csv  (col: '", col, "')")
         } else {
            warning("  Could not find ExceedFlux smooth column")
         }
      }
   }
   
   # -------------------------------------------------------------------------
   # 3. DaysSinceFlush smooth
   # -------------------------------------------------------------------------
   if (!is.null(st$days_since_var)) {
      obs  <- gam_obj$model[[st$days_since_var]]
      grid <- buffered_seq(obs, n_grid, buffer, min_val = 0)
      grid <- pmax(grid, 0)
      nd   <- make_grid(gam_obj, st$days_since_var, grid)
      terms <- predict(gam_obj, nd, type = "terms")
      col   <- find_smooth_col(st$smooth_cols, st$days_since_var)
      
      if (!is.null(col)) {
         df   <- data.frame(days_since_flush = grid, smooth = terms[, col])
         path <- file.path(subdir, "smooth_days_since_flush.csv")
         write.csv(df, path, row.names = FALSE)
         exported$days_since_flush <- df
         message("  Wrote: smooth_days_since_flush.csv  (col: '", col, "')")
      } else {
         warning("  Could not find DaysSinceFlush smooth column")
      }
   }
   
   # -------------------------------------------------------------------------
   # 4. Wind smooths (by=WindDir: one CSV per direction level)
   # -------------------------------------------------------------------------
   if (!is.null(st$wind_var)) {
      obs       <- gam_obj$model[[st$wind_var]]
      wind_grid <- buffered_seq(obs, n_grid, buffer)
      col_name  <- if (st$wind_type == "U") "wind_u" else "wind_v"
      
      if (!is.null(st$wind_dir_var)) {
         dir_levels <- levels(gam_obj$model[[st$wind_dir_var]])
         
         for (dir in dir_levels) {
            # Clip grid to observed range for this direction only
            obs_this_dir <- obs[gam_obj$model[[st$wind_dir_var]] == dir]
            grid_dir     <- buffered_seq(obs_this_dir, n_grid, buffer)
            
            nd    <- make_grid(gam_obj, st$wind_var, grid_dir,
                               wind_dir_level = dir)
            terms <- predict(gam_obj, nd, type = "terms")
            
            pattern <- paste0(st$wind_var, ".*", dir)
            col     <- find_smooth_col(st$smooth_cols, pattern)
            
            if (!is.null(col)) {
               df        <- data.frame(smooth = terms[, col])
               df[[col_name]] <- grid_dir
               df        <- df[, c(col_name, "smooth")]
               fname     <- paste0("smooth_wind_", tolower(dir), ".csv")
               path      <- file.path(subdir, fname)
               write.csv(df, path, row.names = FALSE)
               exported[[paste0("wind_", tolower(dir))]] <- df
               message("  Wrote: ", fname, "  (col: '", col, "')")
            } else {
               warning(sprintf("  Could not find wind smooth column for direction '%s'", dir))
            }
         }
         
      } else {
         # Single wind smooth — no WindDir
         nd    <- make_grid(gam_obj, st$wind_var, wind_grid)
         terms <- predict(gam_obj, nd, type = "terms")
         col   <- find_smooth_col(st$smooth_cols, st$wind_var)
         
         if (!is.null(col)) {
            df           <- data.frame(smooth = terms[, col])
            df[[col_name]] <- wind_grid
            df           <- df[, c(col_name, "smooth")]
            path         <- file.path(subdir, "smooth_wind.csv")
            write.csv(df, path, row.names = FALSE)
            exported$wind <- df
            message("  Wrote: smooth_wind.csv  (col: '", col, "')")
         }
      }
   }
   
   # -------------------------------------------------------------------------
   # 5. Tide smooth
   # -------------------------------------------------------------------------
   if (!is.null(st$tide_var)) {
      obs  <- gam_obj$model[[st$tide_var]]
      grid <- buffered_seq(obs, n_grid, buffer)
      nd   <- make_grid(gam_obj, st$tide_var, grid)
      terms <- predict(gam_obj, nd, type = "terms")
      col   <- find_smooth_col(st$smooth_cols, st$tide_var)
      
      if (!is.null(col)) {
         df   <- data.frame(tide_range = grid, smooth = terms[, col])
         path <- file.path(subdir, "smooth_tide.csv")
         write.csv(df, path, row.names = FALSE)
         exported$tide <- df
         message("  Wrote: smooth_tide.csv  (col: '", col, "')")
      } else {
         warning("  Could not find tide smooth column")
      }
   }
   
   # -------------------------------------------------------------------------
   # 6. Validation points
   # -------------------------------------------------------------------------
   set.seed(seed)
   val_idx  <- sample(seq_len(nrow(gam_obj$model)), 5)
   val_data <- gam_obj$model[val_idx, , drop = FALSE]
   
   r_pred  <- predict(gam_obj, val_data, type = "response")
   r_terms <- predict(gam_obj, val_data, type = "terms")
   
   intercept <- coef(gam_obj)["(Intercept)"]
   lag_coef  <- if (!is.null(st$lag_var)) coef(gam_obj)[st$lag_var] else 0
   lag_vals  <- if (!is.null(st$lag_var)) val_data[[st$lag_var]] else 0
   
   # Reconstruct: intercept + lag term + all smooth terms
   # type="terms" does NOT include the parametric intercept or lag,
   # so we add them back manually
   manual_pred <- intercept + lag_coef * lag_vals + rowSums(r_terms)
   
   val_df <- data.frame(
      r_prediction     = r_pred,
      manual_prediction = manual_pred,
      diff             = r_pred - manual_pred
   )
   
   if (!is.null(st$lag_var))               val_df$lag_salinity       <- val_data[[st$lag_var]]
   if (!is.null(st$rolling_discharge_var)) val_df$rolling_discharge  <- val_data[[st$rolling_discharge_var]]
   if (!is.null(st$exceed_flux_var))       val_df$exceed_flux        <- val_data[[st$exceed_flux_var]]
   if (!is.null(st$days_since_var))        val_df$days_since_flush   <- val_data[[st$days_since_var]]
   if (!is.null(st$wind_var))             val_df$wind               <- val_data[[st$wind_var]]
   if (!is.null(st$wind_dir_var))         val_df$wind_dir           <- as.character(val_data[[st$wind_dir_var]])
   if (!is.null(st$tide_var))             val_df$tide_range         <- val_data[[st$tide_var]]
   
   path <- file.path(subdir, "validation_points.csv")
   write.csv(val_df, path, row.names = FALSE)
   exported$validation <- val_df
   message("  Wrote: validation_points.csv  (max |diff| = ",
           format(max(abs(val_df$diff)), scientific = TRUE, digits = 3), ")")
   
   if (max(abs(val_df$diff)) > 1e-6) {
      warning("  Validation diff > 1e-6 — check parametric term reconstruction")
   }
   
   # -------------------------------------------------------------------------
   # 7. Metadata
   # -------------------------------------------------------------------------
   meta <- data.frame(
      lag_days              = k,
      rolling_discharge_var = st$rolling_discharge_var %||% NA_character_,
      exceed_flux_var       = st$exceed_flux_var       %||% NA_character_,
      days_since_var        = st$days_since_var        %||% NA_character_,
      wind_var              = st$wind_var              %||% NA_character_,
      wind_type             = st$wind_type             %||% NA_character_,
      wind_dir_var          = st$wind_dir_var          %||% NA_character_,
      flush_regime_var      = st$flush_regime_var      %||% NA_character_,
      tide_var              = st$tide_var              %||% NA_character_,
      lag_sal_var           = st$lag_var               %||% NA_character_,
      gam_intercept         = intercept,
      gam_lag_coef          = lag_coef,
      n_obs                 = nrow(gam_obj$model),
      r_squared             = summary(gam_obj)$r.sq,
      stringsAsFactors      = FALSE
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
# Run for all lead times
# =============================================================================
output_dir <- "Outputs/ForecastSmooths"

for (k in seq(1, 30, 1)) {
   export_forecast_smooths(k = k, models = models, output_dir = output_dir)
}
