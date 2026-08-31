# Helper function: Safe rolling mean enforcing minimum data completeness (>= 70% valid data)
safe_rollmean <- function(x, k, min_prop = 0.70) {
   min_obs <- ceiling(k * min_prop)
   
   # Count valid non-NA entries per rolling window
   valid_counts <- zoo::rollapply(
      !is.na(x), width = k, FUN = sum, fill = NA, align = "right"
   )
   
   # Sum non-NA values per rolling window
   roll_sums <- zoo::rollapply(
      ifelse(is.na(x), 0, x), width = k, FUN = sum, fill = NA, align = "right"
   )
   
   # Mask windows that fall below the completeness threshold
   ifelse(valid_counts >= min_obs, roll_sums / valid_counts, NA_real_)
}

build_model_data <- function(daily_raw, flush_threshold, estuary_axis_deg = 0) {
   
   model_data <- daily_raw %>%
      arrange(DateTime) %>%
      
   # =======================================================================
   # PART 0: SALINITY PREDICTORS
   # =======================================================================
   mutate(
      LagSalinity = lag(Salinity, 0)
   ) %>%
      
   # =======================================================================
   # PART 1: TIDE PREDICTORS
   # =======================================================================
   mutate(
      TideRange1  = zoo::rollmean(TideRange, 1,  fill = NA, align = "right", na.rm = TRUE),
      TideRange2  = zoo::rollmean(TideRange, 2,  fill = NA, align = "right", na.rm = TRUE),
      TideRange3  = zoo::rollmean(TideRange, 3,  fill = NA, align = "right", na.rm = TRUE),
      TideRange4  = zoo::rollmean(TideRange, 4,  fill = NA, align = "right", na.rm = TRUE),
      TideRange5  = zoo::rollmean(TideRange, 5,  fill = NA, align = "right", na.rm = TRUE),
      TideRange6  = zoo::rollmean(TideRange, 6,  fill = NA, align = "right", na.rm = TRUE),
      TideRange7  = zoo::rollmean(TideRange, 7,  fill = NA, align = "right", na.rm = TRUE),
      TideRange8  = zoo::rollmean(TideRange, 8,  fill = NA, align = "right", na.rm = TRUE),
      TideRange9  = zoo::rollmean(TideRange, 9,  fill = NA, align = "right", na.rm = TRUE),
      TideRange10 = zoo::rollmean(TideRange, 10, fill = NA, align = "right", na.rm = TRUE),
      TideRange11 = zoo::rollmean(TideRange, 11, fill = NA, align = "right", na.rm = TRUE),
      TideRange12 = zoo::rollmean(TideRange, 12, fill = NA, align = "right", na.rm = TRUE),
      TideRange13 = zoo::rollmean(TideRange, 13, fill = NA, align = "right", na.rm = TRUE),
      TideRange14 = zoo::rollmean(TideRange, 14, fill = NA, align = "right", na.rm = TRUE),
      TideRange21 = zoo::rollmean(TideRange, 21, fill = NA, align = "right", na.rm = TRUE),
      TideRange30 = zoo::rollmean(TideRange, 30, fill = NA, align = "right", na.rm = TRUE),
      TideRange35 = zoo::rollmean(TideRange, 35, fill = NA, align = "right", na.rm = TRUE),
      TideRange40 = zoo::rollmean(TideRange, 40, fill = NA, align = "right", na.rm = TRUE),
      TideRange50 = zoo::rollmean(TideRange, 50, fill = NA, align = "right", na.rm = TRUE),
      TideRange60 = zoo::rollmean(TideRange, 60, fill = NA, align = "right", na.rm = TRUE),
      
      TideMean1   = zoo::rollmean(TideMean, 1,  fill = NA, align = "right", na.rm = TRUE),
      TideMean2   = zoo::rollmean(TideMean, 2,  fill = NA, align = "right", na.rm = TRUE),
      TideMean3   = zoo::rollmean(TideMean, 3,  fill = NA, align = "right", na.rm = TRUE),
      TideMean4   = zoo::rollmean(TideMean, 4,  fill = NA, align = "right", na.rm = TRUE),
      TideMean5   = zoo::rollmean(TideMean, 5,  fill = NA, align = "right", na.rm = TRUE),
      TideMean6   = zoo::rollmean(TideMean, 6,  fill = NA, align = "right", na.rm = TRUE),
      TideMean7   = zoo::rollmean(TideMean, 7,  fill = NA, align = "right", na.rm = TRUE),
      TideMean8   = zoo::rollmean(TideMean, 8,  fill = NA, align = "right", na.rm = TRUE),
      TideMean9   = zoo::rollmean(TideMean, 9,  fill = NA, align = "right", na.rm = TRUE),
      TideMean10  = zoo::rollmean(TideMean, 10, fill = NA, align = "right", na.rm = TRUE),
      TideMean11  = zoo::rollmean(TideMean, 11, fill = NA, align = "right", na.rm = TRUE),
      TideMean12  = zoo::rollmean(TideMean, 12, fill = NA, align = "right", na.rm = TRUE),
      TideMean13  = zoo::rollmean(TideMean, 13, fill = NA, align = "right", na.rm = TRUE),
      TideMean14  = zoo::rollmean(TideMean, 14, fill = NA, align = "right", na.rm = TRUE),
      TideMean21  = zoo::rollmean(TideMean, 21, fill = NA, align = "right", na.rm = TRUE),
      TideMean30  = zoo::rollmean(TideMean, 30, fill = NA, align = "right", na.rm = TRUE),
      TideMean35  = zoo::rollmean(TideMean, 35, fill = NA, align = "right", na.rm = TRUE),
      TideMean40  = zoo::rollmean(TideMean, 40, fill = NA, align = "right", na.rm = TRUE),
      TideMean50  = zoo::rollmean(TideMean, 50, fill = NA, align = "right", na.rm = TRUE),
      TideMean60  = zoo::rollmean(TideMean, 60, fill = NA, align = "right", na.rm = TRUE)
   ) %>%
      
   # =======================================================================
   # PART 2: WIND PREDICTORS
   # =======================================================================
   mutate(
      RollingWindAlong1  = safe_rollmean(WindAlong, 1,  min_prop = 0.70),
      RollingWindAlong2  = safe_rollmean(WindAlong, 2,  min_prop = 0.70),
      RollingWindAlong3  = safe_rollmean(WindAlong, 3,  min_prop = 0.70),
      RollingWindAlong4  = safe_rollmean(WindAlong, 4,  min_prop = 0.70),
      RollingWindAlong5  = safe_rollmean(WindAlong, 5,  min_prop = 0.70),
      RollingWindAlong6  = safe_rollmean(WindAlong, 6,  min_prop = 0.70),
      RollingWindAlong7  = safe_rollmean(WindAlong, 7,  min_prop = 0.70),
      RollingWindAlong8  = safe_rollmean(WindAlong, 8,  min_prop = 0.70),
      RollingWindAlong9  = safe_rollmean(WindAlong, 9,  min_prop = 0.70),
      RollingWindAlong10 = safe_rollmean(WindAlong, 10, min_prop = 0.70),
      RollingWindAlong11 = safe_rollmean(WindAlong, 11, min_prop = 0.70),
      RollingWindAlong12 = safe_rollmean(WindAlong, 12, min_prop = 0.70),
      RollingWindAlong13 = safe_rollmean(WindAlong, 13, min_prop = 0.70),
      RollingWindAlong14 = safe_rollmean(WindAlong, 14, min_prop = 0.70),
      RollingWindAlong21 = safe_rollmean(WindAlong, 21, min_prop = 0.70),
      RollingWindAlong30 = safe_rollmean(WindAlong, 30, min_prop = 0.70),
      
      RollingWindCross1  = safe_rollmean(WindCross, 1,  min_prop = 0.70),
      RollingWindCross2  = safe_rollmean(WindCross, 2,  min_prop = 0.70),
      RollingWindCross3  = safe_rollmean(WindCross, 3,  min_prop = 0.70),
      RollingWindCross4  = safe_rollmean(WindCross, 4,  min_prop = 0.70),
      RollingWindCross5  = safe_rollmean(WindCross, 5,  min_prop = 0.70),
      RollingWindCross6  = safe_rollmean(WindCross, 6,  min_prop = 0.70),
      RollingWindCross7  = safe_rollmean(WindCross, 7,  min_prop = 0.70),
      RollingWindCross8  = safe_rollmean(WindCross, 8,  min_prop = 0.70),
      RollingWindCross9  = safe_rollmean(WindCross, 9,  min_prop = 0.70),
      RollingWindCross10 = safe_rollmean(WindCross, 10, min_prop = 0.70),
      RollingWindCross11 = safe_rollmean(WindCross, 11, min_prop = 0.70),
      RollingWindCross12 = safe_rollmean(WindCross, 12, min_prop = 0.70),
      RollingWindCross13 = safe_rollmean(WindCross, 13, min_prop = 0.70),
      RollingWindCross14 = safe_rollmean(WindCross, 14, min_prop = 0.70),
      RollingWindCross21 = safe_rollmean(WindCross, 21, min_prop = 0.70),
      RollingWindCross30 = safe_rollmean(WindCross, 30, min_prop = 0.70)
   ) %>%
      
   # =======================================================================
   # PART 3: DISCHARGE PREDICTORS
   # =======================================================================
   mutate(
      RollingDischarge1  = zoo::rollmean(Discharge, 1,  fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge2  = zoo::rollmean(Discharge, 2,  fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge3  = zoo::rollmean(Discharge, 3,  fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge4  = zoo::rollmean(Discharge, 4,  fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge6  = zoo::rollmean(Discharge, 6,  fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge7  = zoo::rollmean(Discharge, 7,  fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge10 = zoo::rollmean(Discharge, 10, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge12 = zoo::rollmean(Discharge, 12, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge14 = zoo::rollmean(Discharge, 14, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge21 = zoo::rollmean(Discharge, 21, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge30 = zoo::rollmean(Discharge, 30, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge35 = zoo::rollmean(Discharge, 35, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge40 = zoo::rollmean(Discharge, 40, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge50 = zoo::rollmean(Discharge, 50, fill = NA, align = "right", na.rm = TRUE),
      RollingDischarge60 = zoo::rollmean(Discharge, 60, fill = NA, align = "right", na.rm = TRUE),
      
      MaxDischarge1      = zoo::rollmax(MaxDischarge, 1,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge2      = zoo::rollmax(MaxDischarge, 2,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge3      = zoo::rollmax(MaxDischarge, 3,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge4      = zoo::rollmax(MaxDischarge, 4,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge5      = zoo::rollmax(MaxDischarge, 5,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge6      = zoo::rollmax(MaxDischarge, 6,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge7      = zoo::rollmax(MaxDischarge, 7,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge8      = zoo::rollmax(MaxDischarge, 8,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge9      = zoo::rollmax(MaxDischarge, 9,  fill = NA, align = "right", na.rm = TRUE),
      MaxDischarge10     = zoo::rollmax(MaxDischarge, 10, fill = NA, align = "right", na.rm = TRUE),
      
      ExceedFlux1        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 1,  fill = NA, align = "right"),
      ExceedFlux2        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 2,  fill = NA, align = "right"),
      ExceedFlux3        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 3,  fill = NA, align = "right"),
      ExceedFlux4        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 4,  fill = NA, align = "right"),
      ExceedFlux5        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 5,  fill = NA, align = "right"),
      ExceedFlux6        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 6,  fill = NA, align = "right"),
      ExceedFlux7        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 7,  fill = NA, align = "right"),
      ExceedFlux8        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 8,  fill = NA, align = "right"),
      ExceedFlux9        = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 9,  fill = NA, align = "right"),
      ExceedFlux10       = zoo::rollsum(pmax(0, MaxDischarge - flush_threshold), 10, fill = NA, align = "right")
   )
   
   # Set NaN and Inf values to NA
   model_data[] <- lapply(model_data, function(x) { x[is.nan(x) | is.infinite(x)] <- NA; x })
   
   # Clean up base variables that are fully superseded by rolling versions
   model_data <- model_data %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      relocate(Salinity, .after = DayOfYear) %>%
      relocate(FERC, .after = DayOfYear) %>%
      dplyr::select(-c(TideRange, TideMean, MaxDischarge, WindAlong, WindCross, Discharge))
   
   return(model_data)
}