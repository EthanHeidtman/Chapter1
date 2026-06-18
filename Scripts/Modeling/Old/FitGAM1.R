# =============================================================================
# Function:       fit_gam
# Project:        Chapter1
# Description:    Fits the unified multi-horizon salinity GAM on stacked
#                 date-horizon data. h is treated as an explicit smooth term,
#                 with ti(h, predictor) interactions allowing each predictor's
#                 contribution to vary across forecast lead times.
#                 LagSalinity enters as a linear main effect (per prior
#                 workflow) but still receives a ti(h, LagSalinity) interaction
#                 to capture horizon-dependent decay of its coefficient.
#                 Wind enters with by = WindDir (RightBank/LeftBank or
#                 UpEstuary/DownEstuary depending on Along vs Cross), but the
#                 ti(h, wind) interaction is NOT split by WindDir.
#                 K is tuned via expanding-window CV across grouped k-ranges:
#                 k_h (horizon smooth + h-marginal in all ti terms),
#                 k_interaction (predictor-marginal in all ti terms),
#                 k_sustained_flow, k_flushing_flow, k_physical (tide),
#                 k_wind. LagSalinity has no main-effect k (linear).
# =============================================================================

fit_gam <- function(data,
                    response = 'Salinity_h',
                    predictors = NULL,           # must include 'h'
                    folds = NULL,
                    high_salinity_threshold = 0.15,
                    
                    # Distribution family
                    family_type = "gaussian",   # "gaussian", "Gamma", "Tweedie"
                    link = NULL,                 # NULL = auto-select
                    tweedie_p = 1.5,
                    
                    # Smoothing parameters by term group
                    k_h_range             = c(4, 12),
                    k_interaction_range   = c(4, 12),
                    k_sustained_flow_range = c(4, 12),
                    k_flushing_flow_range  = c(4, 12),
                    k_physical_range       = c(4, 10),
                    k_wind_range           = c(4, 10),
                    
                    # Additional custom interactions, beyond the automatic
                    # ti(h, predictor) terms. List of list(vars = c(...))
                    interactions = list(),
                    
                    # Basis types
                    basis_default = 'tp',
                    basis_horizon = 'cr',
                    
                    # BAM parameters
                    method = 'fREML',
                    discrete = TRUE,
                    nthreads = 4,
                    gam_select = TRUE,
                    
                    # Tuning control
                    gam_levels = 3) {
   
   library(mgcv)
   library(dplyr)
   library(purrr)
   
   if (!('h' %in% predictors)) {
      stop("predictors must include 'h' for the unified multi-horizon GAM.")
   }
   
   # ============================================================================
   # SET UP FAMILY OBJECT
   # ============================================================================
   
   if (is.null(link)) {
      link <- switch(family_type,
                     "gaussian" = "identity",
                     "Gamma"    = "log",
                     "Tweedie"  = "log")
      cat("Auto-selected link:", link, "for", family_type, "family\n\n")
   }
   
   if (family_type == "Gamma" && link == "identity") {
      warning("Identity link with Gamma can produce negative predictions. ",
              "Consider link = 'log'.\n")
   }
   
   gam_family <- switch(family_type,
                        "gaussian" = gaussian(link = link),
                        "Gamma"    = Gamma(link = link),
                        "Tweedie"  = Tweedie(p = tweedie_p, link = link),
                        stop("Unknown family_type"))
   
   # ============================================================================
   # CLASSIFY PREDICTORS
   # ============================================================================
   
   h_var <- 'h'
   non_h_predictors <- setdiff(predictors, h_var)
   
   lag_vars       <- non_h_predictors[grepl("LagSalinity", non_h_predictors, ignore.case = TRUE)]
   sustained_vars <- non_h_predictors[grepl("RollingDischarge|RollingAnomaly", non_h_predictors, ignore.case = TRUE)]
   flushing_vars  <- non_h_predictors[grepl("MaxDischarge|ExceedFlux", non_h_predictors, ignore.case = TRUE)]
   tide_vars      <- non_h_predictors[grepl("TideRange|TideMean", non_h_predictors, ignore.case = TRUE)]
   wind_vars      <- non_h_predictors[grepl("RollingWindAlong|RollingWindCross", non_h_predictors, ignore.case = TRUE)]
   
   classified <- c(lag_vars, sustained_vars, flushing_vars, tide_vars, wind_vars)
   other_vars <- setdiff(non_h_predictors, classified)
   
   has_lag        <- length(lag_vars)       > 0
   has_sustained  <- length(sustained_vars) > 0
   has_flushing   <- length(flushing_vars)  > 0
   has_tide       <- length(tide_vars)      > 0
   has_wind       <- length(wind_vars)      > 0
   has_other      <- length(other_vars)     > 0
   has_interactions <- length(interactions) > 0
   
   # All non-h predictors get a ti(h, predictor) interaction
   ti_vars <- non_h_predictors
   
   # ============================================================================
   # PREPARE DATA
   # ============================================================================
   
   data_subset <- data %>%
      mutate(.row_id = row_number()) %>%
      select(.row_id, DateTime, all_of(response), all_of(predictors))
   
   # Construct WindDir from the wind predictor's sign, following Script 01
   # convention: RollingWindAlong >= 0 -> UpEstuary / DownEstuary
   #             RollingWindCross >= 0 -> RightBank / LeftBank
   if (has_wind) {
      wind_var <- wind_vars[1]
      if (grepl("Along", wind_var)) {
         data_subset <- data_subset %>%
            mutate(WindDir = factor(
               ifelse(.data[[wind_var]] >= 0, "UpEstuary", "DownEstuary"),
               levels = c("DownEstuary", "UpEstuary")
            ))
      } else {
         data_subset <- data_subset %>%
            mutate(WindDir = factor(
               ifelse(.data[[wind_var]] >= 0, "RightBank", "LeftBank"),
               levels = c("LeftBank", "RightBank")
            ))
      }
   }
   
   complete_rows <- complete.cases(data_subset %>% select(-.row_id))
   
   data_clean <- data_subset %>%
      filter(complete_rows) %>%
      mutate(
         Response          = .data[[response]],
         Response_original = .data[[response]]
      )
   
   cat("=== DATA PREPARATION ===\n")
   cat("Original rows:", format(nrow(data), big.mark = ","), "\n")
   cat("After removing NAs:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response range: [", round(min(data_clean$Response), 4), ", ",
       round(max(data_clean$Response), 4), "]\n\n")
   
   # ============================================================================
   # CHECK DATA REQUIREMENTS for non-Gaussian families
   # ============================================================================
   
   if (family_type == "Gamma" && any(data_clean$Response <= 0)) {
      n_bad <- sum(data_clean$Response <= 0)
      cat("WARNING: Gamma requires positive values. Found", n_bad,
          "values <= 0. Adding 0.001.\n\n")
      data_clean$Response          <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   if (family_type == "Tweedie" && any(data_clean$Response < 0)) {
      n_bad <- sum(data_clean$Response < 0)
      cat("WARNING: Tweedie requires non-negative values. Found", n_bad,
          "negative values. Setting to 0.001.\n\n")
      data_clean$Response          <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   # ============================================================================
   # CREATE K TUNING GRID
   # ============================================================================
   
   k_sequences <- list()
   
   k_sequences$k_h <- unique(round(seq(
      k_h_range[1], k_h_range[2], length.out = gam_levels)))
   
   k_sequences$k_interaction <- unique(round(seq(
      k_interaction_range[1], k_interaction_range[2], length.out = gam_levels)))
   
   if (has_sustained) {
      k_sequences$k_sustained_flow <- unique(round(seq(
         k_sustained_flow_range[1], k_sustained_flow_range[2], length.out = gam_levels)))
   }
   if (has_flushing) {
      k_sequences$k_flushing_flow <- unique(round(seq(
         k_flushing_flow_range[1], k_flushing_flow_range[2], length.out = gam_levels)))
   }
   if (has_tide || has_other) {
      k_sequences$k_physical <- unique(round(seq(
         k_physical_range[1], k_physical_range[2], length.out = gam_levels)))
   }
   if (has_wind) {
      k_sequences$k_wind <- unique(round(seq(
         k_wind_range[1], k_wind_range[2], length.out = gam_levels)))
   }
   
   k_grid <- expand.grid(k_sequences) %>% distinct()
   
   if (!has_sustained)            k_grid$k_sustained_flow <- k_sustained_flow_range[1]
   if (!has_flushing)             k_grid$k_flushing_flow  <- k_flushing_flow_range[1]
   if (!has_tide && !has_other)   k_grid$k_physical       <- k_physical_range[1]
   if (!has_wind)                 k_grid$k_wind           <- k_wind_range[1]
   
   k_grid <- k_grid %>%
      select(k_h, k_interaction, k_sustained_flow, k_flushing_flow, k_physical, k_wind)
   
   active_k_types <- names(k_sequences)
   
   # ============================================================================
   # MODEL SETUP SUMMARY
   # ============================================================================
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response:", response, "\n")
   cat("Predictors:", length(predictors), "\n")
   cat("Family:", family_type, "with", link, "link\n")
   if (family_type == "Tweedie") cat("Tweedie power:", tweedie_p, "\n")
   cat("\n")
   
   cat("Term groups:\n")
   cat("  Horizon (h):       s(h)", if (length(ti_vars) > 0) paste0(" + ti(h, ", ti_vars, ")", collapse = "") else "", "\n")
   if (has_lag)       cat("  LagSalinity:       ", paste(lag_vars,       collapse = ", "), "(linear)\n")
   if (has_sustained) cat("  Sustained flow:    ", paste(sustained_vars, collapse = ", "), "\n")
   if (has_flushing)  cat("  Flushing flow:     ", paste(flushing_vars,  collapse = ", "), "\n")
   if (has_tide)      cat("  Tide:              ", paste(tide_vars,      collapse = ", "), "\n")
   if (has_wind)      cat("  Wind:              ", paste(wind_vars,      collapse = ", "), "(by = WindDir)\n")
   if (has_other)     cat("  Other:             ", paste(other_vars,     collapse = ", "), "\n")
   if (has_interactions) cat("  Custom interactions:", length(interactions), "\n")
   
   cat("\nTuning", nrow(k_grid), "k combinations across:", paste(active_k_types, collapse = ", "), "\n")
   print(k_grid %>% select(all_of(active_k_types)))
   cat("\n")
   
   # ============================================================================
   # BUILD FORMULA FUNCTION
   # ============================================================================
   
   build_gam_formula <- function(k_h, k_interaction, k_sustained_flow,
                                 k_flushing_flow, k_physical, k_wind) {
      
      terms <- c()
      
      # Horizon main effect
      terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", basis_horizon, "')"))
      
      # LagSalinity: linear main effect
      if (has_lag) {
         terms <- c(terms, lag_vars)
      }
      
      # Sustained discharge
      if (has_sustained) {
         terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow,
                                  ", bs='", basis_default, "')"))
      }
      
      # Flushing discharge
      if (has_flushing) {
         terms <- c(terms, paste0("s(", flushing_vars, ", k=", k_flushing_flow,
                                  ", bs='", basis_default, "')"))
      }
      
      # Tide
      if (has_tide) {
         terms <- c(terms, paste0("s(", tide_vars, ", k=", k_physical,
                                  ", bs='", basis_default, "')"))
      }
      
      # Wind, with by = WindDir
      if (has_wind) {
         terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,
                                  ", bs='", basis_default, "')"))
      }
      
      # Other (catch-all)
      if (has_other) {
         terms <- c(terms, paste0("s(", other_vars, ", k=", k_physical,
                                  ", bs='", basis_default, "')"))
      }
      
      # ti(h, predictor) for every non-h predictor
      for (var in ti_vars) {
         terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                                  "), bs=c('", basis_horizon, "', '", basis_default, "'))"))
      }
      
      # Custom additional interactions
      if (has_interactions) {
         for (int in interactions) {
            if (all(int$vars %in% predictors)) {
               terms <- c(terms, paste0("ti(", paste(int$vars, collapse = ", "),
                                        ", k=", k_interaction, ")"))
            }
         }
      }
      
      as.formula(paste("Response ~", paste(terms, collapse = " + ")))
   }
   
   # ============================================================================
   # HELPER FUNCTION: Fit and evaluate on fold
   # Folds reference row indices in the ORIGINAL data; data_clean retains
   # .row_id to map back correctly after NA-filtering.
   # ============================================================================
   
   model_cols <- setdiff(names(data_clean), c(".row_id", "DateTime", response, "Response_original"))
   
   fit_fold <- function(formula, train_idx, test_idx, fold_num) {
      
      train_data <- data_clean %>%
         filter(.row_id %in% train_idx) %>%
         select(all_of(model_cols))
      
      test_data <- data_clean %>%
         filter(.row_id %in% test_idx)
      
      bam_args <- list(
         formula  = formula,
         data     = train_data,
         family   = gam_family,
         method   = method,
         discrete = discrete,
         nthreads = nthreads,
         select   = gam_select
      )
      
      gam_fit <- tryCatch({
         suppressWarnings(do.call(bam, bam_args))
      }, error = function(e) {
         cat("    [Fold", fold_num, "error:", e$message, "]\n")
         return(NULL)
      })
      
      fail_result <- list(
         rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
         high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
         n_high_sal = 0L, failed = TRUE
      )
      
      if (is.null(gam_fit)) return(fail_result)
      
      preds <- tryCatch({
         predict(gam_fit, newdata = test_data %>% select(all_of(model_cols)),
                 type = "response")
      }, error = function(e) {
         cat("    [Fold", fold_num, "prediction error:", e$message, "]\n")
         return(NULL)
      })
      
      if (is.null(preds) || any(!is.finite(preds))) return(fail_result)
      
      preds_original <- preds  # identity link (gaussian) or log-link already on response scale
      
      if (any(!is.finite(preds_original))) return(fail_result)
      
      overall_rmse <- sqrt(mean((test_data$Response_original - preds_original)^2))
      overall_rsq  <- cor(test_data$Response_original, preds_original)^2
      overall_mae  <- mean(abs(test_data$Response_original - preds_original))
      
      high_idx <- test_data$Response_original > high_salinity_threshold
      n_high   <- sum(high_idx)
      
      if (n_high > 1) {
         high_rmse <- sqrt(mean((test_data$Response_original[high_idx] - preds_original[high_idx])^2))
         high_rsq  <- cor(test_data$Response_original[high_idx], preds_original[high_idx])^2
         high_mae  <- mean(abs(test_data$Response_original[high_idx] - preds_original[high_idx]))
      } else {
         high_rmse <- NA_real_
         high_rsq  <- NA_real_
         high_mae  <- NA_real_
      }
      
      list(
         rmse = overall_rmse, rsq = overall_rsq, mae = overall_mae,
         high_rmse = high_rmse, high_rsq = high_rsq, high_mae = high_mae,
         n_high_sal = n_high, failed = FALSE
      )
   }
   
   # ============================================================================
   # CROSS-VALIDATION
   # ============================================================================
   
   cat("Running CV...\n")
   
   tune_results <- map_dfr(1:nrow(k_grid), function(i) {
      
      k_vals <- k_grid[i, ]
      formula <- build_gam_formula(k_vals$k_h, k_vals$k_interaction,
                                   k_vals$k_sustained_flow, k_vals$k_flushing_flow,
                                   k_vals$k_physical, k_vals$k_wind)
      
      active_k_str <- paste(
         sapply(active_k_types, function(k) paste0(k, "=", k_vals[[k]])),
         collapse = ", "
      )
      cat("  ", active_k_str)
      
      fold_results <- map_dfr(seq_along(folds), function(j) {
         res <- fit_fold(formula, folds[[j]]$train, folds[[j]]$test, j)
         tibble(fold = j, !!!res)
      })
      
      summary_row <- fold_results %>%
         summarize(
            mean_rmse      = mean(rmse, na.rm = TRUE),
            mean_rsq       = mean(rsq, na.rm = TRUE),
            mean_mae       = mean(mae, na.rm = TRUE),
            sd_rmse        = sd(rmse, na.rm = TRUE),
            mean_high_rmse = mean(high_rmse, na.rm = TRUE),
            mean_high_rsq  = mean(high_rsq, na.rm = TRUE),
            mean_high_mae  = mean(high_mae, na.rm = TRUE),
            sd_high_rmse   = sd(high_rmse, na.rm = TRUE),
            total_high_sal = sum(n_high_sal, na.rm = TRUE),
            n_failed       = sum(failed)
         )
      
      cat(" -> RMSE:", round(summary_row$mean_rmse, 4),
          ", High-Sal RMSE:", round(summary_row$mean_high_rmse, 4))
      if (summary_row$n_failed > 0) cat(" [", summary_row$n_failed, "failed]")
      cat("\n")
      
      bind_cols(k_vals, summary_row)
   })
   
   cat("\n=== CV RESULTS ===\n")
   print(tune_results %>%
            arrange(mean_rmse) %>%
            select(all_of(c(active_k_types, "mean_rmse", "mean_rsq", "n_failed"))))
   cat("\n")
   
   # ============================================================================
   # SELECT BEST K
   # ============================================================================
   
   valid_results <- tune_results %>% filter(n_failed < length(folds))
   
   if (nrow(valid_results) == 0) {
      stop("All k configurations failed. Check data and formula.")
   }
   
   best_k <- valid_results %>% slice_min(mean_high_rmse, n = 1, with_ties = FALSE)
   
   cat("=== BEST K VALUES ===\n")
   for (k_type in active_k_types) cat(k_type, ":", best_k[[k_type]], "\n")
   cat("CV RMSE:", round(best_k$mean_rmse, 4), "+/-", round(best_k$sd_rmse, 4), "\n")
   cat("CV R2:", round(best_k$mean_rsq, 4), "\n\n")
   
   # ============================================================================
   # FIT FINAL MODEL
   # ============================================================================
   
   cat("Fitting final model...\n")
   final_formula <- build_gam_formula(best_k$k_h, best_k$k_interaction,
                                      best_k$k_sustained_flow, best_k$k_flushing_flow,
                                      best_k$k_physical, best_k$k_wind)
   
   print(final_formula)
   cat("\n")
   
   final_bam_args <- list(
      formula  = final_formula,
      data     = data_clean %>% select(all_of(model_cols)),
      family   = gam_family,
      method   = method,
      discrete = discrete,
      nthreads = nthreads,
      select   = gam_select
   )
   
   start_time <- Sys.time()
   final_gam  <- do.call(bam, final_bam_args)
   fit_time   <- difftime(Sys.time(), start_time, units = "secs")
   
   cat("Fit time:", round(fit_time, 2), "sec\n\n")
   
   # ============================================================================
   # MODEL SUMMARY
   # ============================================================================
   
   cat("=== MODEL SUMMARY ===\n")
   print(summary(final_gam))
   cat("\n")
   
   cat("Deviance explained:", round(summary(final_gam)$dev.expl * 100, 2), "%\n")
   cat("Adjusted R2:", round(summary(final_gam)$r.sq, 4), "\n")
   cat("AIC:", round(AIC(final_gam), 2), "\n\n")
   
   # ============================================================================
   # BASIS CHECK
   # ============================================================================
   
   cat("=== BASIS CHECK ===\n")
   cat("(Increase relevant k if k-index < 1 and p < 0.05)\n\n")
   print(k.check(final_gam, n.rep = 0))
   cat("\n")
   
   # ============================================================================
   # SMOOTH TERMS
   # ============================================================================
   
   s_table <- summary(final_gam)$s.table
   smooth_info <- tibble(
      term    = rownames(s_table),
      edf     = s_table[, "edf"],
      ref_df  = s_table[, "Ref.df"],
      F_stat  = s_table[, "F"],
      p_value = s_table[, "p-value"]
   ) %>% arrange(desc(edf))
   
   cat("=== SMOOTH TERMS ===\n")
   print(smooth_info, n = Inf)
   cat("\n")
   
   sig_terms <- smooth_info %>% filter(p_value < 0.05)
   cat("Significant (p < 0.05):", nrow(sig_terms), "/", nrow(smooth_info), "\n\n")
   
   # ============================================================================
   # FOLD-LEVEL RESULTS (for final formula)
   # ============================================================================
   
   cat("Computing fold-level results...\n")
   fold_level_results <- map_dfr(seq_along(folds), function(j) {
      res <- fit_fold(final_formula, folds[[j]]$train, folds[[j]]$test, j)
      
      tibble(
         id        = paste0("Fold", j),
         .metric   = c("rmse", "rsq", "mae", "high_rmse", "high_rsq", "high_mae"),
         .estimate = c(res$rmse, res$rsq, res$mae,
                       res$high_rmse, res$high_rsq, res$high_mae)
      )
   })
   cat("Done.\n\n")
   
   # ============================================================================
   # RETURN RESULTS
   # ============================================================================
   
   gam_workflow <- structure(
      list(fit = list(fit = final_gam, formula = final_formula, family = family_type)),
      class = c("workflow", "list")
   )
   
   list(
      tune_results = fold_level_results,
      tune_grid    = tune_results,
      best_params  = tibble(
         k_h              = best_k$k_h,
         k_interaction    = best_k$k_interaction,
         k_sustained_flow = best_k$k_sustained_flow,
         k_flushing_flow  = best_k$k_flushing_flow,
         k_physical       = best_k$k_physical,
         k_wind           = best_k$k_wind,
         family           = family_type,
         link             = link
      ),
      final_fit     = gam_workflow,
      gam_object    = final_gam,
      formula       = final_formula,
      smooth_info   = smooth_info,
      selected_vars = sig_terms$term,
      model_type    = "gam",
      transform_info = list(family = family_type, link = link)
   )
}