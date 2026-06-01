# Function to fit_gam
fit_gam <- function(data,
                    response = 'Salinity',
                    predictors = NULL,
                    folds = NULL,
                    high_salinity_threshold = 0.15,
                    
                    # Response transformation (ONLY for Gaussian family)
                    transform_response = "none",  # "none", "log", "sqrt"
                    
                    # Distribution family
                    family_type = "gaussian",  # "gaussian", "Gamma", "Tweedie"
                    link = NULL,  # NULL = auto-select
                    tweedie_p = 1.5,
                    
                    # Smoothing parameters by variable type
                    k_sustained_flow_range = c(8, 15),
                    k_flushing_flow_range = c(3, 10), 
                    k_physical_range = c(6, 12),
                    k_temporal_range = c(12, 12),
                    k_lagged_range = c(1, 3),
                    k_interaction_range = c(6, 6),
                    
                    # Interactions
                    interactions = list(),
                    
                    # AR(1) parameters
                    use_ar1 = FALSE,
                    rho = NULL,          # Fixed rho value
                    ar_start = NULL,     # Logical vector marking series breaks
                    
                    # Basis types
                    basis_default = 'tp',
                    basis_cyclical = 'cc',
                    
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
   
   # ============================================================================
   # VALIDATION: AR(1) parameters
   # ============================================================================
   
   if (use_ar1) {
      if (is.null(rho)) {
         stop("use_ar1 = TRUE requires rho to be specified (e.g., 0.5).")
      }
      if (is.null(ar_start)) {
         stop("use_ar1 = TRUE requires ar_start to be specified.")
      }
      if (!is.logical(ar_start)) {
         stop("ar_start must be a logical vector (TRUE/FALSE).")
      }
      if (abs(rho) >= 1) {
         stop("rho must be in (-1, 1). Got: ", rho)
      }
      if (length(ar_start) != nrow(data)) {
         stop("ar_start length (", length(ar_start), ") must match data rows (", 
              nrow(data), ").")
      }
      
      cat("=== AR(1) CONFIGURATION ===\n")
      cat("Fixed rho:", round(rho, 4), "\n")
      cat("Number of series segments:", sum(ar_start), "\n\n")
   }
   
   # ============================================================================
   # VALIDATION: Family and transformation compatibility
   # ============================================================================
   
   if (transform_response != "none" && family_type != "gaussian") {
      stop("Manual transformation only allowed with 'gaussian' family.\n",
           "Use appropriate link function instead.")
   }
   
   # ============================================================================
   # SET UP FAMILY OBJECT
   # ============================================================================
   
   if (is.null(link)) {
      link <- switch(family_type,
                     "gaussian" = "identity",
                     "Gamma" = "log",
                     "Tweedie" = "log")
      cat("Auto-selected link:", link, "for", family_type, "family\n\n")
   }
   
   if (family_type == "Gamma" && link == "identity") {
      warning("Identity link with Gamma can produce negative predictions. ",
              "Consider link = 'log'.\n")
   }
   
   gam_family <- switch(family_type,
                        "gaussian" = gaussian(link = link),
                        "Gamma" = Gamma(link = link),
                        "Tweedie" = Tweedie(p = tweedie_p, link = link),
                        stop("Unknown family_type"))
   
   # ============================================================================
   # PREPARE DATA
   # ============================================================================
   
   # Identify complete cases
   data_subset <- data %>%
      select(DateTime, all_of(response), all_of(predictors))
   
   complete_rows <- complete.cases(data_subset)
   
   # Subset ar_start for complete cases
   ar_start_clean <- if (use_ar1) ar_start[complete_rows] else NULL
   
   # Clean data
   data_clean <- data_subset %>%
      filter(complete_rows) %>%
      mutate(
         Response = .data[[response]],
         Response_original = .data[[response]]
      )
   
   if ("InFlushRegime" %in% predictors) {
      data_clean <- data_clean %>%
         mutate(InFlushRegime = factor(
            ifelse(InFlushRegime == 1, "Flushing", "NotFlushing"),
            levels = c("NotFlushing", "Flushing")
         ))
      
      if (nlevels(droplevels(data_clean$InFlushRegime)) < 2) {
         warning("InFlushRegime has only one level in full dataset. Check threshold.")
      }
   }
   
   cat("=== DATA PREPARATION ===\n")
   cat("Original rows:", format(nrow(data), big.mark = ","), "\n")
   cat("After removing NAs:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response range: [", round(min(data_clean$Response), 4), ", ",
       round(max(data_clean$Response), 4), "]\n")
   if (use_ar1) {
      cat("AR segments:", sum(ar_start_clean), "\n")
   }
   cat("\n")
   
   # ============================================================================
   # APPLY MANUAL TRANSFORMATION (Gaussian only)
   # ============================================================================
   
   if (family_type == "gaussian" && transform_response == "log") {
      if (any(data_clean$Response <= 0)) {
         stop("Cannot log-transform non-positive values.")
      }
      data_clean$Response <- log(data_clean$Response)
      cat("Applied log transformation\n")
      cat("Log-scale range: [", round(min(data_clean$Response), 4), ", ",
          round(max(data_clean$Response), 4), "]\n\n")
      
   } else if (family_type == "gaussian" && transform_response == "sqrt") {
      if (any(data_clean$Response < 0)) {
         stop("Cannot sqrt-transform negative values.")
      }
      data_clean$Response <- sqrt(data_clean$Response)
      cat("Applied sqrt transformation\n\n")
   }
   
   # ============================================================================
   # CHECK DATA REQUIREMENTS for non-Gaussian families
   # ============================================================================
   
   if (family_type == "Gamma" && any(data_clean$Response <= 0)) {
      n_bad <- sum(data_clean$Response <= 0)
      cat("WARNING: Gamma requires positive values. Found", n_bad, 
          "values ≤ 0. Adding 0.001.\n\n")
      data_clean$Response <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   if (family_type == "Tweedie" && any(data_clean$Response < 0)) {
      n_bad <- sum(data_clean$Response < 0)
      cat("WARNING: Tweedie requires non-negative values. Found", n_bad,
          "negative values. Setting to 0.001.\n\n")
      data_clean$Response <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   # ============================================================================
   # CLASSIFY PREDICTORS
   # ============================================================================
   
   sustained_flow_vars <- predictors[grepl("RollingDischarge|RollingAnomaly",
                                         predictors, ignore.case = TRUE)]
   
   flushing_flow_vars <- predictors[grepl("ExceedFlux|MaxDischarge",
                                          predictors, ignore.case = TRUE)]
   
   physical_vars <- predictors[grepl("Tid|RollingWindAlong|RollingWindCross",
                                     predictors, ignore.case = TRUE)]
   physical_vars <- physical_vars[!grepl("Dir", physical_vars)]
   
   temporal_vars <- predictors[grepl("Sin|Cos", predictors, ignore.case = TRUE)]
   lagged_vars   <- predictors[grepl("^Salinity_lag|^lag.*Salinity",
                                     predictors, ignore.case = TRUE)]
   other_vars    <- setdiff(predictors,
                            c(sustained_flow_vars, flushing_flow_vars,
                              physical_vars, temporal_vars, lagged_vars))
   other_vars    <- other_vars[!grepl("Dir", other_vars)]
   
   has_sustained_flow   <- length(sustained_flow_vars)   > 0
   has_flushing_flow  <- length(flushing_flow_vars)  > 0
   has_physical       <- length(physical_vars)       > 0
   has_temporal       <- length(temporal_vars)       > 0
   has_lagged         <- length(lagged_vars)         > 0
   has_other          <- length(other_vars)          > 0
   has_interactions   <- length(interactions)        > 0
   
   # ============================================================================
   # CREATE K TUNING GRID
   # ============================================================================
   
   k_sequences <- list()
   
   if (has_sustained_flow) {
      k_sequences$k_sustained_flow <- unique(round(seq(
         k_sustained_flow_range[1], k_sustained_flow_range[2], length.out = gam_levels)))
   }
   if (has_flushing_flow) {
      k_sequences$k_flushing_flow <- unique(round(seq(
         k_flushing_flow_range[1], k_flushing_flow_range[2], length.out = gam_levels)))
   }
   if (has_physical || has_other) {
      k_sequences$k_physical <- unique(round(seq(
         k_physical_range[1], k_physical_range[2], length.out = gam_levels)))
   }
   if (has_temporal) {
      k_sequences$k_temporal <- unique(round(seq(
         k_temporal_range[1], k_temporal_range[2], length.out = gam_levels)))
   }
   if (has_lagged) {
      k_sequences$k_lagged <- unique(round(seq(
         k_lagged_range[1], k_lagged_range[2], length.out = gam_levels)))
   }
   if (has_interactions) {
      k_sequences$k_interaction <- unique(round(seq(
         k_interaction_range[1], k_interaction_range[2], length.out = gam_levels)))
   }
   
   if (length(k_sequences) == 0) stop("No predictors or interactions specified!")
   
   k_grid <- expand.grid(k_sequences) %>% distinct()
   
   if (!has_sustained_flow)  k_grid$k_sustained_flow  <- k_sustained_flow_range[1]
   if (!has_flushing_flow) k_grid$k_flushing_flow <- k_flushing_flow_range[1]
   if (!has_physical && !has_other) k_grid$k_physical <- k_physical_range[1]
   if (!has_temporal)      k_grid$k_temporal      <- k_temporal_range[1]
   if (!has_lagged)        k_grid$k_lagged        <- k_lagged_range[1]
   if (!has_interactions)  k_grid$k_interaction   <- k_interaction_range[1]
   
   k_grid <- k_grid %>%
      select(k_sustained_flow, k_flushing_flow, k_physical,
             k_temporal, k_lagged, k_interaction)
   
   # ============================================================================
   # MODEL SETUP SUMMARY
   # ============================================================================
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response:", response, "\n")
   cat("Predictors:", length(predictors), "\n")
   cat("Family:", family_type, "with", link, "link\n")
   if (family_type == "gaussian" && transform_response != "none") {
      cat("Response transformation:", transform_response, "\n")
   }
   if (family_type == "Tweedie") {
      cat("Tweedie power:", tweedie_p, "\n")
   }
   if (use_ar1) {
      cat("AR(1): enabled (rho =", round(rho, 4), ")\n")
   }
   cat("\n")
   
   cat("Variable groups:\n")
   if (has_sustained_flow)  cat("  Sustained flow:  ", paste(sustained_flow_vars,  collapse=", "), "\n")
   if (has_flushing_flow) cat("  Flushing flow: ", paste(flushing_flow_vars, collapse=", "), "\n")
   if (has_physical)      cat("  Physical:      ", paste(physical_vars,      collapse=", "), "\n")
   if (has_temporal)      cat("  Temporal:      ", paste(temporal_vars,      collapse=", "), "\n")
   if (has_lagged)        cat("  Lagged:        ", paste(lagged_vars,        collapse=", "), "\n")
   if (has_other)         cat("  Other:         ", paste(other_vars,         collapse=", "), "\n")
   if (has_interactions)  cat("  Interactions:  ", length(interactions),                    "\n")
   
   active_k_types <- names(k_sequences)
   cat("Tuning", nrow(k_grid), "k combinations\n")
   print(k_grid %>% select(all_of(active_k_types)))
   cat("\n")
   
   # ============================================================================
   # BUILD FORMULA FUNCTION
   # ============================================================================
   
   build_gam_formula <- function(k_sustained_flow, k_flushing_flow, k_physical,
                                 k_temporal, k_lagged, k_interaction) {
      terms <- c()
      
      if (has_sustained_flow) {
         terms <- c(terms, paste0("s(", sustained_flow_vars, ", k=", k_sustained_flow,
                                  ", bs='", basis_default, "')"))
      }
      
      if (has_flushing_flow) {
         exceed_vars     <- flushing_flow_vars[grepl("ExceedFlux",     flushing_flow_vars)]
         days_since_vars <- flushing_flow_vars[grepl("DaysSinceFlush", flushing_flow_vars)]
         
         if (length(exceed_vars) > 0) {
            if ("InFlushRegime" %in% predictors && is.factor(data_clean$InFlushRegime)) {
               terms <- c(terms, paste0("s(", exceed_vars,
                                        ", by=InFlushRegime, k=", k_flushing_flow,
                                        ", bs='", basis_default, "')"))
            } else {
               terms <- c(terms, paste0("s(", exceed_vars, ", k=", k_flushing_flow,
                                        ", bs='", basis_default, "')"))
            }
         }
         
         if (length(days_since_vars) > 0) {
            terms <- c(terms, paste0("s(", days_since_vars, ", k=", k_flushing_flow,
                                     ", bs='", basis_default, "')"))
         }
      }
      
      if (has_physical) {
         phys_vars_copy <- physical_vars
         wind_vars <- phys_vars_copy[grepl("RollingWindAlong|RollingWindCross", phys_vars_copy)]
         
         if (length(wind_vars) > 0 && "WindDir" %in% predictors) {
            terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_physical,
                                     ", bs='", basis_default, "')"))
            phys_vars_copy <- setdiff(phys_vars_copy, wind_vars)
         }
         if (length(phys_vars_copy) > 0) {
            terms <- c(terms, paste0("s(", phys_vars_copy, ", k=", k_physical,
                                     ", bs='", basis_default, "')"))
         }
      }
      
      if (has_temporal) {
         terms <- c(terms, paste0("s(", temporal_vars, ", k=", k_temporal,
                                  ", bs='", basis_cyclical, "')"))
      }
      
      if (has_lagged) {
         terms <- c(terms, lagged_vars)
      }
      
      if (has_other) {
         terms <- c(terms, paste0("s(", other_vars, ", k=", k_physical,
                                  ", bs='", basis_default, "')"))
      }
      
      if (has_interactions) {
         for (int in interactions) {
            if (all(int$vars %in% predictors)) {
               terms <- c(terms, paste0("ti(", paste(int$vars, collapse=", "),
                                        ", k=", k_interaction, ")"))
            }
         }
      }
      
      as.formula(paste("Response ~", paste(terms, collapse=" + ")))
   }
   
   # ============================================================================
   # HELPER FUNCTION: Fit and evaluate on fold
   # ============================================================================
   
   fit_fold <- function(formula, train_idx, test_idx, fold_num) {
      
      # Subset data
      train_data <- data_clean[train_idx, ] %>% 
         select(-DateTime, -Response_original)
      test_data <- data_clean[test_idx, ]
      
      # Build bam arguments
      bam_args <- list(
         formula = formula,
         data = train_data,
         family = gam_family,
         method = method,
         discrete = discrete,
         nthreads = nthreads,
         select = gam_select
      )
      
      # Add AR(1) parameters if needed
      if (use_ar1) {
         # Create AR.start for this training fold
         fold_ar_start <- ar_start_clean[train_idx]
         fold_ar_start[1] <- TRUE  # First obs must be series start
         
         bam_args$rho <- rho
         bam_args$AR.start <- fold_ar_start
      }
      
      # Fit model
      gam_fit <- tryCatch({
         suppressWarnings(do.call(bam, bam_args))
      }, error = function(e) {
         cat("    [Fold", fold_num, "error:", e$message, "]\n")
         return(NULL)
      })
      
      if (is.null(gam_fit)) {
         return(list(
            rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
            high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
            n_high_sal = 0L, failed = TRUE
         ))
      }
      
      # Predict
      preds <- tryCatch({
         predict(gam_fit, 
                 newdata = test_data %>% select(-DateTime, -Response_original), 
                 type = "response")
      }, error = function(e) {
         cat("    [Fold", fold_num, "prediction error:", e$message, "]\n")
         return(NULL)
      })
      
      if (is.null(preds) || any(!is.finite(preds))) {
         return(list(
            rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
            high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
            n_high_sal = 0L, failed = TRUE
         ))
      }
      
      # Back-transform if needed
      if (family_type == "gaussian" && transform_response == "log") {
         sigma_sq <- summary(gam_fit)$scale
         preds_original <- exp(preds + sigma_sq/2)
      } else if (family_type == "gaussian" && transform_response == "sqrt") {
         preds_original <- preds^2
      } else {
         # Gamma/Tweedie with log link: already on original scale
         preds_original <- preds
      }
      
      if (any(!is.finite(preds_original))) {
         return(list(
            rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
            high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
            n_high_sal = 0L, failed = TRUE
         ))
      }
      
      # Calculate overall metrics
      overall_rmse <- sqrt(mean((test_data$Response_original - preds_original)^2))
      overall_rsq <- cor(test_data$Response_original, preds_original)^2
      overall_mae <- mean(abs(test_data$Response_original - preds_original))
      
      # Calculate high salinity metrics
      high_idx <- test_data$Response_original > high_salinity_threshold
      n_high <- sum(high_idx)
      
      if (n_high > 1) {
         high_rmse <- sqrt(mean((test_data$Response_original[high_idx] - 
                                    preds_original[high_idx])^2))
         high_rsq <- cor(test_data$Response_original[high_idx], 
                         preds_original[high_idx])^2
         high_mae <- mean(abs(test_data$Response_original[high_idx] - 
                                 preds_original[high_idx]))
      } else {
         high_rmse <- NA_real_
         high_rsq <- NA_real_
         high_mae <- NA_real_
      }
      
      list(
         rmse = overall_rmse,
         rsq = overall_rsq,
         mae = overall_mae,
         high_rmse = high_rmse,
         high_rsq = high_rsq,
         high_mae = high_mae,
         n_high_sal = n_high,
         failed = FALSE
      )
   }
   
   # ============================================================================
   # CROSS-VALIDATION
   # ============================================================================
   
   cat("Running CV...\n")
   
   tune_results <- map_dfr(1:nrow(k_grid), function(i) {
      
      k_vals <- k_grid[i, ]
      formula <- build_gam_formula(k_vals$k_sustained_flow, k_vals$k_flushing_flow,
                                   k_vals$k_physical, k_vals$k_temporal,
                                   k_vals$k_lagged, k_vals$k_interaction)
      
      # Print active k values
      active_k_str <- paste(
         sapply(active_k_types, function(k) paste0(k, "=", k_vals[[k]])),
         collapse = ", "
      )
      cat("  ", active_k_str)
      
      # Evaluate across folds
      fold_results <- map_dfr(seq_along(folds), function(j) {
         res <- fit_fold(formula, folds[[j]]$train, folds[[j]]$test, j)
         tibble(fold = j, !!!res)
      })
      
      # Aggregate results
      summary <- fold_results %>%
         summarize(
            mean_rmse = mean(rmse, na.rm = TRUE),
            mean_rsq = mean(rsq, na.rm = TRUE),
            mean_mae = mean(mae, na.rm = TRUE),
            sd_rmse = sd(rmse, na.rm = TRUE),
            mean_high_rmse = mean(high_rmse, na.rm = TRUE),
            mean_high_rsq = mean(high_rsq, na.rm = TRUE),
            mean_high_mae = mean(high_mae, na.rm = TRUE),
            sd_high_rmse = sd(high_rmse, na.rm = TRUE),
            total_high_sal = sum(n_high_sal, na.rm = TRUE),
            n_failed = sum(failed)
         )
      
      cat(" → RMSE:", round(summary$mean_rmse, 4), ", High-Sal RMSE: ", round(summary$mean_high_rmse, 4))
      if (summary$n_failed > 0) {
         cat(" [", summary$n_failed, "failed]")
      }
      cat("\n")
      
      bind_cols(k_vals, summary)
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
      stop("All k configurations failed. Check data and AR setup.")
   }
   
   best_k <- valid_results %>% slice_min(mean_rmse, n = 1, with_ties = FALSE)
   
   cat("=== BEST K VALUES ===\n")
   for (k_type in active_k_types) {
      cat(k_type, ":", best_k[[k_type]], "\n")
   }
   cat("CV RMSE:", round(best_k$mean_rmse, 4), "±", round(best_k$sd_rmse, 4), "\n")
   cat("CV R²:", round(best_k$mean_rsq, 4), "\n\n")
   
   # ============================================================================
   # FIT FINAL MODEL
   # ============================================================================
   
   cat("Fitting final model...\n")
   final_formula <- build_gam_formula(best_k$k_sustained_flow, best_k$k_flushing_flow,
                                      best_k$k_physical, best_k$k_temporal,
                                      best_k$k_lagged, best_k$k_interaction)
   
   print(final_formula)
   cat("\n")
   
   # Build final bam arguments
   final_bam_args <- list(
      formula = final_formula,
      data = data_clean %>% select(-DateTime, -Response_original),
      family = gam_family,
      method = method,
      discrete = discrete,
      nthreads = nthreads
   )
   
   if (use_ar1) {
      final_bam_args$rho <- rho
      final_bam_args$AR.start <- ar_start_clean
   }
   
   start_time <- Sys.time()
   final_gam <- do.call(bam, final_bam_args)
   fit_time <- difftime(Sys.time(), start_time, units = "secs")
   
   cat("Fit time:", round(fit_time, 2), "sec\n\n")
   
   # ============================================================================
   # MODEL SUMMARY
   # ============================================================================
   
   cat("=== MODEL SUMMARY ===\n")
   print(summary(final_gam))
   cat("\n")
   
   cat("Deviance explained:", round(summary(final_gam)$dev.expl * 100, 2), "%\n")
   cat("Adjusted R²:", round(summary(final_gam)$r.sq, 4), "\n")
   cat("AIC:", round(AIC(final_gam), 2), "\n\n")
   
   # ============================================================================
   # BASIS CHECK
   # ============================================================================
   
   cat("=== BASIS CHECK ===\n")
   cat("(Increase k if k-index < 1 and p < 0.05)\n\n")
   print(k.check(final_gam, n.rep = 0))
   cat("\n")
   
   # ============================================================================
   # SMOOTH TERMS
   # ============================================================================
   
   if ('k_flow' %in% active_k_types) {
      s_table <- summary(final_gam)$s.table
      smooth_info <- tibble(
         term = rownames(s_table),
         edf = s_table[, "edf"],
         ref_df = s_table[, "Ref.df"],
         F_stat = s_table[, "F"],
         p_value = s_table[, "p-value"]
      ) %>% arrange(desc(edf))
      
      cat("=== SMOOTH TERMS ===\n")
      print(smooth_info, n = Inf)
      cat("\n")
      
      sig_terms <- smooth_info %>% filter(p_value < 0.05)
      cat("Significant (p < 0.05):", nrow(sig_terms), "/", nrow(smooth_info), "\n\n")
      
   } else  {
      smooth_info <- NULL
      sig_terms <- NULL
   }
   
   # ============================================================================
   # FOLD-LEVEL RESULTS
   # ============================================================================
   
   cat("Computing fold-level results...\n")
   fold_level_results <- map_dfr(seq_along(folds), function(j) {
      res <- fit_fold(final_formula, folds[[j]]$train, folds[[j]]$test, j)
      
      tibble(
         id = paste0("Fold", j),
         .metric = c("rmse", "rsq", "mae", "high_rmse", "high_rsq", "high_mae"),
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
      tune_grid = tune_results,
      best_params = tibble(
         k_sustained_flow  = best_k$k_sustained_flow,
         k_flushing_flow = best_k$k_flushing_flow,
         k_physical      = best_k$k_physical,
         k_temporal      = best_k$k_temporal,
         k_lagged        = best_k$k_lagged,
         k_interaction   = best_k$k_interaction,
         family          = family_type,
         link            = link,
         transform       = if(family_type == "gaussian") transform_response else "via_link",
         use_ar1         = use_ar1,
         rho             = if(use_ar1) rho else NA_real_
      ),
      final_fit = gam_workflow,
      gam_object = final_gam,
      formula = final_formula,
      smooth_info = if (!is.null(smooth_info)) smooth_info else NULL,
      selected_vars = if (!is.null(sig_terms)) sig_terms$term else NULL,
      model_type = "gam",
      transform_info = list(
         family = family_type,
         link = link,
         manual_transform = if(family_type == "gaussian") transform_response else "none",
         sigma_sq = if(family_type == "gaussian" && transform_response == "log") {
            summary(final_gam)$scale
         } else NULL
      ),
      ar_info = if(use_ar1) {
         list(rho = rho, n_segments = sum(ar_start_clean))
      } else NULL
   )
}