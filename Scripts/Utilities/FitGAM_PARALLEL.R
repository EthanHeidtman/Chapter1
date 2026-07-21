# =============================================================================
# Function:       fit_gam  (parallelized version)
#
# CHANGES FROM PRIOR VERSION:
#   - K-combo-level parallelization via future/furrr. Each k-combo runs its
#     own 11-fold loop as a single task; tasks are distributed across workers
#     via plan() set by the CALLER (Script 04), not inside this function --
#     this keeps fit_gam agnostic to laptop vs cluster vs Windows.
#   - Every bam() call inside the CV loop now forces nthreads = 1. Benchmarking
#     confirmed multithreading inside bam does not meaningfully help at this
#     fit size, and combining it with process-level parallelism risks core
#     oversubscription. This is NOT exposed as a tunable.
#   - Per-fold convergence is now captured via withCallingHandlers around each
#     bam() call (not just tryCatch for hard errors). Two new columns appear
#     in fold-level output: `converged` (FALSE if bgam.fitd reports
#     non-convergence) and `warning_text` (concatenated warning messages, for
#     inspection). These roll up into candidate_summary as a diagnostic
#     column (n_folds_converged out of length(folds)) -- reported, not
#     filtering. This does not replace or reinstate the k.check hurdle that
#     was dropped from candidate selection; it is a distinct, convergence-
#     specific check.
#   - No <<- accumulation. Each parallel task returns its own fold_results
#     tibble (fold_cv_all is rebuilt via bind_rows on the collected results).
#   - Progress reporting uses progressr (cross-platform, future-aware) in
#     place of live per-combo cat() output, since multisession workers do not
#     stream stdout in real time.
#   - Refit-top-10-for-EDF stage is UNCHANGED and stays serial (cheap relative
#     to the grid search; no benefit to parallelizing 10 fits).
#   - Aggregation, candidate ranking (by mean_high_rmse), and all four plots
#     (pA-pD) are UNCHANGED and consume the same tibble shapes as before.
# =============================================================================

fit_gam <- function(data,
                    response = 'Salinity_h',
                    predictors = NULL,
                    folds = NULL,
                    high_salinity_threshold = 0.16,
                    
                    family_type = "gaussian",
                    link = NULL,
                    tweedie_p = 1.5,
                    
                    # k values to test (upper bounds on wiggliness)
                    k_h_fixed              = 4,
                    k_physical_fixed       = 10,
                    k_interaction_range    = c(5, 16),
                    k_sustained_flow_range = c(5, 16),
                    k_flushing_flow_range  = c(5, 16),
                    k_wind_range           = c(5, 16),
                    
                    interactions = list(),
                    
                    basis_default = 'tp',
                    basis_horizon = 'cr',
                    
                    method     = 'fREML',
                    discrete   = TRUE,
                    nthreads   = 4,       # still used for the serial top-10 refit stage only
                    gam_select = TRUE,
                    
                    gam_levels       = 3,
                    n_top_candidates = 10,
                    plot_output_dir  = 'Outputs/Plots/UnifiedGAM/GAMSelection',
                    
                    # --- new parallelization arguments ---
                    n_workers = NULL,      # NULL = auto (detectCores() - 2); caller may override
                    show_progress = TRUE) {
   
   library(mgcv)
   library(dplyr)
   library(purrr)
   library(ggplot2)
   library(tidyr)
   library(ggrepel)
   library(future)
   library(furrr)
   library(progressr)
   
   if (!('h' %in% predictors)) {
      stop("predictors must include 'h' for the unified multi-horizon GAM.")
   }
   
   gam_colors <- list(
      primary   = "#f58220",
      secondary = "#009bba",
      tertiary  = "#fdb515",
      dark      = "#002030"
   )
   
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
   
   h_var            <- 'h'
   non_h_predictors <- setdiff(predictors, h_var)
   
   lag_vars       <- non_h_predictors[grepl("LagSalinity",                    non_h_predictors, ignore.case = TRUE)]
   sustained_vars <- non_h_predictors[grepl("RollingDischarge|RollingAnomaly",non_h_predictors, ignore.case = TRUE)]
   flushing_vars  <- non_h_predictors[grepl("MaxDischarge|ExceedFlux",        non_h_predictors, ignore.case = TRUE)]
   tide_vars      <- non_h_predictors[grepl("TideRange|TideMean",             non_h_predictors, ignore.case = TRUE)]
   wind_vars      <- non_h_predictors[grepl("RollingWindAlong|RollingWindCross", non_h_predictors, ignore.case = TRUE)]
   
   classified <- c(lag_vars, sustained_vars, flushing_vars, tide_vars, wind_vars)
   other_vars <- setdiff(non_h_predictors, classified)
   
   has_lag          <- length(lag_vars)       > 0
   has_sustained    <- length(sustained_vars) > 0
   has_flushing     <- length(flushing_vars)  > 0
   has_tide         <- length(tide_vars)      > 0
   has_wind         <- length(wind_vars)      > 0
   has_other        <- length(other_vars)     > 0
   has_interactions <- length(interactions)   > 0
   
   ti_vars <- non_h_predictors
   
   # Validate interaction specs up front so failures surface before any CV work
   if (has_interactions) {
      for (int in interactions) {
         int_vars <- if (is.list(int)) int$vars else int
         if (length(int_vars) != 2) {
            stop("Each interaction must specify exactly 2 variables. Got: ",
                 paste(int_vars, collapse = ", "))
         }
         if (!all(int_vars %in% predictors)) {
            stop("Interaction pair (", paste(int_vars, collapse = ", "),
                 ") includes a variable not in `predictors`.")
         }
      }
   }
   
   data_subset <- data %>%
      mutate(.row_id = row_number()) %>%
      select(.row_id, DateTime, all_of(response), all_of(predictors))
   
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
   cat("Original rows:", format(nrow(data),       big.mark = ","), "\n")
   cat("After removing NAs:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response range: [", round(min(data_clean$Response), 4), ", ",
       round(max(data_clean$Response), 4), "]\n\n")
   
   if (family_type == "Gamma" && any(data_clean$Response <= 0)) {
      n_bad <- sum(data_clean$Response <= 0)
      cat("WARNING: Gamma requires positive values. Found", n_bad, "values <= 0. Adding 0.001.\n\n")
      data_clean$Response          <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   if (family_type == "Tweedie" && any(data_clean$Response < 0)) {
      n_bad <- sum(data_clean$Response < 0)
      cat("WARNING: Tweedie requires non-negative values. Found", n_bad, "negative values. Setting to 0.001.\n\n")
      data_clean$Response          <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   model_cols <- setdiff(names(data_clean),
                         c(".row_id", "DateTime", response, "Response_original"))
   
   # ============================================================================
   # K TUNING GRID  (unchanged)
   # ============================================================================
   
   k_sequences <- list()
   if (has_sustained) k_sequences$k_sustained_flow <- unique(round(seq(k_sustained_flow_range[1], k_sustained_flow_range[2], length.out = gam_levels)))
   if (has_flushing)  k_sequences$k_flushing_flow  <- unique(round(seq(k_flushing_flow_range[1],  k_flushing_flow_range[2],  length.out = gam_levels)))
   if (has_wind)      k_sequences$k_wind           <- unique(round(seq(k_wind_range[1],            k_wind_range[2],            length.out = gam_levels)))
   k_sequences$k_interaction <- unique(round(seq(k_interaction_range[1], k_interaction_range[2], length.out = gam_levels)))
   
   k_grid <- expand.grid(k_sequences) %>% distinct()
   
   k_grid$k_h        <- k_h_fixed
   k_grid$k_physical <- k_physical_fixed
   if (!has_sustained) k_grid$k_sustained_flow <- k_sustained_flow_range[1]
   if (!has_flushing)  k_grid$k_flushing_flow  <- k_flushing_flow_range[1]
   if (!has_wind)       k_grid$k_wind           <- k_wind_range[1]
   
   k_grid <- k_grid %>%
      select(k_h, k_interaction, k_sustained_flow, k_flushing_flow, k_physical, k_wind) %>%
      mutate(k_index = row_number())
   
   active_k_types <- names(k_sequences)  # only the searched dims, for CV reporting
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response:", response, "\n")
   cat("Family:", family_type, "with", link, "link\n\n")
   cat("k_h FIXED at", k_h_fixed, "| k_physical FIXED at", k_physical_fixed, "\n")
   cat("Term groups:\n")
   if (has_lag)       cat("  LagSalinity:    ", paste(lag_vars,       collapse = ", "), "(linear)\n")
   if (has_sustained) cat("  Sustained flow: ", paste(sustained_vars, collapse = ", "), "\n")
   if (has_flushing)  cat("  Flushing flow:  ", paste(flushing_vars,  collapse = ", "), "\n")
   if (has_tide)      cat("  Tide:           ", paste(tide_vars,      collapse = ", "), "\n")
   if (has_wind)      cat("  Wind:           ", paste(wind_vars,      collapse = ", "), "(by = WindDir)\n")
   if (has_other)     cat("  Other:          ", paste(other_vars,     collapse = ", "), "\n")
   cat("\nTuning", nrow(k_grid), "k combinations across:", paste(active_k_types, collapse = ", "), "\n")
   
   if (has_interactions) {
      int_labels <- sapply(interactions, function(int) {
         v <- if (is.list(int)) int$vars else int
         paste0("ti(", v[1], ", ", v[2], ")")
      })
      cat("\nVariable-variable interactions (var-interactions, pooled, reuse k_interaction):\n")
      cat("  ", paste(int_labels, collapse = ", "), "\n")
   }
   cat("\n")
   
   # ============================================================================
   # BUILD FORMULA  (unchanged)
   # ============================================================================
   
   build_gam_formula <- function(k_h, k_interaction, k_sustained_flow,
                                 k_flushing_flow, k_physical, k_wind) {
      terms <- c()
      terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", basis_horizon, "')"))
      if (has_lag)       terms <- c(terms, lag_vars)
      if (has_sustained) terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow, ", bs='", basis_default, "')"))
      if (has_flushing)  terms <- c(terms, paste0("s(", flushing_vars,  ", k=", k_flushing_flow,  ", bs='", basis_default, "')"))
      if (has_tide)      terms <- c(terms, paste0("s(", tide_vars,      ", k=", k_physical,        ", bs='", basis_default, "')"))
      if (has_wind)      terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,    ", bs='", basis_default, "')"))
      if (has_other)     terms <- c(terms, paste0("s(", other_vars,     ", k=", k_physical,        ", bs='", basis_default, "')"))
      for (var in ti_vars) {
         terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                                  "), bs=c('", basis_horizon, "', '", basis_default, "'))"))
      }
      if (has_interactions) {
         for (int in interactions) {
            int_vars <- if (is.list(int)) int$vars else int
            terms <- c(terms, paste0(
               "ti(", int_vars[1], ", ", int_vars[2],
               ", k=c(", k_interaction, ", ", k_interaction, ")",
               ", bs=c('", basis_default, "', '", basis_default, "'))"
            ))
         }
      }
      as.formula(paste("Response ~", paste(terms, collapse = " + ")))
   }
   
   # ============================================================================
   # FOLD-FITTING UNIT  (nthreads forced to 1; convergence + warnings captured)
   # ============================================================================
   
   fit_fold <- function(formula, train_idx, test_idx, fold_num) {
      
      train_data <- data_clean %>% filter(.row_id %in% train_idx) %>% select(all_of(model_cols))
      test_data  <- data_clean %>% filter(.row_id %in% test_idx)
      
      warning_msgs <- character(0)
      
      gam_fit <- tryCatch({
         withCallingHandlers({
            mgcv::bam(
               formula  = formula,
               data     = train_data,
               family   = gam_family,
               method   = method,
               discrete = TRUE,     # multi-threading requires discrete=TRUE, but nthreads is forced to 1 below
               nthreads = 1L,       # <-- forced; see header comment. Not a tunable.
               control  = list(nthreads = 1L)
            )
         }, warning = function(w) {
            warning_msgs[[length(warning_msgs) + 1]] <<- conditionMessage(w)
            invokeRestart("muffleWarning")
         })
      }, error = function(e) {
         cat("    [Fold", fold_num, "error:", e$message, "]\n")
         return(NULL)
      })
      
      converged <- !any(grepl("did not converge", warning_msgs, fixed = TRUE))
      warning_text <- if (length(warning_msgs) > 0) paste(unique(warning_msgs), collapse = " | ") else NA_character_
      
      fail <- list(rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
                   high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
                   n_high_sal = 0L, failed = TRUE,
                   converged = FALSE, warning_text = warning_text)
      
      if (is.null(gam_fit)) return(fail)
      
      preds <- tryCatch({
         predict(gam_fit, newdata = test_data %>% select(all_of(model_cols)), type = "response")
      }, error = function(e) {
         cat("    [Fold", fold_num, "prediction error:", e$message, "]\n")
         return(NULL)
      })
      
      if (is.null(preds) || any(!is.finite(preds))) {
         fail$converged    <- converged
         fail$warning_text <- warning_text
         return(fail)
      }
      
      obs      <- test_data$Response_original
      high_idx <- obs > high_salinity_threshold
      n_high   <- sum(high_idx)
      
      high_rmse <- if (n_high > 1) sqrt(mean((obs[high_idx] - preds[high_idx])^2)) else NA_real_
      high_rsq  <- if (n_high > 1) cor(obs[high_idx], preds[high_idx])^2           else NA_real_
      high_mae  <- if (n_high > 1) mean(abs(obs[high_idx] - preds[high_idx]))       else NA_real_
      
      list(
         rmse         = sqrt(mean((obs - preds)^2)),
         rsq          = cor(obs, preds)^2,
         mae          = mean(abs(obs - preds)),
         high_rmse    = high_rmse,
         high_rsq     = high_rsq,
         high_mae     = high_mae,
         n_high_sal   = n_high,
         failed       = FALSE,
         converged    = converged,
         warning_text = warning_text
      )
   }
   
   # ============================================================================
   # PER-K-COMBO TASK  (this is the unit distributed across parallel workers)
   # Runs all folds for one k-combo, returns a single tibble. No <<- anywhere.
   # ============================================================================
   
   fit_one_kcombo <- function(k_row) {
      
      formula <- build_gam_formula(k_row$k_h, k_row$k_interaction,
                                   k_row$k_sustained_flow, k_row$k_flushing_flow,
                                   k_row$k_physical, k_row$k_wind)
      
      fold_results <- map_dfr(seq_along(folds), function(j) {
         res <- fit_fold(formula, folds[[j]]$train, folds[[j]]$test, j)
         tibble(fold = j, !!!res)
      })
      
      fold_results %>% mutate(k_index = k_row$k_index)
   }
   
   # ============================================================================
   # RUN CV ACROSS K-GRID, PARALLELIZED AT THE K-COMBO LEVEL
   #
   # plan() is expected to already be set by the caller (Script 04) so that
   # this function stays agnostic to laptop / Windows / cluster. If no plan
   # has been set, future defaults to sequential -- fit_gam will still work,
   # just without the speedup, which is a safe fallback rather than a failure.
   # ============================================================================
   
   cat("Running CV across", nrow(k_grid), "k combinations",
       "(plan:", class(future::plan())[1], ")...\n\n")
   
   k_row_list <- split(k_grid, k_grid$k_index)
   
   run_kcombo_grid <- function() {
      if (isTRUE(show_progress)) {
         progressr::with_progress({
            p <- progressr::progressor(along = k_row_list)
            furrr::future_map(k_row_list, function(k_row) {
               res <- fit_one_kcombo(k_row)
               p(sprintf("k_index=%d", k_row$k_index))
               res
            }, .options = furrr::furrr_options(seed = TRUE))
         })
      } else {
         furrr::future_map(k_row_list, fit_one_kcombo,
                           .options = furrr::furrr_options(seed = TRUE))
      }
   }
   
   all_fold_results_list <- run_kcombo_grid()
   fold_cv_all <- bind_rows(all_fold_results_list)
   
   # ============================================================================
   # AGGREGATE PER K-COMBO  (unchanged metrics, + new convergence diagnostic)
   # ============================================================================
   
   tune_results <- fold_cv_all %>%
      group_by(k_index) %>%
      summarize(
         mean_rmse           = mean(rmse,      na.rm = TRUE),
         mean_rsq            = mean(rsq,       na.rm = TRUE),
         mean_mae            = mean(mae,       na.rm = TRUE),
         sd_rmse             = sd(rmse,        na.rm = TRUE),
         mean_high_rmse      = mean(high_rmse, na.rm = TRUE),
         mean_high_rsq       = mean(high_rsq,  na.rm = TRUE),
         mean_high_mae       = mean(high_mae,  na.rm = TRUE),
         sd_high_rmse        = sd(high_rmse,   na.rm = TRUE),
         total_high_sal      = sum(n_high_sal, na.rm = TRUE),
         n_failed            = sum(failed),
         n_folds_converged   = sum(converged, na.rm = TRUE),   # <-- new diagnostic
         n_folds_total       = n(),
         .groups = "drop"
      ) %>%
      left_join(k_grid, by = "k_index") %>%
      select(all_of(c("k_index", active_k_types, "k_h", "k_physical")),
             mean_rmse, mean_rsq, mean_mae, sd_rmse,
             mean_high_rmse, mean_high_rsq, mean_high_mae, sd_high_rmse,
             total_high_sal, n_failed, n_folds_converged, n_folds_total)
   
   cat("\n=== CV RESULTS ===\n")
   print(tune_results %>%
            arrange(mean_high_rmse) %>%
            select(all_of(c(active_k_types, "mean_rmse", "mean_high_rmse", "sd_high_rmse",
                            "n_failed", "n_folds_converged"))))
   cat("\n")
   
   n_candidates <- min(n_top_candidates,
                       nrow(tune_results %>% filter(n_failed < length(folds))))
   
   top_candidates_meta <- tune_results %>%
      filter(n_failed < length(folds)) %>%
      arrange(mean_high_rmse) %>%
      slice_head(n = n_candidates) %>%
      mutate(candidate_rank = row_number())
   
   cat("=== TOP", n_candidates, "CANDIDATES ===\n")
   print(top_candidates_meta %>%
            select(candidate_rank, all_of(active_k_types),
                   mean_rmse, mean_high_rmse, sd_high_rmse, n_folds_converged))
   cat("\n")
   
   # ============================================================================
   # REFIT TOP CANDIDATES — EXTRACT EDF  (unchanged, stays serial)
   # ============================================================================
   
   cat("Refitting top", n_candidates, "candidates to extract EDF (models not retained)...\n\n")
   
   full_train_data <- data_clean %>% select(all_of(model_cols))
   
   candidate_edf_tables <- map(1:nrow(top_candidates_meta), function(i) {
      
      meta    <- top_candidates_meta[i, ]
      formula <- build_gam_formula(meta$k_h, meta$k_interaction,
                                   meta$k_sustained_flow, meta$k_flushing_flow,
                                   meta$k_physical, meta$k_wind)
      
      cat("  Candidate", meta$candidate_rank,
          paste(sapply(active_k_types, function(k) paste0(k, "=", meta[[k]])), collapse = ", "))
      
      gam_fit <- tryCatch({
         do.call(bam, list(
            formula  = formula,
            data     = full_train_data,
            family   = gam_family,
            method   = method,
            discrete = discrete,
            nthreads = nthreads,
            select   = gam_select
         ))
      }, error = function(e) {
         cat(" [refit error:", e$message, "]\n")
         return(NULL)
      })
      
      if (is.null(gam_fit)) {
         cat(" -> refit failed\n")
         return(NULL)
      }
      
      s_table   <- summary(gam_fit)$s.table
      total_edf <- sum(s_table[, "edf"])
      
      cat(" -> Total EDF:", round(total_edf, 2), "\n")
      
      edf_tbl <- tibble(
         candidate_rank = meta$candidate_rank,
         term           = rownames(s_table),
         edf            = s_table[, "edf"],
         p_value        = s_table[, "p-value"]
      ) %>%
         mutate(term_group = case_when(
            grepl("^ti\\(h,",                        term) & grepl("LagSalinity",                     term) ~ "LagSalinity",
            grepl("^ti\\(h,",                        term) & grepl("RollingDischarge|RollingAnomaly", term) ~ "SustainedDischarge",
            grepl("^ti\\(h,",                        term) & grepl("MaxDischarge|ExceedFlux",         term) ~ "FlushingDischarge",
            grepl("^ti\\(h,",                        term) & grepl("TideRange|TideMean",              term) ~ "Tide",
            grepl("^ti\\(h,",                        term) & grepl("RollingWindAlong|RollingWindCross",term) ~ "Wind",
            grepl("^ti\\(",                          term) & !grepl("^ti\\(h,", term)                       ~ "VarInteraction",
            grepl("LagSalinity",                     term) ~ "LagSalinity",
            grepl("RollingDischarge|RollingAnomaly", term) ~ "SustainedDischarge",
            grepl("MaxDischarge|ExceedFlux",         term) ~ "FlushingDischarge",
            grepl("TideRange|TideMean",              term) ~ "Tide",
            grepl("RollingWindAlong|RollingWindCross",term) ~ "Wind",
            grepl("^s\\(h\\)",                        term) ~ "Horizon",
            TRUE                                           ~ "Other"
         ))
      
      rm(gam_fit)
      gc(verbose = FALSE)
      
      edf_tbl
   })
   
   cat("\n")
   
   # ============================================================================
   # CANDIDATE SUMMARY  (+ n_folds_converged carried through as a diagnostic)
   # ============================================================================
   
   candidate_summary <- map_dfr(seq_len(nrow(top_candidates_meta)), function(i) {
      meta      <- top_candidates_meta[i, ]
      edf_tbl   <- candidate_edf_tables[[i]]
      if (is.null(edf_tbl)) return(NULL)
      tibble(
         candidate_rank    = meta$candidate_rank,
         total_edf         = sum(edf_tbl$edf),
         mean_high_rmse    = meta$mean_high_rmse,
         sd_high_rmse      = meta$sd_high_rmse,
         mean_rmse         = meta$mean_rmse,
         n_folds_converged = meta$n_folds_converged,
         n_folds_total     = meta$n_folds_total
      )
   }) %>%
      mutate(label = paste0("C", candidate_rank)) %>%
      arrange(mean_high_rmse) %>%
      mutate(candidate_rank = row_number())   # re-rank post-sort
   
   cat("=== CANDIDATES RANKED BY RMSE ===\n")
   print(candidate_summary %>%
            select(candidate_rank, total_edf, mean_rmse, mean_high_rmse, sd_high_rmse,
                   n_folds_converged, n_folds_total))
   cat("\n")
   
   # ============================================================================
   # SELECTION PLOTS  (unchanged)
   # ============================================================================
   
   cat("Building selection plots...\n")
   dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)
   
   gam_theme <- theme_bw() +
      theme(
         plot.title    = element_text(size = 16, face = "bold", color = gam_colors$dark),
         plot.subtitle = element_text(size = 13,                color = gam_colors$dark),
         axis.title    = element_text(size = 14, face = "bold", color = gam_colors$dark),
         axis.text     = element_text(size = 12,                color = gam_colors$dark),
         panel.border  = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.title  = element_text(size = 12, face = "bold", color = gam_colors$dark),
         legend.text   = element_text(size = 11,                color = gam_colors$dark),
         legend.background = element_rect(fill = "white", color = NA),
         legend.key        = element_rect(fill = "white", color = NA)
      )
   
   n_folds <- length(folds)
   
   pA <- candidate_summary %>%
      mutate(
         se_high_rmse = sd_high_rmse / sqrt(n_folds)
      ) %>%
      ggplot(aes(x = total_edf, y = mean_high_rmse, label = label)) +
      geom_errorbar(
         aes(
            ymin = mean_high_rmse - se_high_rmse,
            ymax = mean_high_rmse + se_high_rmse
         ),
         width = 1.5, linewidth = 0.7, color = "grey60"
      ) +
      geom_point(size = 3.5, color = gam_colors$primary) +
      ggrepel::geom_text_repel(
         size = 4, color = gam_colors$dark, fontface = "bold",
         box.padding = 0.4, point.padding = 0.3, direction = "both"
      ) +
      scale_y_continuous(
         limits = c(0, NA),
         expand = expansion(mult = c(0.02, 0.1))
      ) +
      labs(
         title = "Accuracy vs Complexity",
         x     = "Total Expected Degrees of Freedom",
         y     = "Mean High-Salinity RMSE Across Folds (ppt)"
      ) +
      gam_theme
   
   pB <- candidate_summary %>%
      mutate(
         se_high_rmse = sd_high_rmse / sqrt(n_folds)
      ) %>%
      ggplot(aes(x = mean_high_rmse, y = se_high_rmse, label = label)) +
      geom_point(size = 3.5, color = gam_colors$secondary) +
      ggrepel::geom_text_repel(
         size = 4, color = gam_colors$dark, fontface = "bold",
         box.padding = 0.5, point.padding = 0.3, direction = "both"
      ) +
      scale_x_continuous(
         limits = c(0, NA),
         expand = expansion(mult = c(0.02, 0.08))
      ) +
      scale_y_continuous(
         limits = c(0, NA),
         expand = expansion(mult = c(0.02, 0.08))
      ) +
      labs(
         title = "Accuracy vs Consistency",
         x     = "Mean High-Salinity RMSE Across Folds (ppt)",
         y     = "SE of High-Salinity RMSE Across Folds (ppt)"
      ) +
      gam_theme
   
   clean_term_label <- function(term) {
      if (grepl(":WindDir", term)) {
         var   <- sub("^s\\(([^)]+)\\).*$", "\\1", term)
         level <- sub("^.*:WindDir", "", term)
         return(paste0(var, " (", level, ")"))
      }
      if (grepl("^ti\\(h,", term)) {
         var <- sub("^ti\\(h,\\s*([^,)]+).*$", "\\1", term)
         return(paste0("h x ", var))
      }
      if (grepl("^ti\\(", term)) {
         inner <- sub("^ti\\(([^)]+)\\)$", "\\1", term)
         vars  <- trimws(strsplit(inner, ",")[[1]])
         return(paste(vars, collapse = " x "))
      }
      sub("^s\\(([^)]+)\\)$", "\\1", term)
   }
   
   edf_all <- bind_rows(candidate_edf_tables) %>%
      filter(!is.na(edf)) %>%
      mutate(term_short = vapply(term, clean_term_label, character(1)))
   
   pC <- ggplot(edf_all,
                aes(x = factor(candidate_rank,
                               labels = paste0("C", sort(unique(candidate_rank)))),
                    y    = reorder(term_short, edf, FUN = mean),
                    fill = edf)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(edf, 1)), size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "EDF") +
      labs(title = "Per-Term Expected Degrees of Freedom",
           x     = "Candidate",
           y     = "Smooth Term") +
      gam_theme +
      theme(axis.text.y = element_text(size = 9))
   
   fold_profiles <- fold_cv_all %>%
      inner_join(top_candidates_meta %>% select(k_index, candidate_rank), by = "k_index") %>%
      filter(!is.na(high_rmse))
   
   pD <- ggplot(fold_profiles,
                aes(x     = fold,
                    y     = high_rmse,
                    color = factor(candidate_rank),
                    group = factor(candidate_rank))) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      scale_color_manual(
         values = setNames(
            colorRampPalette(c(gam_colors$secondary, gam_colors$primary,
                               gam_colors$tertiary))(n_candidates),
            as.character(1:n_candidates)
         ),
         name = "Candidate"
      ) +
      labs(title = "High-Salinity RMSE by Fold",
           x     = "CV Fold",
           y     = "High-Salinity RMSE") +
      scale_x_continuous(breaks = seq_along(folds)) +
      gam_theme +
      theme(legend.position = "right")
   
   for (p_info in list(
      list(p = pA, name = "AccuracyVsComplexity",  w = 8,  h = 6),
      list(p = pB, name = "AccuracyVsConsistency", w = 8,  h = 6),
      list(p = pC, name = "EDFHeatmap",            w = 10,
           h = max(6, n_distinct(edf_all$term_short) * 0.35 + 2)),
      list(p = pD, name = "FoldProfiles",          w = 10, h = 6)
   )) {
      ggsave(file.path(plot_output_dir, paste0(p_info$name, ".png")),
             p_info$p, width = p_info$w, height = p_info$h, dpi = 600)
      ggsave(file.path(plot_output_dir, paste0(p_info$name, ".svg")),
             p_info$p, width = p_info$w, height = p_info$h)
   }
   
   cat("Plots saved to:", plot_output_dir, "\n\n")
   
   cat("=== CANDIDATE SELECTION SUMMARY (ranked by RMSE) ===\n")
   cat("Inspect plots in:", plot_output_dir, "\n")
   cat("Also inspect n_folds_converged below -- a candidate with strong RMSE\n")
   cat("but a low convergence count may be winning on folds that didn't\n")
   cat("actually settle to a stable fit. This is a diagnostic, not a filter;\n")
   cat("no candidates are excluded automatically.\n")
   cat("Set SELECTED_CANDIDATE_RANK in Script 04 then run Phase 3.\n\n")
   
   print(candidate_summary %>%
            select(candidate_rank, total_edf, mean_rmse, mean_high_rmse, sd_high_rmse,
                   n_folds_converged, n_folds_total) %>%
            mutate(across(where(is.numeric), ~ round(., 4))))
   cat("\n")
   
   list(
      tune_grid         = tune_results,
      top_candidates    = top_candidates_meta,
      candidate_summary = candidate_summary,
      edf_tables        = candidate_edf_tables,
      fold_cv_all       = fold_cv_all,   # <-- now exposed: per-fold detail incl. converged/warning_text
      data_clean        = data_clean,
      model_cols        = model_cols,
      fit_params        = list(
         family_type    = family_type,
         link           = link,
         tweedie_p      = tweedie_p,
         method         = method,
         discrete       = discrete,
         nthreads       = nthreads,
         gam_select     = gam_select,
         basis_default  = basis_default,
         basis_horizon  = basis_horizon,
         active_k_types = active_k_types,
         interactions   = interactions
      ),
      model_type     = "gam",
      transform_info = list(family = family_type, link = link)
   )
}

# =============================================================================
# Function:       select_gam_candidate   (UNCHANGED from prior version)
# Description:    Refits the chosen candidate from saved metadata. Post-hoc
#                 cluster-robust SEs are applied separately (see
#                 get_cluster_robust_vcov), since rho was found to vary
#                 systematically with h (0.59 at h=1 to 0.90 at h=20), making
#                 a single pooled AR(1) parameter mis-specified. This function
#                 does not need parallelization (single fit) and is left as-is.
# =============================================================================

select_gam_candidate <- function(candidates_output, rank = 1) {
   
   library(mgcv)
   library(dplyr)
   
   meta_row <- candidates_output$candidate_summary %>%
      filter(candidate_rank == rank)
   
   if (nrow(meta_row) == 0) {
      stop("No candidate with rank ", rank, ". Available ranks: ",
           paste(candidates_output$candidate_summary$candidate_rank, collapse = ", "))
   }
   
   orig_meta <- candidates_output$top_candidates %>%
      filter(abs(mean_high_rmse - meta_row$mean_high_rmse) < 1e-10)
   
   if (nrow(orig_meta) == 0) {
      stop("Could not match re-ranked candidate back to original k-values. ",
           "Check candidate_summary/top_candidates alignment.")
   }
   orig_meta <- orig_meta[1, ]
   
   p          <- candidates_output$fit_params
   data_clean <- candidates_output$data_clean
   model_cols <- candidates_output$model_cols
   
   gam_family <- switch(p$family_type,
                        "gaussian" = gaussian(link = p$link),
                        "Gamma"    = Gamma(link = p$link),
                        "Tweedie"  = Tweedie(p = p$tweedie_p, link = p$link),
                        stop("Unknown family_type"))
   
   non_h_cols     <- setdiff(model_cols, c("h", "Response", "WindDir"))
   lag_vars       <- non_h_cols[grepl("LagSalinity",                     non_h_cols, ignore.case = TRUE)]
   sustained_vars <- non_h_cols[grepl("RollingDischarge|RollingAnomaly", non_h_cols, ignore.case = TRUE)]
   flushing_vars  <- non_h_cols[grepl("MaxDischarge|ExceedFlux",         non_h_cols, ignore.case = TRUE)]
   tide_vars      <- non_h_cols[grepl("TideRange|TideMean",              non_h_cols, ignore.case = TRUE)]
   wind_vars      <- non_h_cols[grepl("RollingWindAlong|RollingWindCross",non_h_cols, ignore.case = TRUE)]
   classified     <- c(lag_vars, sustained_vars, flushing_vars, tide_vars, wind_vars)
   other_vars     <- setdiff(non_h_cols, classified)
   ti_vars        <- non_h_cols
   
   k_h              <- orig_meta$k_h
   k_interaction    <- orig_meta$k_interaction
   k_sustained_flow <- orig_meta$k_sustained_flow
   k_flushing_flow  <- orig_meta$k_flushing_flow
   k_physical       <- orig_meta$k_physical
   k_wind           <- orig_meta$k_wind
   
   terms <- c()
   terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", p$basis_horizon, "')"))
   if (length(lag_vars)       > 0) terms <- c(terms, lag_vars)
   if (length(sustained_vars) > 0) terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow, ", bs='", p$basis_default, "')"))
   if (length(flushing_vars)  > 0) terms <- c(terms, paste0("s(", flushing_vars,  ", k=", k_flushing_flow,  ", bs='", p$basis_default, "')"))
   if (length(tide_vars)      > 0) terms <- c(terms, paste0("s(", tide_vars,      ", k=", k_physical,        ", bs='", p$basis_default, "')"))
   if (length(wind_vars)      > 0) terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,    ", bs='", p$basis_default, "')"))
   if (length(other_vars)     > 0) terms <- c(terms, paste0("s(", other_vars,      ", k=", k_physical,        ", bs='", p$basis_default, "')"))
   for (var in ti_vars) {
      terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                               "), bs=c('", p$basis_horizon, "', '", p$basis_default, "'))"))
   }
   
   if (length(p$interactions) > 0) {
      for (int in p$interactions) {
         int_vars <- if (is.list(int)) int$vars else int
         terms <- c(terms, paste0(
            "ti(", int_vars[1], ", ", int_vars[2],
            ", k=c(", k_interaction, ", ", k_interaction, ")",
            ", bs=c('", p$basis_default, "', '", p$basis_default, "'))"
         ))
      }
   }
   
   final_formula <- as.formula(paste("Response ~", paste(terms, collapse = " + ")))
   
   cat("=== SELECTED CANDIDATE", rank, "===\n")
   if (length(p$interactions) > 0) {
      cat("Includes", length(p$interactions), "pooled variable-variable interaction(s).\n")
   }
   cat("Formula:\n"); print(final_formula); cat("\n")
   
   final_gam <- tryCatch({
      mgcv::bam(
         formula  = final_formula,
         data     = data_clean %>% select(all_of(model_cols)),
         family   = gam_family,
         method   = p$method,
         discrete = p$discrete,
         nthreads = as.integer(p$nthreads),
         control  = list(nthreads = as.integer(p$nthreads)),
         select   = p$gam_select
      )
   }, error = function(e) { stop("Final refit failed: ", e$message) })
   
   cat("\n=== MODEL SUMMARY (naive SEs — see get_cluster_robust_vcov for corrected inference) ===\n")
   print(summary(final_gam))
   cat("\n")
   
   s_table     <- summary(final_gam)$s.table
   smooth_info <- tibble(
      term    = rownames(s_table),
      edf     = s_table[, "edf"],
      ref_df  = s_table[, "Ref.df"],
      F_stat  = s_table[, "F"],
      p_value = s_table[, "p-value"]
   ) %>% arrange(desc(edf))
   
   sig_terms <- smooth_info %>% filter(p_value < 0.05)
   cat("Significant terms (p < 0.05, NAIVE SEs):", nrow(sig_terms), "/", nrow(smooth_info), "\n")
   cat("NOTE: these significance flags use working-independence SEs and have not\n")
   cat("been corrected for residual autocorrelation. Apply get_cluster_robust_vcov()\n")
   cat("before treating any of this as final inference.\n\n")
   
   gam_workflow <- structure(
      list(fit = list(
         fit     = final_gam,
         formula = final_formula,
         family  = p$family_type
      )),
      class = c("workflow", "list")
   )
   
   list(
      tune_results   = candidates_output$tune_grid,
      tune_grid      = candidates_output$tune_grid,
      best_params    = bind_cols(
         orig_meta %>% select(all_of(p$active_k_types), k_h, k_physical),
         tibble(family = p$family_type, link = p$link)
      ),
      final_fit      = gam_workflow,
      gam_object     = final_gam,
      formula        = final_formula,
      smooth_info    = smooth_info,
      selected_vars  = sig_terms$term,
      model_type     = "gam",
      transform_info = candidates_output$transform_info
   )
}