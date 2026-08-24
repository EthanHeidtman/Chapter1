# =============================================================================
# Function:       fit_gam  (parallelized version — flattened k-combo x fold tasks)
#
# CHANGES FROM PRIOR VERSION:
#   - Parallelization grain changed from "one task per k-combo" (with all folds
#     run serially inside the task) to "one task per (k-combo, fold) pair".
#     This was necessary because task cost varies substantially across k values
#     (larger k_interaction = more expensive/harder-to-converge bam() fits,
#     confirmed via total_edf and "reparameterization unstable" warnings), and
#     with small-to-moderate k-grids, coarse per-k-combo tasking left most
#     workers idle while 1-2 workers finished the expensive tail. Flattening
#     to per-fold tasks gives furrr's scheduler enough granularity to spread
#     uneven cost across all workers.
#   - furrr_options(scheduling = 4): requests ~4x as many chunks as workers so
#     idle workers can pick up more tasks instead of waiting on stragglers.
#     Only effective now that task count is large (k-combos x folds instead of
#     just k-combos).
#   - Fold train/test subsets are precomputed ONCE before the parallel loop
#     (precomputed_folds), rather than re-filtered inside every task. Under
#     the old per-k-combo tasking this filter ran length(folds) times total;
#     under flattened tasking it would otherwise run nrow(k_grid) x
#     length(folds) times, so precomputing is now more important, not less.
#   - fit_fold() signature changed: takes train_data/test_data directly
#     instead of train_idx/test_idx + doing its own filter/select.
#   - Aggregation (tune_results, candidate selection, EDF refit, plots) is
#     UNCHANGED -- all operate on fold_cv_all exactly as before, just now
#     assembled from flat per-fold tasks instead of per-k-combo bundles.
# =============================================================================

fit_gam <- function(data,
                    response = 'Salinity_h',
                    predictors = NULL,
                    folds = NULL,
                    high_salinity_threshold = 0.16,
                    
                    family_type = "gaussian",
                    link = NULL,
                    tweedie_p = 1.5,
                    
                    # k values to test
                    k_h_fixed              = 4,
                    k_physical_fixed       = 10,
                    k_interaction_range    = c(6, 14),
                    k_sustained_flow_range = c(4, 10),
                    k_flushing_flow_range  = c(6, 12),
                    k_wind_range           = c(5, 14),
                    
                    interactions = list(),
                    wind_ti_by = FALSE,
                    
                    basis_default = 'tp',
                    basis_horizon = 'cr',
                    
                    method     = 'fREML',
                    discrete   = TRUE,
                    nthreads   = 4,
                    gam_select = TRUE,
                    
                    gam_levels       = 3,
                    n_top_candidates = 10,
                    plot_output_dir  = 'Outputs/Plots/UnifiedGAM/GAMSelection',
                    
                    # Parallel & Timeout arguments
                    timeout_sec   = 20,   # Max seconds allowed per fold task
                    n_workers     = NULL,
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
   library(R.utils)
   
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
      warning("Identity link with Gamma can produce negative predictions. Consider link = 'log'.\n")
   }
   
   gam_family <- switch(family_type,
                        "gaussian" = gaussian(link = link),
                        "Gamma"    = Gamma(link = link),
                        "Tweedie"  = Tweedie(p = tweedie_p, link = link),
                        stop("Unknown family_type"))
   
   h_var            <- 'h'
   non_h_predictors <- setdiff(predictors, h_var)
   
   lag_vars       <- non_h_predictors[grepl("LagSalinity",                     non_h_predictors, ignore.case = TRUE)]
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
   
   if (has_interactions) {
      for (int in interactions) {
         int_vars <- if (is.list(int)) int$vars else int
         if (length(int_vars) != 2) {
            stop("Each interaction must specify exactly 2 variables. Got: ", paste(int_vars, collapse = ", "))
         }
         if (!all(int_vars %in% predictors)) {
            stop("Interaction pair (", paste(int_vars, collapse = ", "), ") includes a variable not in `predictors`.")
         }
      }
   }
   
   data_subset <- data %>%
      mutate(.row_id = row_number()) %>%
      select(.row_id, DateTime, all_of(response), all_of(predictors))
   
   wind_dir_convention <- NULL
   if (has_wind) {
      wind_var <- wind_vars[1]
      if (grepl("Along", wind_var)) {
         wind_dir_convention <- list(
            var_name       = wind_var,
            positive_level = "UpEstuary",
            negative_level = "DownEstuary",
            levels         = c("DownEstuary", "UpEstuary")
         )
         data_subset <- data_subset %>%
            mutate(WindDir = factor(
               ifelse(.data[[wind_var]] >= 0, "UpEstuary", "DownEstuary"),
               levels = wind_dir_convention$levels
            ))
      } else {
         wind_dir_convention <- list(
            var_name       = wind_var,
            positive_level = "RightBank",
            negative_level = "LeftBank",
            levels         = c("LeftBank", "RightBank")
         )
         data_subset <- data_subset %>%
            mutate(WindDir = factor(
               ifelse(.data[[wind_var]] >= 0, "RightBank", "LeftBank"),
               levels = wind_dir_convention$levels
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
   cat("Response range: [", round(min(data_clean$Response), 4), ", ", round(max(data_clean$Response), 4), "]\n\n")
   
   if (family_type == "Gamma" && any(data_clean$Response <= 0)) {
      data_clean$Response          <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   if (family_type == "Tweedie" && any(data_clean$Response < 0)) {
      data_clean$Response          <- pmax(data_clean$Response, 0.001)
      data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
   }
   
   model_cols <- setdiff(names(data_clean), c(".row_id", "DateTime", response, "Response_original"))
   
   # Setup Tuning Grid
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
   
   active_k_types <- names(k_sequences)
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response:", response, "\n")
   cat("Tuning", nrow(k_grid), "k combinations across:", paste(active_k_types, collapse = ", "), "\n\n")
   
   # Build Formula Helper
   build_gam_formula <- function(k_h, k_interaction, k_sustained_flow, k_flushing_flow, k_physical, k_wind) {
      terms <- c()
      terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", basis_horizon, "')"))
      if (has_lag)       terms <- c(terms, lag_vars)
      if (has_sustained) terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow, ", bs='", basis_default, "')"))
      if (has_flushing)  terms <- c(terms, paste0("s(", flushing_vars,  ", k=", k_flushing_flow,  ", bs='", basis_default, "')"))
      if (has_tide)      terms <- c(terms, paste0("s(", tide_vars,      ", k=", k_physical,        ", bs='", basis_default, "')"))
      if (has_wind)      terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,    ", bs='", basis_default, "')"))
      if (has_other)     terms <- c(terms, paste0("s(", other_vars,     ", k=", k_physical,        ", bs='", basis_default, "')"))
      for (var in ti_vars) {
         if (wind_ti_by && var %in% wind_vars) {
            terms <- c(terms, paste0("ti(h, ", var, ", by=WindDir, k=c(", k_h, ", ", k_interaction,
                                     "), bs=c('", basis_horizon, "', '", basis_default, "'))"))
         } else {
            terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                                     "), bs=c('", basis_horizon, "', '", basis_default, "'))"))
         }
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
   
   cat("Precomputing fold train/test subsets...\n\n")
   precomputed_folds <- map(seq_along(folds), function(j) {
      list(
         train_data = data_clean %>% filter(.row_id %in% folds[[j]]$train) %>% select(all_of(model_cols)),
         test_data  = data_clean %>% filter(.row_id %in% folds[[j]]$test)
      )
   })
   
   # ============================================================================
   # UPDATED: FOLD-FITTING UNIT (With timeout protection & capped maxit)
   # ============================================================================
   
   fit_fold <- function(formula, train_data, test_data, fold_num, timeout_sec = 20) {
      
      warning_msgs <- character(0)
      
      gam_fit <- tryCatch({
         withCallingHandlers({
            # Enforce hard wall-clock timeout on low-level bam() optimization
            R.utils::withTimeout({
               mgcv::bam(
                  formula  = formula,
                  data     = train_data,
                  family   = gam_family,
                  method   = method,
                  discrete = TRUE,
                  nthreads = 1L,
                  control  = mgcv::gam.control(
                     maxit   = 50,    # Cap outer fREML iterations to stop infinite loops
                     epsilon = 1e-6,
                     trace   = FALSE,
                     nthreads = 1L
                  )
               )
            }, timeout = timeout_sec, onTimeout = "error")
         }, warning = function(w) {
            warning_msgs[[length(warning_msgs) + 1]] <<- conditionMessage(w)
            invokeRestart("muffleWarning")
         })
      }, error = function(e) {
         cat(sprintf("    [Fold %d failed/timed out: %s]\n", fold_num, e$message))
         return(NULL)
      })
      
      converged <- !any(grepl("did not converge", warning_msgs, fixed = TRUE)) && !is.null(gam_fit)
      warning_text <- if (length(warning_msgs) > 0) paste(unique(warning_msgs), collapse = " | ") else NA_character_
      
      fail <- list(rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
                   high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
                   n_high_sal = 0L, failed = TRUE,
                   converged = FALSE, warning_text = warning_text)
      
      if (is.null(gam_fit)) return(fail)
      
      preds <- tryCatch({
         predict(gam_fit, newdata = test_data %>% select(all_of(model_cols)), type = "response")
      }, error = function(e) {
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
   
   # Task List Setup
   task_grid <- tidyr::crossing(
      k_index  = k_grid$k_index,
      fold_num = seq_along(folds)
   ) %>%
      left_join(k_grid, by = "k_index")
   
   task_list <- split(task_grid, seq_len(nrow(task_grid)))
   
   fit_one_task <- function(task_row) {
      formula <- build_gam_formula(task_row$k_h, task_row$k_interaction,
                                   task_row$k_sustained_flow, task_row$k_flushing_flow,
                                   task_row$k_physical, task_row$k_wind)
      j   <- task_row$fold_num
      fd  <- precomputed_folds[[j]]
      
      log_file <- "task_log.txt"
      t0 <- Sys.time()
      # cat(sprintf("START k_index=%d fold=%d k_interaction=%d time=%s\n",
      #             task_row$k_index, j, task_row$k_interaction, t0),
      #     file = log_file, append = TRUE)
      
      # Pass timeout_sec down to fit_fold
      res <- fit_fold(formula, fd$train_data, fd$test_data, j, timeout_sec = timeout_sec)
      
      t1 <- Sys.time()
      # cat(sprintf("END   k_index=%d fold=%d k_interaction=%d time=%s elapsed=%.1fs\n",
      #             task_row$k_index, j, task_row$k_interaction, t1,
      #             as.numeric(difftime(t1, t0, units = "secs"))),
      #     file = log_file, append = TRUE)
      
      tibble(fold = j, !!!res, k_index = task_row$k_index)
   }
   
   # ============================================================================
   # UPDATED: DYNAMIC PARALLEL SCHEDULING (scheduling = 1)
   # ============================================================================
   
   cat("Running CV across", nrow(k_grid), "k combinations x", length(folds), "folds =",
       nrow(task_grid), "tasks (plan:", class(future::plan())[1], ")...\n\n")
   
   run_kcombo_grid <- function() {
      if (isTRUE(show_progress)) {
         progressr::with_progress({
            p <- progressr::progressor(along = task_list)
            furrr::future_map(task_list, function(task_row) {
               res <- fit_one_task(task_row)
               p(sprintf("k_index=%d fold=%d", task_row$k_index, task_row$fold_num))
               res
            }, .options = furrr::furrr_options(seed = TRUE, scheduling = 1)) # Dynamic dispatch
         })
      } else {
         furrr::future_map(task_list, fit_one_task,
                           .options = furrr::furrr_options(seed = TRUE, scheduling = 1)) # Dynamic dispatch
      }
   }
   
   all_fold_results_list <- run_kcombo_grid()
   fold_cv_all <- bind_rows(all_fold_results_list)
   
   cat(sprintf("[CHECKPOINT] CV loop done: %s\n", Sys.time()))
   
   # Aggregation
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
         n_folds_converged   = sum(converged, na.rm = TRUE),
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
   
   # EDF Refit Loop
   cat("Refitting top", n_candidates, "candidates to extract EDF...\n\n")
   full_train_data <- data_clean %>% select(all_of(model_cols))
   
   candidate_edf_tables <- {
      p <- progressr::progressor(along = 1:nrow(top_candidates_meta))
      furrr::future_map(1:nrow(top_candidates_meta), function(i) {
         meta    <- top_candidates_meta[i, ]
         formula <- build_gam_formula(meta$k_h, meta$k_interaction,
                                      meta$k_sustained_flow, meta$k_flushing_flow,
                                      meta$k_physical, meta$k_wind)
         
         gam_fit <- tryCatch({
            mgcv::bam(
               formula  = formula,
               data     = full_train_data,
               family   = gam_family,
               method   = method,
               discrete = discrete,
               nthreads = 1L,
               control  = mgcv::gam.control(maxit = 50, trace = FALSE, nthreads = 1L),
               select   = gam_select
            )
         }, error = function(e) NULL)
         
         p(sprintf("candidate %d", meta$candidate_rank))
         
         if (is.null(gam_fit)) return(NULL)
         
         s_table <- summary(gam_fit)$s.table
         edf_tbl <- tibble(
            candidate_rank = meta$candidate_rank,
            term           = rownames(s_table),
            edf            = s_table[, "edf"],
            p_value        = s_table[, "p-value"]
         ) %>%
            mutate(term_group = case_when(
               grepl("^ti\\(h,", term) & grepl("LagSalinity", term) ~ "LagSalinity",
               grepl("^ti\\(h,", term) & grepl("RollingDischarge|RollingAnomaly", term) ~ "SustainedDischarge",
               grepl("^ti\\(h,", term) & grepl("MaxDischarge|ExceedFlux", term) ~ "FlushingDischarge",
               grepl("^ti\\(h,", term) & grepl("TideRange|TideMean", term) ~ "Tide",
               grepl("^ti\\(h,", term) & grepl("RollingWindAlong|RollingWindCross", term) ~ "Wind",
               grepl("^ti\\(", term) & !grepl("^ti\\(h,", term) ~ "VarInteraction",
               grepl("LagSalinity", term) ~ "LagSalinity",
               grepl("RollingDischarge|RollingAnomaly", term) ~ "SustainedDischarge",
               grepl("MaxDischarge|ExceedFlux", term) ~ "FlushingDischarge",
               grepl("TideRange|TideMean", term) ~ "Tide",
               grepl("RollingWindAlong|RollingWindCross", term) ~ "Wind",
               grepl("^s\\(h\\)", term) ~ "Horizon",
               TRUE ~ "Other"
            ))
         rm(gam_fit); gc(verbose = FALSE)
         edf_tbl
      }, .options = furrr::furrr_options(seed = TRUE, scheduling = 1))
   }
   
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
      mutate(candidate_rank = row_number())
   
   # Plotting Routine
   dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)
   gam_theme <- theme_bw() +
      theme(
         plot.title    = element_text(size = 16, face = "bold", color = gam_colors$dark),
         plot.subtitle = element_text(size = 13,                color = gam_colors$dark),
         axis.title    = element_text(size = 14, face = "bold", color = gam_colors$dark),
         axis.text     = element_text(size = 12,                color = gam_colors$dark),
         panel.border  = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.title  = element_text(size = 12, face = "bold", color = gam_colors$dark)
      )
   
   n_folds <- length(folds)
   
   pA <- candidate_summary %>%
      mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
      ggplot(aes(x = total_edf, y = mean_high_rmse, label = label)) +
      geom_errorbar(aes(ymin = mean_high_rmse - se_high_rmse, ymax = mean_high_rmse + se_high_rmse), width = 1.5, color = "grey60") +
      geom_point(size = 3.5, color = gam_colors$primary) +
      ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
      labs(title = "Accuracy vs Complexity", x = "Total EDF", y = "Mean High-Salinity RMSE (ppt)") +
      gam_theme
   
   pB <- candidate_summary %>%
      mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
      ggplot(aes(x = mean_high_rmse, y = se_high_rmse, color = total_edf, label = label)) +
      geom_point(size = 3.5) +
      ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
      scale_color_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "Total EDF") +
      labs(title = "Accuracy vs Consistency", x = "Mean High-Salinity RMSE (ppt)", y = "SE of High-Salinity RMSE") +
      gam_theme
   
   clean_term_label <- function(term) {
      
      # Wind interactions
      if (grepl("^ti\\(h,RollingWindCross", term)) {
         days <- sub(".*RollingWindCross([0-9]+).*", "\\1", term)
         
         if (grepl("WindDirLeftBank", term))
            return(paste0("h x ", days, " Day Westerly Wind"))
         
         if (grepl("WindDirRightBank", term))
            return(paste0("h x ", days, " Day Easterly Wind"))
      }
      
      # Other h interactions
      if (grepl("^ti\\(h,", term))
         return(paste0("h x ", sub("^ti\\(h,([^,)]+).*$", "\\1", term)))
      
      # Ordinary smooths
      if (grepl("^s\\(RollingWindCross", term)) {
         days <- sub(".*RollingWindCross([0-9]+).*", "\\1", term)
         
         if (grepl("WindDirLeftBank", term))
            return(paste0(days, " Day Westerly Wind"))
         
         if (grepl("WindDirRightBank", term))
            return(paste0(days, " Day Easterly Wind"))
      }
      
      sub("^s\\(([^)]+)\\)$", "\\1", term)
   }
   
   edf_all <- bind_rows(candidate_edf_tables) %>%
      filter(!is.na(edf)) %>%
      mutate(term_short = vapply(term, clean_term_label, character(1)))
   
   pC <- ggplot(edf_all, aes(x = factor(candidate_rank, labels = paste0("C", sort(unique(candidate_rank)))),
                             y = reorder(term_short, edf, FUN = mean), fill = edf)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(edf, 1)), size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient(low = gam_colors$secondary, high = gam_colors$primary) +
      labs(title = "Per-Term EDF", x = "Candidate", y = "Smooth Term") +
      gam_theme
   
   fold_profiles <- fold_cv_all %>%
      inner_join(top_candidates_meta %>% select(k_index, candidate_rank), by = "k_index") %>%
      filter(!is.na(high_rmse))
   
   candidate_summary_top10 <- candidate_summary %>%
      slice_head(n = 10)
   
   edf_all_top10 <- edf_all %>%
      filter(candidate_rank %in% candidate_summary_top10$candidate_rank)
   
   fold_profiles_top10 <- fold_profiles %>%
      filter(candidate_rank %in% candidate_summary_top10$candidate_rank)
   
   pD <- ggplot(fold_profiles, aes(x = fold, y = high_rmse, color = factor(candidate_rank), group = factor(candidate_rank))) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      labs(title = "High-Salinity RMSE by Fold", x = "CV Fold", y = "High-Salinity RMSE") +
      scale_x_continuous(breaks = seq_along(folds)) +
      gam_theme
   
   # =============================================================================
   # TOP-10 PLOTS
   # =============================================================================
   
   pA_top10 <- candidate_summary_top10 %>%
      mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
      ggplot(aes(x = total_edf, y = mean_high_rmse, color = total_edf, label = label)) +
      geom_errorbar(aes(ymin = mean_high_rmse - se_high_rmse, ymax = mean_high_rmse + se_high_rmse),
                    width = 1.5, color = "grey60") +
      geom_point(size = 3.5) +
      ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
      scale_color_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "Total EDF") +
      labs(title = "Accuracy vs Complexity (Top 10)",
           x = "Total EDF",
           y = "Mean High-Salinity RMSE (ppt)") +
      gam_theme
   
   
   pB_top10 <- candidate_summary_top10 %>%
      mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
      ggplot(aes(x = mean_high_rmse, y = se_high_rmse, color = total_edf, label = label)) +
      geom_point(size = 3.5) +
      ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
      scale_color_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "Total EDF") +
      labs(title = "Accuracy vs Consistency (Top 10)",
           x = "Mean High-Salinity RMSE (ppt)",
           y = "SE of High-Salinity RMSE") +
      gam_theme
   
   
   pC_top10 <- ggplot(
      edf_all_top10,
      aes(x = factor(candidate_rank, labels = paste0("C", sort(unique(candidate_rank)))),
          y = reorder(term_short, edf, FUN = mean),
          fill = edf)
   ) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(edf, 1)), size = 3, color = "white", fontface = "bold") +
      scale_fill_gradient(low = gam_colors$secondary, high = gam_colors$primary) +
      labs(title = "Per-Term EDF (Top 10)", x = "Candidate", y = "Smooth Term") +
      gam_theme
   
   
   pD_top10 <- ggplot(
      fold_profiles_top10,
      aes(x = fold, y = high_rmse, color = factor(candidate_rank), group = factor(candidate_rank))
   ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      labs(title = "High-Salinity RMSE by Fold (Top 10)",
           x = "CV Fold",
           y = "High-Salinity RMSE") +
      scale_x_continuous(breaks = seq_along(folds)) +
      gam_theme
   
   for (p_info in list(
      list(p = pA,       name = "AccuracyVsComplexity",       w = 8,  h = 6),
      list(p = pB,       name = "AccuracyVsConsistency",      w = 8,  h = 6),
      list(p = pC,       name = "EDFHeatmap",                 w = 10, h = max(6, n_distinct(edf_all$term_short) * 0.35 + 2)),
      list(p = pD,       name = "FoldProfiles",               w = 10, h = 6),
      list(p = pA_top10, name = "AccuracyVsComplexity_Top10", w = 8,  h = 6),
      list(p = pB_top10, name = "AccuracyVsConsistency_Top10",w = 8,  h = 6),
      list(p = pC_top10, name = "EDFHeatmap_Top10",           w = 10, h = max(6, n_distinct(edf_all_top10$term_short) * 0.35 + 2)),
      list(p = pD_top10, name = "FoldProfiles_Top10",         w = 10, h = 6)
   )) {
      ggsave(file.path(plot_output_dir, paste0(p_info$name, ".png")),
             p_info$p, width = p_info$w, height = p_info$h, dpi = 600)
      
      ggsave(file.path(plot_output_dir, paste0(p_info$name, ".svg")),
             p_info$p, width = p_info$w, height = p_info$h)
   }
   
   list(
      tune_grid           = tune_results,
      top_candidates      = top_candidates_meta,
      candidate_summary   = candidate_summary,
      edf_tables          = candidate_edf_tables,
      fold_cv_all         = fold_cv_all,
      data_clean          = data_clean,
      model_cols          = model_cols,
      wind_dir_convention = wind_dir_convention,
      fit_params          = list(
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
         interactions   = interactions,
         wind_ti_by     = wind_ti_by
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
      if (isTRUE(p$wind_ti_by) && var %in% wind_vars) {
         terms <- c(terms, paste0("ti(h, ", var, ", by=WindDir, k=c(", k_h, ", ", k_interaction,
                                  "), bs=c('", p$basis_horizon, "', '", p$basis_default, "'))"))
      } else {
         terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                                  "), bs=c('", p$basis_horizon, "', '", p$basis_default, "'))"))
      }
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
      tune_results        = candidates_output$tune_grid,
      tune_grid           = candidates_output$tune_grid,
      best_params         = bind_cols(
         orig_meta %>% select(all_of(p$active_k_types), k_h, k_physical),
         tibble(family = p$family_type, link = p$link)
      ),
      final_fit           = gam_workflow,
      gam_object          = final_gam,
      formula             = final_formula,
      smooth_info         = smooth_info,
      selected_vars       = sig_terms$term,
      model_type          = "gam",
      transform_info      = candidates_output$transform_info,
      wind_dir_convention = candidates_output$wind_dir_convention
   )
}