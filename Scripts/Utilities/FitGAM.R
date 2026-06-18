
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
#
#                 K is tuned via expanding-window CV across grouped k-ranges.
#                 The top N candidates by mean high-salinity RMSE are refit on
#                 the full training data; selection plots are saved so the user
#                 can inspect and manually choose a candidate in Script 04.
#
#                 fit_gam returns METADATA ONLY — no bam objects — to avoid
#                 qs serialization failures on mgcv model environments.
#                 select_gam_candidate() refits the chosen candidate from the
#                 saved k-values and data_clean carried in the metadata.
# =============================================================================

fit_gam <- function(data,
                    response = 'Salinity_h',
                    predictors = NULL,
                    folds = NULL,
                    high_salinity_threshold = 0.15,
                    
                    family_type = "gaussian",
                    link = NULL,
                    tweedie_p = 1.5,
                    
                    k_h_range              = c(4, 12),
                    k_interaction_range    = c(4, 12),
                    k_sustained_flow_range = c(4, 12),
                    k_flushing_flow_range  = c(4, 12),
                    k_physical_range       = c(4, 10),
                    k_wind_range           = c(4, 10),
                    
                    interactions = list(),
                    
                    basis_default = 'tp',
                    basis_horizon = 'cr',
                    
                    method     = 'fREML',
                    discrete   = TRUE,
                    nthreads   = 4,
                    gam_select = TRUE,
                    
                    gam_levels       = 3,
                    n_top_candidates = 10,
                    plot_output_dir  = 'Outputs/Plots/UnifiedGAM/GAMSelection') {
   
   library(mgcv)
   library(dplyr)
   library(purrr)
   library(ggplot2)
   library(tidyr)
   
   if (!('h' %in% predictors)) {
      stop("predictors must include 'h' for the unified multi-horizon GAM.")
   }
   
   # ============================================================================
   # COLOR PALETTE
   # ============================================================================
   
   gam_colors <- list(
      primary   = "#f58220",
      secondary = "#009bba",
      tertiary  = "#fdb515",
      dark      = "#002030"
   )
   
   # ============================================================================
   # FAMILY
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
   
   # ============================================================================
   # PREPARE DATA
   # ============================================================================
   
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
   # K GRID
   # ============================================================================
   
   k_sequences <- list()
   k_sequences$k_h           <- unique(round(seq(k_h_range[1],              k_h_range[2],              length.out = gam_levels)))
   k_sequences$k_interaction  <- unique(round(seq(k_interaction_range[1],   k_interaction_range[2],    length.out = gam_levels)))
   if (has_sustained) k_sequences$k_sustained_flow <- unique(round(seq(k_sustained_flow_range[1], k_sustained_flow_range[2], length.out = gam_levels)))
   if (has_flushing)  k_sequences$k_flushing_flow  <- unique(round(seq(k_flushing_flow_range[1],  k_flushing_flow_range[2],  length.out = gam_levels)))
   if (has_tide || has_other) k_sequences$k_physical <- unique(round(seq(k_physical_range[1], k_physical_range[2], length.out = gam_levels)))
   if (has_wind)      k_sequences$k_wind            <- unique(round(seq(k_wind_range[1],            k_wind_range[2],            length.out = gam_levels)))
   
   k_grid <- expand.grid(k_sequences) %>% distinct()
   
   if (!has_sustained)          k_grid$k_sustained_flow <- k_sustained_flow_range[1]
   if (!has_flushing)           k_grid$k_flushing_flow  <- k_flushing_flow_range[1]
   if (!has_tide && !has_other) k_grid$k_physical       <- k_physical_range[1]
   if (!has_wind)               k_grid$k_wind           <- k_wind_range[1]
   
   k_grid <- k_grid %>%
      select(k_h, k_interaction, k_sustained_flow, k_flushing_flow, k_physical, k_wind)
   
   active_k_types <- names(k_sequences)
   
   # ============================================================================
   # SETUP SUMMARY
   # ============================================================================
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Response:", response, "\n")
   cat("Family:", family_type, "with", link, "link\n\n")
   cat("Term groups:\n")
   if (has_lag)       cat("  LagSalinity:    ", paste(lag_vars,       collapse = ", "), "(linear)\n")
   if (has_sustained) cat("  Sustained flow: ", paste(sustained_vars, collapse = ", "), "\n")
   if (has_flushing)  cat("  Flushing flow:  ", paste(flushing_vars,  collapse = ", "), "\n")
   if (has_tide)      cat("  Tide:           ", paste(tide_vars,      collapse = ", "), "\n")
   if (has_wind)      cat("  Wind:           ", paste(wind_vars,      collapse = ", "), "(by = WindDir)\n")
   if (has_other)     cat("  Other:          ", paste(other_vars,     collapse = ", "), "\n")
   cat("\nTuning", nrow(k_grid), "k combinations across:", paste(active_k_types, collapse = ", "), "\n\n")
   
   # ============================================================================
   # BUILD FORMULA
   # ============================================================================
   
   build_gam_formula <- function(k_h, k_interaction, k_sustained_flow,
                                 k_flushing_flow, k_physical, k_wind) {
      terms <- c()
      terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", basis_horizon, "')"))
      if (has_lag)       terms <- c(terms, lag_vars)
      if (has_sustained) terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow, ", bs='", basis_default, "')"))
      if (has_flushing)  terms <- c(terms, paste0("s(", flushing_vars,  ", k=", k_flushing_flow,  ", bs='", basis_default, "')"))
      if (has_tide)      terms <- c(terms, paste0("s(", tide_vars,      ", k=", k_physical,       ", bs='", basis_default, "')"))
      if (has_wind)      terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,    ", bs='", basis_default, "')"))
      if (has_other)     terms <- c(terms, paste0("s(", other_vars,     ", k=", k_physical,       ", bs='", basis_default, "')"))
      for (var in ti_vars) {
         terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                                  "), bs=c('", basis_horizon, "', '", basis_default, "'))"))
      }
      if (has_interactions) {
         for (int in interactions) {
            if (all(int$vars %in% predictors)) {
               terms <- c(terms, paste0("ti(", paste(int$vars, collapse = ", "), ", k=", k_interaction, ")"))
            }
         }
      }
      as.formula(paste("Response ~", paste(terms, collapse = " + ")))
   }
   
   # ============================================================================
   # FIT ONE FOLD — returns metrics only, no model object
   # ============================================================================
   
   fit_fold <- function(formula, train_idx, test_idx, fold_num) {
      
      train_data <- data_clean %>% filter(.row_id %in% train_idx) %>% select(all_of(model_cols))
      test_data  <- data_clean %>% filter(.row_id %in% test_idx)
      
      gam_fit <- tryCatch({
         suppressWarnings(do.call(bam, list(
            formula  = formula,
            data     = train_data,
            family   = gam_family,
            method   = method,
            discrete = discrete,
            nthreads = nthreads,
            select   = gam_select
         )))
      }, error = function(e) {
         cat("    [Fold", fold_num, "error:", e$message, "]\n")
         return(NULL)
      })
      
      fail <- list(rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
                   high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
                   n_high_sal = 0L, failed = TRUE)
      
      if (is.null(gam_fit)) return(fail)
      
      preds <- tryCatch({
         predict(gam_fit, newdata = test_data %>% select(all_of(model_cols)), type = "response")
      }, error = function(e) {
         cat("    [Fold", fold_num, "prediction error:", e$message, "]\n")
         return(NULL)
      })
      
      if (is.null(preds) || any(!is.finite(preds))) return(fail)
      
      obs      <- test_data$Response_original
      high_idx <- obs > high_salinity_threshold
      n_high   <- sum(high_idx)
      
      high_rmse <- if (n_high > 1) sqrt(mean((obs[high_idx] - preds[high_idx])^2)) else NA_real_
      high_rsq  <- if (n_high > 1) cor(obs[high_idx], preds[high_idx])^2           else NA_real_
      high_mae  <- if (n_high > 1) mean(abs(obs[high_idx] - preds[high_idx]))       else NA_real_
      
      list(
         rmse       = sqrt(mean((obs - preds)^2)),
         rsq        = cor(obs, preds)^2,
         mae        = mean(abs(obs - preds)),
         high_rmse  = high_rmse,
         high_rsq   = high_rsq,
         high_mae   = high_mae,
         n_high_sal = n_high,
         failed     = FALSE
      )
   }
   
   # ============================================================================
   # CROSS-VALIDATION
   # ============================================================================
   
   cat("Running CV across", nrow(k_grid), "k combinations...\n")
   
   all_fold_results <- vector("list", nrow(k_grid))
   
   tune_results <- map_dfr(1:nrow(k_grid), function(i) {
      
      k_vals  <- k_grid[i, ]
      formula <- build_gam_formula(k_vals$k_h, k_vals$k_interaction,
                                   k_vals$k_sustained_flow, k_vals$k_flushing_flow,
                                   k_vals$k_physical, k_vals$k_wind)
      
      cat("  ", paste(sapply(active_k_types, function(k) paste0(k, "=", k_vals[[k]])), collapse = ", "))
      
      fold_results <- map_dfr(seq_along(folds), function(j) {
         res <- fit_fold(formula, folds[[j]]$train, folds[[j]]$test, j)
         tibble(fold = j, !!!res)
      })
      
      all_fold_results[[i]] <<- fold_results %>% mutate(k_index = i)
      
      summ <- fold_results %>%
         summarize(
            mean_rmse      = mean(rmse,      na.rm = TRUE),
            mean_rsq       = mean(rsq,       na.rm = TRUE),
            mean_mae       = mean(mae,       na.rm = TRUE),
            sd_rmse        = sd(rmse,        na.rm = TRUE),
            mean_high_rmse = mean(high_rmse, na.rm = TRUE),
            mean_high_rsq  = mean(high_rsq,  na.rm = TRUE),
            mean_high_mae  = mean(high_mae,  na.rm = TRUE),
            sd_high_rmse   = sd(high_rmse,   na.rm = TRUE),
            total_high_sal = sum(n_high_sal, na.rm = TRUE),
            n_failed       = sum(failed)
         )
      
      cat(" -> RMSE:", round(summ$mean_rmse, 4),
          "| High-Sal RMSE:", round(summ$mean_high_rmse, 4),
          "| SD:", round(summ$sd_high_rmse, 4))
      if (summ$n_failed > 0) cat(" [", summ$n_failed, "failed]")
      cat("\n")
      
      bind_cols(k_vals %>% mutate(k_index = i), summ)
   })
   
   fold_cv_all <- bind_rows(all_fold_results)
   
   cat("\n=== CV RESULTS ===\n")
   print(tune_results %>%
            arrange(mean_high_rmse) %>%
            select(all_of(c(active_k_types, "mean_rmse", "mean_high_rmse", "sd_high_rmse", "n_failed"))))
   cat("\n")
   
   # ============================================================================
   # FILTER TOP N CANDIDATES
   # ============================================================================
   
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
                   mean_rmse, mean_high_rmse, sd_high_rmse))
   cat("\n")
   
   # ============================================================================
   # REFIT TOP CANDIDATES ON FULL DATA — extract EDF then discard model objects
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
            grepl("LagSalinity",                     term) ~ "LagSalinity",
            grepl("RollingDischarge|RollingAnomaly", term) ~ "SustainedDischarge",
            grepl("MaxDischarge|ExceedFlux",         term) ~ "FlushingDischarge",
            grepl("TideRange|TideMean",              term) ~ "Tide",
            grepl("RollingWindAlong|RollingWindCross",term) ~ "Wind",
            grepl("^s\\(h\\)|^ti\\(h",              term) ~ "Horizon",
            TRUE                                           ~ "Other"
         ))
      
      # Discard gam_fit immediately — only keep the EDF table
      rm(gam_fit)
      gc(verbose = FALSE)
      
      edf_tbl
   })
   
   cat("\n")
   
   # Build per-candidate summary with total EDF
   candidate_summary <- map_dfr(seq_len(nrow(top_candidates_meta)), function(i) {
      meta      <- top_candidates_meta[i, ]
      edf_tbl   <- candidate_edf_tables[[i]]
      total_edf <- if (!is.null(edf_tbl)) sum(edf_tbl$edf) else NA_real_
      tibble(
         candidate_rank = meta$candidate_rank,
         total_edf      = total_edf,
         mean_high_rmse = meta$mean_high_rmse,
         sd_high_rmse   = meta$sd_high_rmse,
         mean_rmse      = meta$mean_rmse
      )
   }) %>% mutate(label = paste0("C", candidate_rank))
   
   # ============================================================================
   # SELECTION PLOTS
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
   
   # Plot A: Accuracy vs Complexity
   pA <- ggplot(candidate_summary,
                aes(x = total_edf, y = mean_high_rmse, label = label)) +
      # 1. Plot the error bars
      geom_errorbar(aes(ymin = mean_high_rmse - sd_high_rmse,
                        ymax = mean_high_rmse + sd_high_rmse),
                    width = 0.4, color = gam_colors$secondary, linewidth = 0.8) +
      # 2. Plot the central mean points
      geom_point(size = 4, color = gam_colors$primary) +
      # 3. Position text at the BOTTOM of the error bar (ymin) and push it down slightly
      geom_text(aes(y = mean_high_rmse - sd_high_rmse), 
                vjust = 1.5, size = 4, color = gam_colors$dark, fontface = "bold") +
      # 4. Dynamically pad the y-axis boundaries so long error bars don't clip out
      scale_y_continuous(
         limits = c(
            min(candidate_summary$mean_high_rmse - candidate_summary$sd_high_rmse) * 1.2,
            max(candidate_summary$mean_high_rmse + candidate_summary$sd_high_rmse) * 1.1
         )
      ) +
      labs(title = "Accuracy vs Complexity",
           x     = "Total Expected Degrees of Freedom",
           y     = "Mean High-Salinity RMSE Across Folds (ppt)") +
      gam_theme
   
   # Plot B: Accuracy vs Consistency
   pB <- ggplot(candidate_summary,
                aes(x = mean_high_rmse, y = sd_high_rmse, label = label)) +
      geom_point(size = 4, color = gam_colors$secondary) +
      geom_text(vjust = -0.9, size = 4, color = gam_colors$dark, fontface = "bold") +
      labs(title = "Accuracy vs Consistency",
           x     = "Mean High-Salinity RMSE Across Folds (ppt)",
           y     = "SD of High-Salinity RMSE Across Folds") +
      gam_theme
   
   # Plot C: Per-term EDF heatmap
   edf_all <- bind_rows(candidate_edf_tables) %>%
      filter(!is.na(edf)) %>%
      mutate(term_short = term %>%
                gsub("^s\\(|^ti\\(", "", .) %>%
                gsub("\\)$", "", .) %>%
                gsub(", bs=.*", "", .))
   
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
   
   # Plot D: Fold-level high-RMSE profiles
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
   
   # Save plots
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
   
   # ============================================================================
   # CANDIDATE SUMMARY TABLE
   # ============================================================================
   
   cat("=== CANDIDATE SELECTION SUMMARY ===\n")
   cat("Inspect plots in:", plot_output_dir, "\n")
   cat("Set SELECTED_CANDIDATE_RANK in Script 04 then run Phase 3.\n\n")
   
   print(candidate_summary %>%
            select(candidate_rank, total_edf, mean_rmse, mean_high_rmse, sd_high_rmse) %>%
            mutate(across(where(is.numeric), ~ round(., 4))))
   cat("\n")
   
   # ============================================================================
   # RETURN — metadata only, no bam objects
   # ============================================================================
   
   list(
      tune_grid         = tune_results,
      top_candidates    = top_candidates_meta,   # k-values + CV metrics per candidate
      candidate_summary = candidate_summary,      # total EDF + CV metrics
      edf_tables        = candidate_edf_tables,  # per-term EDF per candidate
      data_clean        = data_clean,             # NA-filtered data for refit
      model_cols        = model_cols,             # column set for bam
      fit_params        = list(                  # all params needed to refit
         family_type    = family_type,
         link           = link,
         tweedie_p      = tweedie_p,
         method         = method,
         discrete       = discrete,
         nthreads       = nthreads,
         gam_select     = gam_select,
         basis_default  = basis_default,
         basis_horizon  = basis_horizon,
         active_k_types = active_k_types
      ),
      model_type     = "gam",
      transform_info = list(family = family_type, link = link)
   )
}


# =============================================================================
# Function:       select_gam_candidate
# Description:    Refits the chosen candidate from saved metadata and promotes
#                 it to the final-model structure expected by downstream scripts.
#                 Call in Script 04 after inspecting selection plots.
#
# Usage:
#   gam_unified <- select_gam_candidate(gam_candidates, rank = 2)
# =============================================================================

select_gam_candidate <- function(candidates_output, rank = 1) {
   
   library(mgcv)
   library(dplyr)
   
   meta_row <- candidates_output$top_candidates %>%
      filter(candidate_rank == rank)
   
   if (nrow(meta_row) == 0) {
      stop("No candidate with rank ", rank, ". Available ranks: ",
           paste(candidates_output$top_candidates$candidate_rank, collapse = ", "))
   }
   
   p          <- candidates_output$fit_params
   data_clean <- candidates_output$data_clean
   model_cols <- candidates_output$model_cols
   
   gam_family <- switch(p$family_type,
                        "gaussian" = gaussian(link = p$link),
                        "Gamma"    = Gamma(link = p$link),
                        "Tweedie"  = Tweedie(p = p$tweedie_p, link = p$link),
                        stop("Unknown family_type"))
   
   # Rebuild formula from saved k-values
   # Infer predictor groups from model_cols
   non_h_cols     <- setdiff(model_cols, c("h", "Response", "WindDir"))
   lag_vars       <- non_h_cols[grepl("LagSalinity",                     non_h_cols, ignore.case = TRUE)]
   sustained_vars <- non_h_cols[grepl("RollingDischarge|RollingAnomaly", non_h_cols, ignore.case = TRUE)]
   flushing_vars  <- non_h_cols[grepl("MaxDischarge|ExceedFlux",         non_h_cols, ignore.case = TRUE)]
   tide_vars      <- non_h_cols[grepl("TideRange|TideMean",              non_h_cols, ignore.case = TRUE)]
   wind_vars      <- non_h_cols[grepl("RollingWindAlong|RollingWindCross",non_h_cols, ignore.case = TRUE)]
   classified     <- c(lag_vars, sustained_vars, flushing_vars, tide_vars, wind_vars)
   other_vars     <- setdiff(non_h_cols, classified)
   ti_vars        <- non_h_cols
   
   k_h              <- meta_row$k_h
   k_interaction    <- meta_row$k_interaction
   k_sustained_flow <- meta_row$k_sustained_flow
   k_flushing_flow  <- meta_row$k_flushing_flow
   k_physical       <- meta_row$k_physical
   k_wind           <- meta_row$k_wind
   
   terms <- c()
   terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", p$basis_horizon, "')"))
   if (length(lag_vars)       > 0) terms <- c(terms, lag_vars)
   if (length(sustained_vars) > 0) terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow, ", bs='", p$basis_default, "')"))
   if (length(flushing_vars)  > 0) terms <- c(terms, paste0("s(", flushing_vars,  ", k=", k_flushing_flow,  ", bs='", p$basis_default, "')"))
   if (length(tide_vars)      > 0) terms <- c(terms, paste0("s(", tide_vars,      ", k=", k_physical,       ", bs='", p$basis_default, "')"))
   if (length(wind_vars)      > 0) terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,    ", bs='", p$basis_default, "')"))
   if (length(other_vars)     > 0) terms <- c(terms, paste0("s(", other_vars,     ", k=", k_physical,       ", bs='", p$basis_default, "')"))
   for (var in ti_vars) {
      terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
                               "), bs=c('", p$basis_horizon, "', '", p$basis_default, "'))"))
   }
   
   final_formula <- as.formula(paste("Response ~", paste(terms, collapse = " + ")))
   
   cat("=== REFITTING CANDIDATE", rank, "===\n")
   cat("Formula:\n"); print(final_formula); cat("\n")
   
   final_gam <- do.call(bam, list(
      formula  = final_formula,
      data     = data_clean %>% select(all_of(model_cols)),
      family   = gam_family,
      method   = p$method,
      discrete = p$discrete,
      nthreads = p$nthreads,
      select   = p$gam_select
   ))
   
   cat("\n=== MODEL SUMMARY ===\n")
   print(summary(final_gam))
   cat("\n")
   
   cat("=== BASIS CHECK ===\n")
   cat("(Increase k if k-index < 1 and p < 0.05)\n\n")
   print(k.check(final_gam, n.rep = 0))
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
   cat("Significant terms (p < 0.05):", nrow(sig_terms), "/", nrow(smooth_info), "\n\n")
   
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
         meta_row %>% select(all_of(p$active_k_types)),
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

# # =============================================================================
# # Function:       fit_gam
# # Project:        Chapter1
# # Description:    Fits the unified multi-horizon salinity GAM on stacked
# #                 date-horizon data. h is treated as an explicit smooth term,
# #                 with ti(h, predictor) interactions allowing each predictor's
# #                 contribution to vary across forecast lead times.
# #                 LagSalinity enters as a linear main effect (per prior
# #                 workflow) but still receives a ti(h, LagSalinity) interaction
# #                 to capture horizon-dependent decay of its coefficient.
# #                 Wind enters with by = WindDir (RightBank/LeftBank or
# #                 UpEstuary/DownEstuary depending on Along vs Cross), but the
# #                 ti(h, wind) interaction is NOT split by WindDir.
# #
# #                 K is tuned via expanding-window CV across grouped k-ranges.
# #                 The top N candidates by mean high-salinity RMSE are refit on
# #                 the full training data; selection plots are saved so the user
# #                 can inspect and manually choose a candidate in Script 04.
# # =============================================================================
# 
# fit_gam <- function(data,
#                     response = 'Salinity_h',
#                     predictors = NULL,           # must include 'h'
#                     folds = NULL,
#                     high_salinity_threshold = 0.15,
#                     
#                     # Distribution family
#                     family_type = "gaussian",   # "gaussian", "Gamma", "Tweedie"
#                     link = NULL,                 # NULL = auto-select
#                     tweedie_p = 1.5,
#                     
#                     # Smoothing parameters by term group
#                     k_h_range             = c(4, 12),
#                     k_interaction_range   = c(4, 12),
#                     k_sustained_flow_range = c(4, 12),
#                     k_flushing_flow_range  = c(4, 12),
#                     k_physical_range       = c(4, 10),
#                     k_wind_range           = c(4, 10),
#                     
#                     # Additional custom interactions, beyond the automatic
#                     # ti(h, predictor) terms. List of list(vars = c(...))
#                     interactions = list(),
#                     
#                     # Basis types
#                     basis_default = 'tp',
#                     basis_horizon = 'cr',
#                     
#                     # BAM parameters
#                     method = 'fREML',
#                     discrete = TRUE,
#                     nthreads = 4,
#                     gam_select = TRUE,
#                     
#                     # Tuning control
#                     gam_levels = 3,
#                     
#                     # Candidate selection
#                     n_top_candidates = 10,
#                     plot_output_dir  = 'Outputs/Plots/UnifiedGAM/GAMSelection') {
#    
#    library(mgcv)
#    library(dplyr)
#    library(purrr)
#    library(ggplot2)
#    library(tidyr)
#    
#    if (!('h' %in% predictors)) {
#       stop("predictors must include 'h' for the unified multi-horizon GAM.")
#    }
#    
#    # ============================================================================
#    # COLOR PALETTE
#    # ============================================================================
#    
#    gam_colors <- list(
#       primary   = "#f58220",
#       secondary = "#009bba",
#       tertiary  = "#fdb515",
#       dark      = "#002030",
#       threshold = "#002030"
#    )
#    
#    # ============================================================================
#    # SET UP FAMILY OBJECT
#    # ============================================================================
#    
#    if (is.null(link)) {
#       link <- switch(family_type,
#                      "gaussian" = "identity",
#                      "Gamma"    = "log",
#                      "Tweedie"  = "log")
#       cat("Auto-selected link:", link, "for", family_type, "family\n\n")
#    }
#    
#    if (family_type == "Gamma" && link == "identity") {
#       warning("Identity link with Gamma can produce negative predictions. ",
#               "Consider link = 'log'.\n")
#    }
#    
#    gam_family <- switch(family_type,
#                         "gaussian" = gaussian(link = link),
#                         "Gamma"    = Gamma(link = link),
#                         "Tweedie"  = Tweedie(p = tweedie_p, link = link),
#                         stop("Unknown family_type"))
#    
#    # ============================================================================
#    # CLASSIFY PREDICTORS
#    # ============================================================================
#    
#    h_var <- 'h'
#    non_h_predictors <- setdiff(predictors, h_var)
#    
#    lag_vars       <- non_h_predictors[grepl("LagSalinity",              non_h_predictors, ignore.case = TRUE)]
#    sustained_vars <- non_h_predictors[grepl("RollingDischarge|RollingAnomaly", non_h_predictors, ignore.case = TRUE)]
#    flushing_vars  <- non_h_predictors[grepl("MaxDischarge|ExceedFlux",  non_h_predictors, ignore.case = TRUE)]
#    tide_vars      <- non_h_predictors[grepl("TideRange|TideMean",       non_h_predictors, ignore.case = TRUE)]
#    wind_vars      <- non_h_predictors[grepl("RollingWindAlong|RollingWindCross", non_h_predictors, ignore.case = TRUE)]
#    
#    classified <- c(lag_vars, sustained_vars, flushing_vars, tide_vars, wind_vars)
#    other_vars <- setdiff(non_h_predictors, classified)
#    
#    has_lag        <- length(lag_vars)       > 0
#    has_sustained  <- length(sustained_vars) > 0
#    has_flushing   <- length(flushing_vars)  > 0
#    has_tide       <- length(tide_vars)      > 0
#    has_wind       <- length(wind_vars)      > 0
#    has_other      <- length(other_vars)     > 0
#    has_interactions <- length(interactions) > 0
#    
#    ti_vars <- non_h_predictors   # every non-h predictor gets ti(h, predictor)
#    
#    # ============================================================================
#    # PREPARE DATA
#    # ============================================================================
#    
#    data_subset <- data %>%
#       mutate(.row_id = row_number()) %>%
#       select(.row_id, DateTime, all_of(response), all_of(predictors))
#    
#    if (has_wind) {
#       wind_var <- wind_vars[1]
#       if (grepl("Along", wind_var)) {
#          data_subset <- data_subset %>%
#             mutate(WindDir = factor(
#                ifelse(.data[[wind_var]] >= 0, "UpEstuary", "DownEstuary"),
#                levels = c("DownEstuary", "UpEstuary")
#             ))
#       } else {
#          data_subset <- data_subset %>%
#             mutate(WindDir = factor(
#                ifelse(.data[[wind_var]] >= 0, "RightBank", "LeftBank"),
#                levels = c("LeftBank", "RightBank")
#             ))
#       }
#    }
#    
#    complete_rows <- complete.cases(data_subset %>% select(-.row_id))
#    
#    data_clean <- data_subset %>%
#       filter(complete_rows) %>%
#       mutate(
#          Response          = .data[[response]],
#          Response_original = .data[[response]]
#       )
#    
#    cat("=== DATA PREPARATION ===\n")
#    cat("Original rows:", format(nrow(data), big.mark = ","), "\n")
#    cat("After removing NAs:", format(nrow(data_clean), big.mark = ","), "\n")
#    cat("Response range: [", round(min(data_clean$Response), 4), ", ",
#        round(max(data_clean$Response), 4), "]\n\n")
#    
#    # ============================================================================
#    # CHECK DATA REQUIREMENTS for non-Gaussian families
#    # ============================================================================
#    
#    if (family_type == "Gamma" && any(data_clean$Response <= 0)) {
#       n_bad <- sum(data_clean$Response <= 0)
#       cat("WARNING: Gamma requires positive values. Found", n_bad,
#           "values <= 0. Adding 0.001.\n\n")
#       data_clean$Response          <- pmax(data_clean$Response, 0.001)
#       data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
#    }
#    
#    if (family_type == "Tweedie" && any(data_clean$Response < 0)) {
#       n_bad <- sum(data_clean$Response < 0)
#       cat("WARNING: Tweedie requires non-negative values. Found", n_bad,
#           "negative values. Setting to 0.001.\n\n")
#       data_clean$Response          <- pmax(data_clean$Response, 0.001)
#       data_clean$Response_original <- pmax(data_clean$Response_original, 0.001)
#    }
#    
#    # ============================================================================
#    # CREATE K TUNING GRID
#    # ============================================================================
#    
#    k_sequences <- list()
#    
#    k_sequences$k_h <- unique(round(seq(
#       k_h_range[1], k_h_range[2], length.out = gam_levels)))
#    
#    k_sequences$k_interaction <- unique(round(seq(
#       k_interaction_range[1], k_interaction_range[2], length.out = gam_levels)))
#    
#    if (has_sustained) {
#       k_sequences$k_sustained_flow <- unique(round(seq(
#          k_sustained_flow_range[1], k_sustained_flow_range[2], length.out = gam_levels)))
#    }
#    if (has_flushing) {
#       k_sequences$k_flushing_flow <- unique(round(seq(
#          k_flushing_flow_range[1], k_flushing_flow_range[2], length.out = gam_levels)))
#    }
#    if (has_tide || has_other) {
#       k_sequences$k_physical <- unique(round(seq(
#          k_physical_range[1], k_physical_range[2], length.out = gam_levels)))
#    }
#    if (has_wind) {
#       k_sequences$k_wind <- unique(round(seq(
#          k_wind_range[1], k_wind_range[2], length.out = gam_levels)))
#    }
#    
#    k_grid <- expand.grid(k_sequences) %>% distinct()
#    
#    if (!has_sustained)            k_grid$k_sustained_flow <- k_sustained_flow_range[1]
#    if (!has_flushing)             k_grid$k_flushing_flow  <- k_flushing_flow_range[1]
#    if (!has_tide && !has_other)   k_grid$k_physical       <- k_physical_range[1]
#    if (!has_wind)                 k_grid$k_wind           <- k_wind_range[1]
#    
#    k_grid <- k_grid %>%
#       select(k_h, k_interaction, k_sustained_flow, k_flushing_flow, k_physical, k_wind)
#    
#    active_k_types <- names(k_sequences)
#    
#    # ============================================================================
#    # MODEL SETUP SUMMARY
#    # ============================================================================
#    
#    cat("=== GAM MODEL SETUP ===\n")
#    cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
#    cat("Response:", response, "\n")
#    cat("Predictors:", length(predictors), "\n")
#    cat("Family:", family_type, "with", link, "link\n")
#    if (family_type == "Tweedie") cat("Tweedie power:", tweedie_p, "\n")
#    cat("\n")
#    
#    cat("Term groups:\n")
#    cat("  Horizon (h):       s(h)", if (length(ti_vars) > 0) paste0(" + ti(h, ", ti_vars, ")", collapse = "") else "", "\n")
#    if (has_lag)       cat("  LagSalinity:       ", paste(lag_vars,       collapse = ", "), "(linear)\n")
#    if (has_sustained) cat("  Sustained flow:    ", paste(sustained_vars, collapse = ", "), "\n")
#    if (has_flushing)  cat("  Flushing flow:     ", paste(flushing_vars,  collapse = ", "), "\n")
#    if (has_tide)      cat("  Tide:              ", paste(tide_vars,      collapse = ", "), "\n")
#    if (has_wind)      cat("  Wind:              ", paste(wind_vars,      collapse = ", "), "(by = WindDir)\n")
#    if (has_other)     cat("  Other:             ", paste(other_vars,     collapse = ", "), "\n")
#    if (has_interactions) cat("  Custom interactions:", length(interactions), "\n")
#    
#    cat("\nTuning", nrow(k_grid), "k combinations across:", paste(active_k_types, collapse = ", "), "\n")
#    print(k_grid %>% select(all_of(active_k_types)))
#    cat("\n")
#    
#    # ============================================================================
#    # BUILD FORMULA FUNCTION
#    # ============================================================================
#    
#    build_gam_formula <- function(k_h, k_interaction, k_sustained_flow,
#                                  k_flushing_flow, k_physical, k_wind) {
#       
#       terms <- c()
#       
#       terms <- c(terms, paste0("s(h, k=", k_h, ", bs='", basis_horizon, "')"))
#       
#       if (has_lag) {
#          terms <- c(terms, lag_vars)
#       }
#       
#       if (has_sustained) {
#          terms <- c(terms, paste0("s(", sustained_vars, ", k=", k_sustained_flow,
#                                   ", bs='", basis_default, "')"))
#       }
#       
#       if (has_flushing) {
#          terms <- c(terms, paste0("s(", flushing_vars, ", k=", k_flushing_flow,
#                                   ", bs='", basis_default, "')"))
#       }
#       
#       if (has_tide) {
#          terms <- c(terms, paste0("s(", tide_vars, ", k=", k_physical,
#                                   ", bs='", basis_default, "')"))
#       }
#       
#       if (has_wind) {
#          terms <- c(terms, paste0("s(", wind_vars, ", by=WindDir, k=", k_wind,
#                                   ", bs='", basis_default, "')"))
#       }
#       
#       if (has_other) {
#          terms <- c(terms, paste0("s(", other_vars, ", k=", k_physical,
#                                   ", bs='", basis_default, "')"))
#       }
#       
#       for (var in ti_vars) {
#          terms <- c(terms, paste0("ti(h, ", var, ", k=c(", k_h, ", ", k_interaction,
#                                   "), bs=c('", basis_horizon, "', '", basis_default, "'))"))
#       }
#       
#       if (has_interactions) {
#          for (int in interactions) {
#             if (all(int$vars %in% predictors)) {
#                terms <- c(terms, paste0("ti(", paste(int$vars, collapse = ", "),
#                                         ", k=", k_interaction, ")"))
#             }
#          }
#       }
#       
#       as.formula(paste("Response ~", paste(terms, collapse = " + ")))
#    }
#    
#    # ============================================================================
#    # HELPER: Fit and evaluate one fold
#    # ============================================================================
#    
#    model_cols <- setdiff(names(data_clean),
#                          c(".row_id", "DateTime", response, "Response_original"))
#    
#    fit_fold <- function(formula, train_idx, test_idx, fold_num) {
#       
#       train_data <- data_clean %>%
#          filter(.row_id %in% train_idx) %>%
#          select(all_of(model_cols))
#       
#       test_data <- data_clean %>%
#          filter(.row_id %in% test_idx)
#       
#       bam_args <- list(
#          formula  = formula,
#          data     = train_data,
#          family   = gam_family,
#          method   = method,
#          discrete = discrete,
#          nthreads = nthreads,
#          select   = gam_select
#       )
#       
#       gam_fit <- tryCatch({
#          suppressWarnings(do.call(bam, bam_args))
#       }, error = function(e) {
#          cat("    [Fold", fold_num, "error:", e$message, "]\n")
#          return(NULL)
#       })
#       
#       fail_result <- list(
#          rmse = NA_real_, rsq = NA_real_, mae = NA_real_,
#          high_rmse = NA_real_, high_rsq = NA_real_, high_mae = NA_real_,
#          n_high_sal = 0L, failed = TRUE
#       )
#       
#       if (is.null(gam_fit)) return(fail_result)
#       
#       preds <- tryCatch({
#          predict(gam_fit,
#                  newdata = test_data %>% select(all_of(model_cols)),
#                  type = "response")
#       }, error = function(e) {
#          cat("    [Fold", fold_num, "prediction error:", e$message, "]\n")
#          return(NULL)
#       })
#       
#       if (is.null(preds) || any(!is.finite(preds))) return(fail_result)
#       
#       preds_original <- preds
#       
#       if (any(!is.finite(preds_original))) return(fail_result)
#       
#       overall_rmse <- sqrt(mean((test_data$Response_original - preds_original)^2))
#       overall_rsq  <- cor(test_data$Response_original, preds_original)^2
#       overall_mae  <- mean(abs(test_data$Response_original - preds_original))
#       
#       high_idx <- test_data$Response_original > high_salinity_threshold
#       n_high   <- sum(high_idx)
#       
#       if (n_high > 1) {
#          high_rmse <- sqrt(mean((test_data$Response_original[high_idx] - preds_original[high_idx])^2))
#          high_rsq  <- cor(test_data$Response_original[high_idx], preds_original[high_idx])^2
#          high_mae  <- mean(abs(test_data$Response_original[high_idx] - preds_original[high_idx]))
#       } else {
#          high_rmse <- NA_real_
#          high_rsq  <- NA_real_
#          high_mae  <- NA_real_
#       }
#       
#       list(
#          rmse = overall_rmse, rsq = overall_rsq, mae = overall_mae,
#          high_rmse = high_rmse, high_rsq = high_rsq, high_mae = high_mae,
#          n_high_sal = n_high, failed = FALSE
#       )
#    }
#    
#    # ============================================================================
#    # CROSS-VALIDATION ACROSS ALL K COMBINATIONS
#    # ============================================================================
#    
#    cat("Running CV across", nrow(k_grid), "k combinations...\n")
#    
#    # fold_cv_results stores per-fold metrics for every k combination
#    # so we can compute sd_high_rmse and plot fold profiles later
#    all_fold_results <- vector("list", nrow(k_grid))
#    
#    tune_results <- map_dfr(1:nrow(k_grid), function(i) {
#       
#       k_vals  <- k_grid[i, ]
#       formula <- build_gam_formula(k_vals$k_h, k_vals$k_interaction,
#                                    k_vals$k_sustained_flow, k_vals$k_flushing_flow,
#                                    k_vals$k_physical, k_vals$k_wind)
#       
#       active_k_str <- paste(
#          sapply(active_k_types, function(k) paste0(k, "=", k_vals[[k]])),
#          collapse = ", "
#       )
#       cat("  ", active_k_str)
#       
#       fold_results <- map_dfr(seq_along(folds), function(j) {
#          res <- fit_fold(formula, folds[[j]]$train, folds[[j]]$test, j)
#          tibble(fold = j, !!!res)
#       })
#       
#       # Store per-fold results indexed by k combination
#       all_fold_results[[i]] <<- fold_results %>% mutate(k_index = i)
#       
#       summary_row <- fold_results %>%
#          summarize(
#             mean_rmse      = mean(rmse,      na.rm = TRUE),
#             mean_rsq       = mean(rsq,       na.rm = TRUE),
#             mean_mae       = mean(mae,       na.rm = TRUE),
#             sd_rmse        = sd(rmse,        na.rm = TRUE),
#             mean_high_rmse = mean(high_rmse, na.rm = TRUE),
#             mean_high_rsq  = mean(high_rsq,  na.rm = TRUE),
#             mean_high_mae  = mean(high_mae,  na.rm = TRUE),
#             sd_high_rmse   = sd(high_rmse,   na.rm = TRUE),
#             total_high_sal = sum(n_high_sal, na.rm = TRUE),
#             n_failed       = sum(failed)
#          )
#       
#       cat(" -> RMSE:", round(summary_row$mean_rmse, 4),
#           "| High-Sal RMSE:", round(summary_row$mean_high_rmse, 4),
#           "| SD:", round(summary_row$sd_high_rmse, 4))
#       if (summary_row$n_failed > 0) cat(" [", summary_row$n_failed, "failed]")
#       cat("\n")
#       
#       bind_cols(k_vals %>% mutate(k_index = i), summary_row)
#    })
#    
#    # Combine all fold-level results into one dataframe
#    fold_cv_all <- bind_rows(all_fold_results)
#    
#    cat("\n=== CV RESULTS (all combinations) ===\n")
#    print(tune_results %>%
#             arrange(mean_high_rmse) %>%
#             select(all_of(c(active_k_types, "mean_rmse", "mean_high_rmse",
#                             "sd_high_rmse", "n_failed"))))
#    cat("\n")
#    
#    # ============================================================================
#    # FILTER TO TOP N CANDIDATES
#    # ============================================================================
#    
#    n_candidates <- min(n_top_candidates, nrow(tune_results %>% filter(n_failed < length(folds))))
#    
#    top_candidates_meta <- tune_results %>%
#       filter(n_failed < length(folds)) %>%
#       arrange(mean_high_rmse) %>%
#       slice_head(n = n_candidates) %>%
#       mutate(candidate_rank = row_number())
#    
#    cat("=== TOP", n_candidates, "CANDIDATES (by mean high-salinity RMSE) ===\n")
#    print(top_candidates_meta %>%
#             select(candidate_rank, all_of(active_k_types),
#                    mean_rmse, mean_high_rmse, sd_high_rmse))
#    cat("\n")
#    
#    # ============================================================================
#    # REFIT TOP CANDIDATES ON FULL TRAINING DATA
#    # ============================================================================
#    
#    cat("Refitting top", n_candidates, "candidates on full training data...\n\n")
#    
#    full_train_data <- data_clean %>% select(all_of(model_cols))
#    
#    top_candidates <- map(1:nrow(top_candidates_meta), function(i) {
#       
#       meta <- top_candidates_meta[i, ]
#       
#       cat("  Candidate", meta$candidate_rank,
#           paste(sapply(active_k_types, function(k) paste0(k, "=", meta[[k]])),
#                 collapse = ", "), "\n")
#       
#       formula <- build_gam_formula(meta$k_h, meta$k_interaction,
#                                    meta$k_sustained_flow, meta$k_flushing_flow,
#                                    meta$k_physical, meta$k_wind)
#       
#       gam_fit <- tryCatch({
#          do.call(bam, list(
#             formula  = formula,
#             data     = full_train_data,
#             family   = gam_family,
#             method   = method,
#             discrete = discrete,
#             nthreads = nthreads,
#             select   = gam_select
#          ))
#       }, error = function(e) {
#          cat("    [Refit error:", e$message, "]\n")
#          return(NULL)
#       })
#       
#       # Extract EDF table
#       edf_table <- NULL
#       total_edf <- NA_real_
#       
#       if (!is.null(gam_fit)) {
#          s_table <- summary(gam_fit)$s.table
#          edf_table <- tibble(
#             candidate_rank = meta$candidate_rank,
#             term           = rownames(s_table),
#             edf            = s_table[, "edf"],
#             p_value        = s_table[, "p-value"]
#          ) %>%
#             mutate(
#                term_group = case_when(
#                   grepl("LagSalinity",              term) ~ "LagSalinity",
#                   grepl("RollingDischarge|RollingAnomaly", term) ~ "SustainedDischarge",
#                   grepl("MaxDischarge|ExceedFlux",  term) ~ "FlushingDischarge",
#                   grepl("TideRange|TideMean",       term) ~ "Tide",
#                   grepl("RollingWindAlong|RollingWindCross", term) ~ "Wind",
#                   grepl("^s\\(h\\)|^ti\\(h",        term) ~ "Horizon",
#                   TRUE                                    ~ "Other"
#                )
#             )
#          total_edf <- sum(edf_table$edf)
#          cat("    Total EDF:", round(total_edf, 2), "\n")
#       }
#       
#       list(
#          candidate_rank = meta$candidate_rank,
#          k_vals         = meta %>% select(all_of(active_k_types)),
#          formula        = formula,
#          gam_object     = gam_fit,
#          edf_table      = edf_table,
#          total_edf      = total_edf,
#          cv_metrics     = meta %>% select(mean_rmse, mean_high_rmse,
#                                           sd_high_rmse, mean_rsq, n_failed)
#       )
#    })
#    
#    cat("\n")
#    
#    # ============================================================================
#    # SELECTION PLOTS
#    # ============================================================================
#    
#    cat("Building selection plots...\n")
#    
#    dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)
#    
#    # --- Shared theme -----------------------------------------------------------
#    
#    gam_theme <- theme_bw() +
#       theme(
#          plot.title    = element_text(size = 16, face = "bold",  color = gam_colors$dark),
#          plot.subtitle = element_text(size = 13,                 color = gam_colors$dark),
#          axis.title    = element_text(size = 14, face = "bold",  color = gam_colors$dark),
#          axis.text     = element_text(size = 12,                 color = gam_colors$dark),
#          panel.border  = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
#          legend.title  = element_text(size = 12, face = "bold",  color = gam_colors$dark),
#          legend.text   = element_text(size = 11,                 color = gam_colors$dark),
#          legend.background = element_rect(fill = "white", color = NA),
#          legend.key        = element_rect(fill = "white", color = NA)
#       )
#    
#    # Collect summary stats per candidate
#    candidate_summary <- map_dfr(top_candidates, function(cand) {
#       tibble(
#          candidate_rank = cand$candidate_rank,
#          total_edf      = cand$total_edf,
#          mean_high_rmse = cand$cv_metrics$mean_high_rmse,
#          sd_high_rmse   = cand$cv_metrics$sd_high_rmse,
#          mean_rmse      = cand$cv_metrics$mean_rmse
#       )
#    }) %>%
#       mutate(label = paste0("C", candidate_rank))
#    
#    # --- Plot A: mean_high_rmse vs total_edf ------------------------------------
#    
#    pA <- ggplot(candidate_summary,
#                 aes(x = total_edf, y = mean_high_rmse, label = label)) +
#       geom_point(size = 4, color = gam_colors$primary) +
#       geom_text(vjust = -0.8, size = 4, color = gam_colors$dark, fontface = "bold") +
#       geom_errorbar(aes(ymin = mean_high_rmse - sd_high_rmse,
#                         ymax = mean_high_rmse + sd_high_rmse),
#                     width = 0.3, color = gam_colors$secondary, linewidth = 0.8) +
#       labs(
#          title    = "Accuracy vs Complexity",
#          # subtitle = "Error bars = ±1 SD of high-salinity RMSE across folds",
#          x        = "Total Expected Degrees of Freedom",
#          y        = "Mean High-Salinity RMSE (ppt)"
#       ) +
#       ylim(-0.5, 0.5) + 
#       gam_theme
#    
#    # --- Plot B: mean_high_rmse vs sd_high_rmse (consistency plot) -------------
#    
#    pB <- ggplot(candidate_summary,
#                 aes(x = mean_high_rmse, y = sd_high_rmse, label = label)) +
#       geom_point(size = 4, color = gam_colors$secondary) +
#       geom_text(vjust = -0.8, size = 4, color = gam_colors$dark, fontface = "bold") +
#       labs(
#          title    = "Accuracy vs Consistency: Top Candidates",
#          # subtitle = "Lower-left corner = high accuracy AND low fold-to-fold variance",
#          x        = "Mean High-Salinity RMSE (ppt)",
#          y        = "SD of High-Salinity RMSE across Folds"
#       ) +
#       gam_theme
#    
#    # --- Plot C: per-term EDF heatmap -------------------------------------------
#    
#    edf_all <- map_dfr(top_candidates, ~ .x$edf_table) %>%
#       filter(!is.na(edf))
#    
#    # Shorten term labels for readability
#    edf_all <- edf_all %>%
#       mutate(term_short = gsub("^s\\(|^ti\\(", "", term) %>%
#                 gsub("\\)$", "", .) %>%
#                 gsub(", bs=.*", "", .))
#    
#    pC <- ggplot(edf_all,
#                 aes(x = factor(candidate_rank, labels = paste0("C", sort(unique(candidate_rank)))),
#                     y = reorder(term_short, edf, FUN = mean),
#                     fill = edf)) +
#       geom_tile(color = "white", linewidth = 0.5) +
#       geom_text(aes(label = round(edf, 1)), size = 3, color = "white", fontface = "bold") +
#       scale_fill_gradient(low = gam_colors$secondary, high = gam_colors$primary,
#                           name = "EDF") +
#       labs(
#          title    = "Per-Term Expected Degrees of Freedom",
#          #subtitle = "Brighter = more complexity; rows ordered by mean EDF",
#          x        = "Candidate",
#          y        = "Smooth Term"
#       ) +
#       gam_theme +
#       theme(axis.text.y = element_text(size = 9))
#    
#    # --- Plot D: fold-level high-RMSE profiles ----------------------------------
#    
#    fold_profiles <- fold_cv_all %>%
#       inner_join(top_candidates_meta %>% select(k_index, candidate_rank),
#                  by = "k_index") %>%
#       filter(!is.na(high_rmse))
#    
#    pD <- ggplot(fold_profiles,
#                 aes(x = fold, y = high_rmse,
#                     color = factor(candidate_rank),
#                     group = factor(candidate_rank))) +
#       geom_line(linewidth = 1.1) +
#       geom_point(size = 2.8) +
#       scale_color_manual(
#          values = setNames(
#             colorRampPalette(c(gam_colors$secondary, gam_colors$primary,
#                                gam_colors$tertiary))(n_candidates),
#             as.character(1:n_candidates)
#          ),
#          name = "Candidate"
#       ) +
#       labs(
#          title    = "High-Salinity RMSE by Fold",
#          #subtitle = "Consistent candidates track closely across all folds",
#          x        = "CV Fold",
#          y        = "High-Salinity RMSE"
#       ) +
#       scale_x_continuous(breaks = seq_along(folds)) +
#       gam_theme +
#       theme(legend.position = "right")
#    
#    # --- Save plots -------------------------------------------------------------
#    
#    # Plot A: 
#    ggsave(file.path(plot_output_dir, "AccuracyVsComplexity.png"),
#           pA, width = 8, height = 6, dpi = 600)
#    ggsave(file.path(plot_output_dir, "AccuracyVsComplexity.svg"),
#           pA, width = 8, height = 6, dpi = 600)
#    
#    # Plot B
#    ggsave(file.path(plot_output_dir, "AccuracyVsConsistency.png"),
#           pB, width = 8, height = 6, dpi = 600)
#    ggsave(file.path(plot_output_dir, "AccuracyVsConsistency.svg"),
#           pB, width = 8, height = 6, dpi = 600)
#    
#    # Plot C:
#    ggsave(file.path(plot_output_dir, "EDFHeatmap.png"),
#           pC, width = 10, height = max(6, nrow(edf_all %>% distinct(term_short)) * 0.35 + 2),
#           dpi = 600)
#    ggsave(file.path(plot_output_dir, "EDFHeatmap.svg"),
#           pC, width = 10, height = max(6, nrow(edf_all %>% distinct(term_short)) * 0.35 + 2),
#           dpi = 600)
#    
#    # Plot D:
#    ggsave(file.path(plot_output_dir, "FoldProfiles.png"),
#           pD, width = 10, height = 6, dpi = 600)
#    ggsave(file.path(plot_output_dir, "FoldProfiles.svg"),
#           pD, width = 10, height = 6, dpi = 600)
#    
#    cat("Plots saved to:", plot_output_dir, "\n\n")
#    
#    # ============================================================================
#    # PRINT CANDIDATE SUMMARY TABLE
#    # ============================================================================
#    
#    cat("=== CANDIDATE SELECTION SUMMARY ===\n")
#    cat("Inspect plots in:", plot_output_dir, "\n")
#    cat("Then set SELECTED_CANDIDATE_RANK in Script 04 and rerun the final fit.\n\n")
#    
#    print(candidate_summary %>%
#             select(candidate_rank, total_edf, mean_rmse,
#                    mean_high_rmse, sd_high_rmse) %>%
#             mutate(across(where(is.numeric), ~ round(., 4))))
#    
#    cat("\n")
#    
#    # ============================================================================
#    # RETURN — no final_fit yet; user selects in Script 04
#    # ============================================================================
#    
#    list(
#       tune_grid        = tune_results,
#       fold_cv_all      = fold_cv_all,
#       top_candidates   = top_candidates,
#       candidate_summary = candidate_summary,
#       model_type       = "gam",
#       transform_info   = list(family = family_type, link = link)
#    )
# }
# 
# 
# # =============================================================================
# # Function:       select_gam_candidate
# # Description:    Promotes one candidate from fit_gam output to the final-model
# #                 structure expected by downstream scripts.  Call this in
# #                 Script 04 after inspecting the selection plots.
# # Usage:
# #   gam_final <- select_gam_candidate(gam_candidates, rank = 2)
# # =============================================================================
# 
# select_gam_candidate <- function(candidates_output, rank = 1) {
#    
#    top <- candidates_output$top_candidates
#    sel <- Filter(function(x) x$candidate_rank == rank, top)
#    
#    if (length(sel) == 0) {
#       stop("No candidate with rank ", rank, " found. Available ranks: ",
#            paste(sapply(top, `[[`, "candidate_rank"), collapse = ", "))
#    }
#    
#    sel <- sel[[1]]
#    
#    if (is.null(sel$gam_object)) {
#       stop("Candidate ", rank, " has a NULL gam_object (refit failed).")
#    }
#    
#    cat("=== SELECTED CANDIDATE", rank, "===\n")
#    cat("Formula:\n"); print(sel$formula); cat("\n")
#    cat("Total EDF:", round(sel$total_edf, 2), "\n")
#    cat("CV Mean High-Sal RMSE:", round(sel$cv_metrics$mean_high_rmse, 4), "\n")
#    cat("CV SD  High-Sal RMSE:", round(sel$cv_metrics$sd_high_rmse, 4), "\n\n")
#    
#    cat("=== MODEL SUMMARY ===\n")
#    print(summary(sel$gam_object))
#    cat("\n")
#    
#    cat("=== BASIS CHECK ===\n")
#    cat("(Increase k if k-index < 1 and p < 0.05)\n\n")
#    print(k.check(sel$gam_object, n.rep = 0))
#    cat("\n")
#    
#    s_table  <- summary(sel$gam_object)$s.table
#    smooth_info <- tibble(
#       term    = rownames(s_table),
#       edf     = s_table[, "edf"],
#       ref_df  = s_table[, "Ref.df"],
#       F_stat  = s_table[, "F"],
#       p_value = s_table[, "p-value"]
#    ) %>% arrange(desc(edf))
#    
#    sig_terms <- smooth_info %>% filter(p_value < 0.05)
#    
#    cat("Significant terms (p < 0.05):", nrow(sig_terms), "/", nrow(smooth_info), "\n\n")
#    
#    # Build the workflow structure downstream scripts expect
#    gam_workflow <- structure(
#       list(fit = list(
#          fit     = sel$gam_object,
#          formula = sel$formula,
#          family  = candidates_output$transform_info$family
#       )),
#       class = c("workflow", "list")
#    )
#    
#    list(
#       tune_results   = candidates_output$tune_grid,
#       tune_grid      = candidates_output$tune_grid,
#       best_params    = bind_cols(
#          sel$k_vals,
#          tibble(family = candidates_output$transform_info$family,
#                 link   = candidates_output$transform_info$link)
#       ),
#       final_fit      = gam_workflow,
#       gam_object     = sel$gam_object,
#       formula        = sel$formula,
#       smooth_info    = smooth_info,
#       selected_vars  = sig_terms$term,
#       model_type     = "gam",
#       transform_info = candidates_output$transform_info
#    )
# }