fit_gam <- function(data,
                    response = 'Salinity',
                    predictors = NULL,
                    folds = NULL,
                    
                    # Response transformation (ONLY for Gaussian family)
                    transform_response = "none",  # "none", "log", "sqrt"
                    
                    # Distribution family
                    family_type = "gaussian",  # "gaussian", "Gamma", "Tweedie"
                    link = NULL,  # NULL = auto-select, or specify: "identity", "log"
                    tweedie_p = 1.5,  # power parameter if using Tweedie
                    
                    # Smoothing parameters by variable type
                    k_flow_range = c(8, 15),
                    k_physical_range = c(6, 12),
                    k_temporal_range = c(12, 12),  
                    k_interaction_range = c(6, 6), 
                    
                    # Interactions
                    interactions = list(),
                    
                    # Weighting
                    use_weights = FALSE,
                    weight_type = "threshold",
                    weight_threshold = 0.3,
                    weight_multiplier = 5,
                    
                    # Basis types
                    basis_default = 'tp',
                    basis_cyclical = 'cc',
                    
                    # BAM parameters
                    method = 'fREML',
                    discrete = TRUE,
                    nthreads = 4,
                    
                    # Tuning control
                    gam_levels = 3) {
   
   library(mgcv)
   library(dplyr)
   library(purrr)
   
   # ============================================================================
   # VALIDATION: Check that transformation and family are compatible
   # ============================================================================
   
   if (transform_response != "none" && family_type != "gaussian") {
      stop("Manual response transformation (transform_response) should ONLY be used with 'gaussian' family.\n",
           "Gamma and Tweedie families handle transformations internally via the link function.\n",
           "Set transform_response = 'none' and use family_type = 'Gamma' with link = 'log'.")
   }
   
   # ============================================================================
   # SET UP FAMILY OBJECT with appropriate link functions
   # ============================================================================
   
   # Auto-select link if not specified
   if (is.null(link)) {
      link <- switch(
         family_type,
         "gaussian" = "identity",
         "Gamma" = "log",
         "Tweedie" = "log"
      )
      cat("Auto-selected link function:", link, "for", family_type, "family\n\n")
   }
   
   # Validate link choices
   if (family_type == "Gamma" && link == "identity") {
      warning("Identity link with Gamma family can produce negative predictions.\n",
              "Strongly recommend using link = 'log'. Proceeding with identity anyway...\n")
   }
   
   # Create family object
   gam_family <- switch(
      family_type,
      "gaussian" = gaussian(link = link),
      "Gamma" = Gamma(link = link),
      "Tweedie" = Tweedie(p = tweedie_p, link = link),
      stop("Unknown family_type. Use 'gaussian', 'Gamma', or 'Tweedie'")
   )
   
   # ============================================================================
   # PREPARE DATA
   # ============================================================================
   
   # Select and clean data
   data_clean <- data %>%
      mutate(Response = .data[[response]]) %>%
      dplyr::select(DateTime, Response, all_of(predictors)) %>%
      drop_na()
   
   # Store original response for metrics (always compute metrics on original scale)
   data_clean$Response_original <- data_clean$Response
   
   # ============================================================================
   # APPLY MANUAL TRANSFORMATION (only for Gaussian family)
   # ============================================================================
   
   if (family_type == "gaussian" && transform_response == "log") {
      if (any(data_clean$Response <= 0)) {
         stop("Cannot log-transform non-positive values. Check your data.")
      }
      data_clean$Response <- log(data_clean$Response)
      cat("Applied log transformation to response (Gaussian family).\n")
      cat("Original range: [", round(min(data_clean$Response_original), 4), ", ",
          round(max(data_clean$Response_original), 4), "]\n")
      cat("Log-scale range: [", round(min(data_clean$Response), 4), ", ",
          round(max(data_clean$Response), 4), "]\n\n")
      
   } else if (family_type == "gaussian" && transform_response == "sqrt") {
      if (any(data_clean$Response < 0)) {
         stop("Cannot sqrt-transform negative values. Check your data.")
      }
      data_clean$Response <- sqrt(data_clean$Response)
      cat("Applied sqrt transformation to response (Gaussian family).\n")
      cat("Original range: [", round(min(data_clean$Response_original), 4), ", ",
          round(max(data_clean$Response_original), 4), "]\n")
      cat("Sqrt-scale range: [", round(min(data_clean$Response), 4), ", ",
          round(max(data_clean$Response), 4), "]\n\n")
   }
   
   # ============================================================================
   # CHECK DATA REQUIREMENTS for non-Gaussian families
   # ============================================================================
   
   if (family_type == "Gamma") {
      n_nonpositive <- sum(data_clean$Response <= 0)
      if (n_nonpositive > 0) {
         cat("WARNING: Gamma family requires strictly positive values.\n")
         cat("Found", n_nonpositive, "values <= 0. Adding small constant (0.001).\n\n")
         data_clean <- data_clean %>%
            mutate(
               Response = pmax(Response, 0.001),
               Response_original = pmax(Response_original, 0.001)
            )
      }
   }
   
   if (family_type == "Tweedie") {
      n_negative <- sum(data_clean$Response < 0)
      if (n_negative > 0) {
         cat("WARNING: Tweedie family requires non-negative values.\n")
         cat("Found", n_negative, "negative values. Setting to 0.001.\n\n")
         data_clean <- data_clean %>%
            mutate(
               Response = pmax(Response, 0.001),
               Response_original = pmax(Response_original, 0.001)
            )
      }
   }
   
   # ============================================================================
   # CREATE WEIGHTS
   # ============================================================================
   
   if (use_weights) {
      # Note: weights apply to TRANSFORMED response if using Gaussian + transform
      weight_target <- data_clean$Response
      
      data_clean <- data_clean %>%
         mutate(
            weight = case_when(
               weight_type == "threshold" & weight_target > weight_threshold ~ weight_multiplier,
               weight_type == "smooth" ~ 
                  1 + (weight_multiplier - 1) * plogis((weight_target - weight_threshold) / 0.1),
               weight_type == "exponential" ~ 
                  exp(pmax(0, (weight_target - weight_threshold) / 0.2)),
               TRUE ~ 1
            )
         )
      
      cat("Weight statistics:\n")
      cat("  Range:", round(min(data_clean$weight), 2), "to", 
          round(max(data_clean$weight), 2), "\n")
      cat("  Median:", round(median(data_clean$weight), 2), "\n")
      cat("  Mean:", round(mean(data_clean$weight), 2), "\n\n")
   } else {
      data_clean$weight <- 1
   }
   
   # ============================================================================
   # CLASSIFY PREDICTORS INTO GROUPS
   # ============================================================================
   
   flow_vars <- predictors[grepl("Discharge|Inflow|LogDischarge|LogInflow", 
                                 predictors, ignore.case = TRUE)]
   physical_vars <- predictors[grepl("Tide|RollingV|Wind", predictors, ignore.case = TRUE)]
   temporal_vars <- predictors[grepl("Sin|Cos|quarter|month", predictors, ignore.case = TRUE)]
   other_vars <- setdiff(predictors, c(flow_vars, physical_vars, temporal_vars))
   
   # ============================================================================
   # SMART K TUNING GRID CREATION
   # ============================================================================
   
   # Determine which variable types are actually present
   has_flow <- length(flow_vars) > 0
   has_physical <- length(physical_vars) > 0
   has_temporal <- length(temporal_vars) > 0
   has_other <- length(other_vars) > 0
   has_interactions <- length(interactions) > 0
   
   # Create sequences for each k type (only if that type exists)
   k_sequences <- list()
   
   if (has_flow) {
      k_sequences$k_flow <- unique(round(seq(k_flow_range[1], k_flow_range[2], 
                                             length.out = gam_levels)))
   } else {
      k_sequences$k_flow <- k_flow_range[1]  # Dummy value, won't be used
   }
   
   if (has_physical || has_other) {
      k_sequences$k_physical <- unique(round(seq(k_physical_range[1], k_physical_range[2], 
                                                 length.out = gam_levels)))
   } else {
      k_sequences$k_physical <- k_physical_range[1]  # Dummy value
   }
   
   if (has_temporal) {
      k_sequences$k_temporal <- unique(round(seq(k_temporal_range[1], k_temporal_range[2], 
                                                 length.out = gam_levels)))
   } else {
      k_sequences$k_temporal <- k_temporal_range[1]  # Dummy value
   }
   
   if (has_interactions) {
      k_sequences$k_interaction <- unique(round(seq(k_interaction_range[1], k_interaction_range[2], 
                                                    length.out = gam_levels)))
   } else {
      k_sequences$k_interaction <- k_interaction_range[1]  # Dummy value
   }
   
   # Build tuning grid only for variable types that exist
   active_k_types <- c()
   if (has_flow) active_k_types <- c(active_k_types, "k_flow")
   if (has_physical || has_other) active_k_types <- c(active_k_types, "k_physical")
   if (has_temporal) active_k_types <- c(active_k_types, "k_temporal")
   if (has_interactions) active_k_types <- c(active_k_types, "k_interaction")
   
   if (length(active_k_types) == 0) {
      stop("No predictors or interactions specified!")
   }
   
   # Create grid from only the active k types
   k_grid <- expand.grid(k_sequences[active_k_types]) %>%
      distinct()
   
   # Add dummy columns for inactive k types (needed for formula building)
   if (!has_flow) k_grid$k_flow <- k_flow_range[1]
   if (!has_physical && !has_other) k_grid$k_physical <- k_physical_range[1]
   if (!has_temporal) k_grid$k_temporal <- k_temporal_range[1]
   if (!has_interactions) k_grid$k_interaction <- k_interaction_range[1]
   
   # Reorder columns
   k_grid <- k_grid %>%
      select(k_flow, k_physical, k_temporal, k_interaction)
   
   # ============================================================================
   # MODEL SETUP SUMMARY
   # ============================================================================
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Predictors:", length(predictors), "\n")
   cat("Family:", family_type, "with", link, "link\n")
   if (family_type == "gaussian" && transform_response != "none") {
      cat("Response transformation:", transform_response, "(manual)\n")
   } else if (family_type != "gaussian") {
      cat("Response transformation: handled by", link, "link function\n")
   }
   if (family_type == "Tweedie") {
      cat("Tweedie power parameter:", tweedie_p, "\n")
   }
   cat("\n")
   
   cat("Variable groups:\n")
   if (has_flow) {
      cat("  Flow (k =", paste(unique(k_sequences$k_flow), collapse = ", "), "):", 
          paste(flow_vars, collapse = ", "), "\n")
   }
   if (has_physical) {
      cat("  Physical (k =", paste(unique(k_sequences$k_physical), collapse = ", "), "):", 
          paste(physical_vars, collapse = ", "), "\n")
   }
   if (has_temporal) {
      cat("  Temporal (k =", paste(unique(k_sequences$k_temporal), collapse = ", "), "):", 
          paste(temporal_vars, collapse = ", "), "\n")
   }
   if (has_other) {
      cat("  Other (k =", paste(unique(k_sequences$k_physical), collapse = ", "), "):", 
          paste(other_vars, collapse = ", "), "\n")
   }
   if (has_interactions) {
      cat("  Interactions (k =", paste(unique(k_sequences$k_interaction), collapse = ", "), "):",
          length(interactions), "specified\n")
   }
   cat("\n")
   
   cat("Tuning", nrow(k_grid), "k combinations across", length(active_k_types), 
       "active parameter types\n")
   cat("Active parameters:", paste(active_k_types, collapse = ", "), "\n")
   print(k_grid %>% select(all_of(active_k_types)))
   cat("\n")
   
   # ============================================================================
   # FUNCTION TO BUILD GAM FORMULA
   # ============================================================================
   
   build_gam_formula <- function(k_flow, k_physical, k_temporal, k_interaction) {
      
      terms <- c()
      
      # Flow variables
      if (has_flow) {
         terms <- c(terms, paste0("s(", flow_vars, ", k=", k_flow, ", bs='", basis_default, "')"))
      }
      
      # Physical variables
      if (has_physical) {
         terms <- c(terms, paste0("s(", physical_vars, ", k=", k_physical, ", bs='", basis_default, "')"))
      }
      
      # Temporal variables (cyclical)
      if (has_temporal) {
         terms <- c(terms, paste0("s(", temporal_vars, ", k=", k_temporal, ", bs='", basis_cyclical, "')"))
      }
      
      # Other variables
      if (has_other) {
         terms <- c(terms, paste0("s(", other_vars, ", k=", k_physical, ", bs='", basis_default, "')"))
      }
      
      # Add interactions (tensor products)
      if (has_interactions) {
         for (int in interactions) {
            if (all(int$vars %in% predictors)) {
               terms <- c(terms, 
                          paste0("ti(", paste(int$vars, collapse = ", "), 
                                 ", k=", k_interaction, ")"))
            }
         }
      }
      
      as.formula(paste("Response ~", paste(terms, collapse = " + ")))
   }
   
   # ============================================================================
   # CROSS-VALIDATION ACROSS K VALUES
   # ============================================================================
   
   cat("Running CV across k values...\n")
   tune_results <- map_dfr(1:nrow(k_grid), function(i) {
      
      k_flow <- k_grid$k_flow[i]
      k_physical <- k_grid$k_physical[i]
      k_temporal <- k_grid$k_temporal[i]
      k_interaction <- k_grid$k_interaction[i]
      
      # Only print active k values
      k_string <- paste(
         if (has_flow) paste0("k_flow=", k_flow) else NULL,
         if (has_physical || has_other) paste0("k_physical=", k_physical) else NULL,
         if (has_temporal) paste0("k_temporal=", k_temporal) else NULL,
         if (has_interactions) paste0("k_interaction=", k_interaction) else NULL,
         sep = ", "
      )
      cat("  ", k_string)
      
      formula <- build_gam_formula(k_flow, k_physical, k_temporal, k_interaction)
      
      # CV for this k combination
      fold_results <- map_dfr(seq_along(folds), function(j) {
         
         train_idx <- folds[[j]]$train
         test_idx <- folds[[j]]$test
         
         train_fold <- data_clean[train_idx, ] %>% dplyr::select(-DateTime)
         test_fold <- data_clean[test_idx, ] %>% dplyr::select(-DateTime)
         
         # Fit BAM with specified family
         gam_fit <- tryCatch({
            suppressWarnings(
               bam(formula, 
                   data = train_fold,
                   family = gam_family,
                   method = method,
                   discrete = discrete,
                   nthreads = nthreads)
            )
         }, error = function(e) {
            return(NULL)
         })
         
         if (is.null(gam_fit)) {
            return(tibble(
               fold = j,
               rmse = NA_real_,
               rsq = NA_real_,
               mae = NA_real_
            ))
         }
         
         # =====================================================================
         # PREDICT AND BACK-TRANSFORM (if needed)
         # =====================================================================
         
         preds <- predict(gam_fit, newdata = test_fold, type = "response")
         
         # Back-transform ONLY if we manually transformed (Gaussian + log/sqrt)
         if (family_type == "gaussian" && transform_response == "log") {
            sigma_sq <- summary(gam_fit)$scale
            preds_original <- exp(preds + sigma_sq/2)
            
         } else if (family_type == "gaussian" && transform_response == "sqrt") {
            preds_original <- preds^2
            
         } else {
            preds_original <- preds
         }
         
         # Compute metrics on ORIGINAL scale
         tibble(
            fold = j,
            rmse = sqrt(mean((test_fold$Response_original - preds_original)^2)),
            rsq = cor(test_fold$Response_original, preds_original)^2,
            mae = mean(abs(test_fold$Response_original - preds_original))
         )
      })
      
      # Aggregate across folds
      result <- fold_results %>%
         summarize(
            k_flow = k_flow,
            k_physical = k_physical,
            k_temporal = k_temporal,
            k_interaction = k_interaction,
            mean_rmse = mean(rmse, na.rm = TRUE),
            mean_rsq = mean(rsq, na.rm = TRUE),
            mean_mae = mean(mae, na.rm = TRUE),
            sd_rmse = sd(rmse, na.rm = TRUE),
            n_failed = sum(is.na(rmse))
         )
      
      cat(" → RMSE:", round(result$mean_rmse, 4), "\n")
      
      return(result)
   })
   
   cat("\n=== K TUNING RESULTS ===\n")
   print(tune_results %>% arrange(mean_rmse) %>% select(all_of(c(active_k_types, "mean_rmse", "mean_rsq"))))
   cat("\n")
   
   # ============================================================================
   # SELECT BEST K VALUES
   # ============================================================================
   
   # Select best k combination (break ties by choosing first - simplest due to ordering)
   best_k <- tune_results %>%
      slice_min(mean_rmse, n = 1, with_ties = FALSE)
   
   cat("=== BEST K VALUES ===\n")
   if (has_flow) cat("k_flow:", best_k$k_flow, "\n")
   if (has_physical || has_other) cat("k_physical:", best_k$k_physical, "\n")
   if (has_temporal) cat("k_temporal:", best_k$k_temporal, "\n")
   if (has_interactions) cat("k_interaction:", best_k$k_interaction, "\n")
   cat("Mean CV RMSE:", round(best_k$mean_rmse, 4), "\n")
   cat("Mean CV R²:", round(best_k$mean_rsq, 4), "\n\n")
   
   # ============================================================================
   # FIT FINAL MODEL
   # ============================================================================
   
   cat("Fitting final BAM with best k values...\n")
   final_formula <- build_gam_formula(best_k$k_flow, best_k$k_physical, 
                                      best_k$k_temporal, best_k$k_interaction)
   
   cat("Formula:\n")
   print(final_formula)
   cat("\n")
   
   start_time <- Sys.time()
   final_gam <- bam(
      final_formula,
      data = data_clean %>% dplyr::select(-DateTime),
      family = gam_family,
      method = method,
      discrete = discrete,
      nthreads = nthreads,
      weights = data_clean$weight
   )
   end_time <- Sys.time()
   
   cat("Fitting time:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n\n")
   
   # ============================================================================
   # MODEL SUMMARY
   # ============================================================================
   
   cat("=== FINAL MODEL SUMMARY ===\n")
   print(summary(final_gam))
   cat("\n")
   
   cat("Deviance explained:", round(summary(final_gam)$dev.expl * 100, 2), "%\n")
   cat("R-squared (adj):", round(summary(final_gam)$r.sq, 4), "\n")
   cat("AIC:", round(AIC(final_gam), 2), "\n\n")
   
   # ============================================================================
   # CHECK BASIS DIMENSIONS
   # ============================================================================
   
   cat("=== BASIS DIMENSION CHECK ===\n")
   cat("(If k-index < 1 and p < 0.05, increase k for that term)\n\n")
   k_check <- k.check(final_gam, n.rep = 0)
   print(k_check)
   cat("\n")
   
   # ============================================================================
   # EXTRACT SMOOTH INFORMATION
   # ============================================================================
   
   s_table <- summary(final_gam)$s.table
   smooth_info <- tibble(
      term = rownames(s_table),
      edf = s_table[, "edf"],
      ref_df = s_table[, "Ref.df"],
      F_stat = s_table[, "F"],
      p_value = s_table[, "p-value"]
   ) %>%
      arrange(desc(edf))
   
   cat("=== SMOOTH TERMS (sorted by complexity) ===\n")
   cat("(edf = effective degrees of freedom)\n\n")
   print(smooth_info, n = Inf)
   cat("\n")
   
   # Significant terms
   sig_terms <- smooth_info %>%
      filter(p_value < 0.05)
   
   cat("=== SIGNIFICANT SMOOTH TERMS (p < 0.05) ===\n")
   cat("Count:", nrow(sig_terms), "/", nrow(smooth_info), "\n")
   print(sig_terms %>% dplyr::select(term, edf, p_value))
   cat("\n")
   
   # ============================================================================
   # GET FOLD-LEVEL RESULTS WITH BEST K
   # ============================================================================
   
   cat("Computing fold-level metrics with best k...\n")
   fold_level_results <- map_dfr(seq_along(folds), function(j) {
      
      train_idx <- folds[[j]]$train
      test_idx <- folds[[j]]$test
      
      train_fold <- data_clean[train_idx, ] %>% dplyr::select(-DateTime, -Response_original)
      test_fold <- data_clean[test_idx, ]
      
      gam_fit <- suppressWarnings(
         bam(final_formula, 
             data = train_fold,
             family = gam_family,
             method = method,
             discrete = discrete,
             nthreads = nthreads)
      )
      
      preds <- predict(gam_fit, newdata = test_fold %>% dplyr::select(-DateTime, -Response_original), 
                       type = "response")
      
      # Back-transform if needed
      if (family_type == "gaussian" && transform_response == "log") {
         sigma_sq <- summary(gam_fit)$scale
         preds_original <- exp(preds + sigma_sq/2)
      } else if (family_type == "gaussian" && transform_response == "sqrt") {
         preds_original <- preds^2
      } else {
         preds_original <- preds
      }
      
      tibble(
         id = paste0("Fold", j),
         .metric = c("rmse", "rsq", "mae"),
         .estimate = c(
            sqrt(mean((test_fold$Response_original - preds_original)^2)),
            cor(test_fold$Response_original, preds_original)^2,
            mean(abs(test_fold$Response_original - preds_original))
         )
      )
   })
   
   cat("Done.\n\n")
   
   # ============================================================================
   # CREATE OUTPUT STRUCTURE
   # ============================================================================
   
   # Create tidymodels-compatible workflow structure
   gam_workflow <- structure(
      list(
         fit = list(
            fit = final_gam,
            formula = final_formula,
            family = family_type
         )
      ),
      class = c("workflow", "list")
   )
   
   # Return comprehensive results
   list(
      tune_results = fold_level_results,
      tune_grid = tune_results,
      best_params = tibble(
         k_flow = best_k$k_flow,
         k_physical = best_k$k_physical,
         k_temporal = best_k$k_temporal,
         k_interaction = best_k$k_interaction,
         family = family_type,
         link = link,
         transform = if(family_type == "gaussian") transform_response else "via_link"
      ),
      final_fit = gam_workflow,
      gam_object = final_gam,
      formula = final_formula,
      smooth_info = smooth_info,
      selected_vars = sig_terms$term,
      model_type = "gam",
      # Store transformation info for prediction
      transform_info = list(
         family = family_type,
         link = link,
         manual_transform = if(family_type == "gaussian") transform_response else "none",
         sigma_sq = if(family_type == "gaussian" && transform_response == "log") {
            summary(final_gam)$scale
         } else {
            NULL
         }
      )
   )
}