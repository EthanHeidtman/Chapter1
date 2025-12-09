fit_gam <- function(data,
                    response = 'Salinity',
                    predictors = NULL,
                    folds = NULL,
                    
                    # Response transformation
                    transform_response = "none",  # "none", "log", "sqrt"
                    
                    # Distribution family
                    family_type = "gaussian",  # "gaussian", "Gamma", "Tweedie"
                    link = "identity",  # "identity", "log"; auto-set for some families
                    tweedie_p = 1.5,  # power parameter if using Tweedie
                    
                    # Smoothing parameters by variable type
                    k_flow_range = c(8, 15),
                    k_physical_range = c(6, 12),
                    k_temporal = 12,
                    k_interaction = 6,
                    
                    # Interactions
                    interactions = list(
                       list(vars = c('RollingInflows90', 'RollingDischarge48')),
                       list(vars = c('RollingDischarge48', 'RollingV168')),
                       list(vars = c('RollingDischarge48', 'TideRange24')),
                       list(vars = c('TideRange24', 'RollingV168'))
                    ),
                    
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
   
   # Set up family object
   gam_family <- switch(
      family_type,
      "gaussian" = gaussian(link = link),
      "Gamma" = Gamma(link = ifelse(link == "identity", "log", link)),
      "Tweedie" = Tweedie(p = tweedie_p, link = ifelse(link == "identity", "log", link)),
      stop("Unknown family_type. Use 'gaussian', 'Gamma', or 'Tweedie'")
   )
   
   # Prepare data
   data_clean <- data %>%
      mutate(Response = .data[[response]]) %>%
      dplyr::select(DateTime, Response, all_of(predictors)) %>%
      drop_na()
   
   # Store original response for metrics
   data_clean$Response_original <- data_clean$Response
   
   # Apply transformation if requested
   if (transform_response == "log") {
      if (any(data_clean$Response <= 0)) {
         stop("Cannot log-transform non-positive values. Check your data.")
      }
      data_clean$Response <- log(data_clean$Response)
      cat("Applied log transformation to response.\n")
      cat("Original range: [", round(min(data_clean$Response_original), 4), ", ",
          round(max(data_clean$Response_original), 4), "]\n")
      cat("Log-scale range: [", round(min(data_clean$Response), 4), ", ",
          round(max(data_clean$Response), 4), "]\n\n")
      
   } else if (transform_response == "sqrt") {
      if (any(data_clean$Response < 0)) {
         stop("Cannot sqrt-transform negative values. Check your data.")
      }
      data_clean$Response <- sqrt(data_clean$Response)
      cat("Applied sqrt transformation to response.\n\n")
   }
   
   # Check for zeros/negatives if using Gamma
   if (family_type == "Gamma") {
      n_zeros <- sum(data_clean$Response <= 0)
      if (n_zeros > 0) {
         cat("WARNING: Gamma family requires positive values.\n")
         cat("Found", n_zeros, "values <= 0. Adding small constant (0.001).\n\n")
         data_clean <- data_clean %>%
            mutate(Response = pmax(Response, 0.001))
      }
   }
   
   # Create weights
   if (use_weights) {
      data_clean <- data_clean %>%
         mutate(
            weight = case_when(
               weight_type == "threshold" & Response > weight_threshold ~ weight_multiplier,
               weight_type == "smooth" ~ 
                  1 + (weight_multiplier - 1) * plogis((Response - weight_threshold) / 0.1),
               weight_type == "exponential" ~ 
                  exp(pmax(0, (Response - weight_threshold) / 0.2)),
               TRUE ~ 1
            )
         )
      
      cat("Weight statistics:\n")
      cat("  Range:", round(min(data_clean$weight), 2), "to", 
          round(max(data_clean$weight), 2), "\n")
      cat("  Median:", round(median(data_clean$weight), 2), "\n")
      cat("  High salinity (>0.5) weight:", 
          round(mean(data_clean$weight[data_clean$Response > 0.5]), 2), "\n\n")
   } else {
      data_clean$weight <- 1
   }
   
   cat("=== GAM MODEL SETUP ===\n")
   cat("Sample size:", format(nrow(data_clean), big.mark = ","), "\n")
   cat("Predictors:", length(predictors), "\n")
   cat("Response transformation:", transform_response, "\n")
   cat("Family:", family_type, "with", gam_family$link, "link\n")
   if (family_type == "Tweedie") {
      cat("Tweedie power parameter:", tweedie_p, "\n")
   }
   cat("\n")
   
   # Classify predictors into groups
   flow_vars <- predictors[grepl("Discharge|Inflow", predictors, ignore.case = TRUE)]
   physical_vars <- predictors[grepl("Tide|RollingV|Wind", predictors, ignore.case = TRUE)]
   temporal_vars <- predictors[grepl("Sin|Cos", predictors, ignore.case = TRUE)]
   other_vars <- setdiff(predictors, c(flow_vars, physical_vars, temporal_vars))
   
   cat("Variable groups:\n")
   cat("  Flow (k =", k_flow_range[1], "-", k_flow_range[2], "):", 
       paste(flow_vars, collapse = ", "), "\n")
   cat("  Physical (k =", k_physical_range[1], "-", k_physical_range[2], "):", 
       paste(physical_vars, collapse = ", "), "\n")
   cat("  Temporal (k =", k_temporal, "):", 
       paste(temporal_vars, collapse = ", "), "\n")
   if (length(other_vars) > 0) {
      cat("  Other (k =", k_physical_range[1], "-", k_physical_range[2], "):", 
          paste(other_vars, collapse = ", "), "\n")
   }
   cat("\n")
   
   # Create tuning grid for k values
   k_grid <- expand.grid(
      k_flow = seq(k_flow_range[1], k_flow_range[2], length.out = gam_levels),
      k_physical = seq(k_physical_range[1], k_physical_range[2], length.out = gam_levels)
   ) %>%
      mutate(
         k_flow = round(k_flow),
         k_physical = round(k_physical)
      ) %>%
      distinct()
   
   cat("Tuning", nrow(k_grid), "k combinations\n")
   print(k_grid)
   cat("\n")
   
   # Function to build GAM formula
   build_gam_formula <- function(k_flow, k_physical) {
      
      terms <- c()
      
      # Flow variables
      if (length(flow_vars) > 0) {
         terms <- c(terms, paste0("s(", flow_vars, ", k=", k_flow, ", bs='", basis_default, "')"))
      }
      
      # Physical variables
      if (length(physical_vars) > 0) {
         terms <- c(terms, paste0("s(", physical_vars, ", k=", k_physical, ", bs='", basis_default, "')"))
      }
      
      # Temporal variables (cyclical)
      if (length(temporal_vars) > 0) {
         terms <- c(terms, paste0("s(", temporal_vars, ", k=", k_temporal, ", bs='", basis_cyclical, "')"))
      }
      
      # Other variables
      if (length(other_vars) > 0) {
         terms <- c(terms, paste0("s(", other_vars, ", k=", k_physical, ", bs='", basis_default, "')"))
      }
      
      # Add interactions (tensor products)
      if (length(interactions) > 0) {
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
   
   # Tune across k grid with CV
   cat("Running CV across k values...\n")
   tune_results <- map_dfr(1:nrow(k_grid), function(i) {
      
      k_flow <- k_grid$k_flow[i]
      k_physical <- k_grid$k_physical[i]
      
      cat("  k_flow =", k_flow, ", k_physical =", k_physical)
      
      formula <- build_gam_formula(k_flow, k_physical)
      
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
         
         # Predict (automatically on response scale)
         preds <- predict(gam_fit, newdata = test_fold, type = "response")
         
         # Back-transform predictions if needed
         if (transform_response == "log") {
            # Get residual variance for bias correction
            sigma_sq <- summary(gam_fit)$scale
            # Bias-corrected back-transformation
            preds_original <- exp(preds + sigma_sq/2)
         } else if (transform_response == "sqrt") {
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
      
      # Aggregate
      result <- fold_results %>%
         summarize(
            k_flow = k_flow,
            k_physical = k_physical,
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
   print(tune_results %>% arrange(mean_rmse))
   cat("\n")
   
   # Select best k values
   best_k <- tune_results %>%
      slice_min(mean_rmse, n = 1)
   
   cat("=== BEST K VALUES ===\n")
   cat("k_flow:", best_k$k_flow, "\n")
   cat("k_physical:", best_k$k_physical, "\n")
   cat("k_temporal:", k_temporal, "(fixed)\n")
   cat("k_interaction:", k_interaction, "(fixed)\n")
   cat("Mean CV RMSE:", round(best_k$mean_rmse, 4), "\n")
   cat("Mean CV R²:", round(best_k$mean_rsq, 4), "\n\n")
   
   # Fit final model with best k
   cat("Fitting final BAM with best k values...\n")
   final_formula <- build_gam_formula(best_k$k_flow, best_k$k_physical)
   
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
   
   # Model summary
   cat("=== FINAL MODEL SUMMARY ===\n")
   print(summary(final_gam))
   cat("\n")
   
   cat("Deviance explained:", round(summary(final_gam)$dev.expl * 100, 2), "%\n")
   cat("R-squared (adj):", round(summary(final_gam)$r.sq, 4), "\n")
   cat("AIC:", round(AIC(final_gam), 2), "\n\n")
   
   # Check basis dimensions
   cat("=== BASIS DIMENSION CHECK ===\n")
   cat("(If k-index < 1 and p < 0.05, increase k for that term)\n\n")
   k_check <- k.check(final_gam, n.rep = 0)
   print(k_check)
   cat("\n")
   
   # Extract smooth information
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
   
   # Get fold-level results with best k for compatibility with plotting
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
      
      # Back-transform predictions if needed
      if (transform_response == "log") {
         sigma_sq <- summary(gam_fit)$scale
         preds_original <- exp(preds + sigma_sq/2)
      } else if (transform_response == "sqrt") {
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
   
   # Return results
   list(
      tune_results = fold_level_results,
      tune_grid = tune_results,
      best_params = tibble(
         k_flow = best_k$k_flow,
         k_physical = best_k$k_physical,
         k_temporal = k_temporal,
         k_interaction = k_interaction,
         family = family_type,
         link = gam_family$link,
         transform = transform_response
      ),
      final_fit = gam_workflow,
      gam_object = final_gam,
      formula = final_formula,
      smooth_info = smooth_info,
      selected_vars = sig_terms$term,
      model_type = "gam",
      # Store transformation info for prediction
      transform_info = list(
         type = transform_response,
         sigma_sq = if(transform_response == "log") summary(final_gam)$scale else NULL
      )
   )
}