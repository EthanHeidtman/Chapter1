# This function systematically identifies the best predictors for salinity prediction
# Returns best predictors from each category and their interactions for GAM building

linear_predictor_selector <- function(data, salinity_threshold, predictor_config, performance_criteria) {
   
   cat("STARTING SYSTEMATIC PREDICTOR TESTING PROCESS\n")
   cat("=============================================\n")
   
   # Initialize results storage
   stage_results <- list()
   
   # Stage 0: Test tide predictors
   cat("Stage 0: Testing tide predictors...\n")
   stage_results$tide <- test_predictor_group('Salinity ~', predictor_config$tide, data, 'tide')
   
   # Stage 1: Test discharge lag predictors
   cat("\nStage 1: Testing discharge lag predictors...\n")
   stage_results$discharge_lag <- test_predictor_group('Salinity ~', predictor_config$discharge_lag, data, "discharge_lag")
   
   # Stage 2: Test discharge rolling predictors
   cat("\nStage 2: Testing discharge rolling predictors...\n")
   stage_results$discharge_rolling <- test_predictor_group('Salinity ~', predictor_config$discharge_rolling, data, "discharge_rolling")
   
   # Stage 3: Test lagged inflow predictors
   cat("\nStage 3: Testing inflow lag predictors...\n")
   stage_results$inflow_lag <- test_predictor_group('Salinity ~', predictor_config$inflow_lag, data, "inflow_lag")
   
   # Stage 4: Test rolling inflow predictors
   cat("\nStage 4: Testing inflow rolling predictors...\n")
   stage_results$inflow_rolling <- test_predictor_group('Salinity ~', predictor_config$inflow_rolling, data, "inflow_rolling")
   
   # Stage 5: Test continuous stress predictors
   cat("\nStage 5: Testing continuous stress predictors...\n")
   stage_results$stress_continuous <- test_predictor_group('Salinity ~', predictor_config$stress_continuous, data, "stress_continuous")
   
   # Stage 6: Test temporal predictors
   cat("\nStage 6: Testing temporal predictors...\n")
   stage_results$temporal <- test_predictor_group('Salinity ~', predictor_config$temporal, data, "temporal")
   
   # Summary
   cat("\n=============================================\n")
   cat("PREDICTOR TESTING PROCESS COMPLETED\n")
   cat("=============================================\n")
   
   cat("TESTED PREDICTOR CATEGORIES:\n")
   for (category in names(stage_results)) {
      n_predictors <- nrow(stage_results[[category]]$summary_table)
      cat(sprintf("  %s: %d predictors tested\n", category, n_predictors))
   }
   
   # Return results for analysis
   results <- list(
      stage_results = stage_results,
      summary = list(
         categories_tested = names(stage_results),
         total_categories = length(stage_results),
         selection_method = "manual_evaluation"
      )
   )
   
   cat("=============================================\n")
   
   return(results)
}

# Helper function to check correlations among selected predictors
check_predictor_correlations <- function(data, selected_predictors, target = "Salinity", threshold = 0.2) {
   library(ggplot2)
   library(reshape2)
   library(energy)   # for dcor
   
   # Check predictors exist in data
   missing_preds <- selected_predictors[!selected_predictors %in% names(data)]
   if(length(missing_preds) > 0) {
      cat(sprintf("Warning: These predictors not found in data: %s\n", paste(missing_preds, collapse = ", ")))
      selected_predictors <- selected_predictors[selected_predictors %in% names(data)]
   }
   
   if(!(target %in% names(data))) {
      stop(sprintf("Target variable '%s' not found in data!", target))
   }
   
   if(length(selected_predictors) < 1) {
      cat("Need at least 1 predictor for dependence check\n")
      return(invisible(NULL))
   }
   
   dcor_vals <- numeric(length(selected_predictors))
   
   for(i in seq_along(selected_predictors)) {
      pred <- selected_predictors[i]
      x <- data[[pred]]
      y <- data[[target]]
      
      complete_idx <- complete.cases(x, y)
      x <- x[complete_idx]
      y <- y[complete_idx]
      
      dcor_vals[i] <- dcor(x, y)
   }
   
   results <- data.frame(
      Predictor = selected_predictors,
      DistanceCorrelation = dcor_vals
   )
   
   strong_dcor <- results[results$DistanceCorrelation > threshold, ]
   
   cat(sprintf("DISTANCE CORRELATION CHECK (threshold = %.2f):\n", threshold))
   cat("=====================================\n")
   
   if(nrow(strong_dcor) > 0) {
      cat("Strong distance correlations with target found:\n")
      for(i in 1:nrow(strong_dcor)) {
         cat(sprintf("  %s: dCor = %.3f\n", strong_dcor$Predictor[i], strong_dcor$DistanceCorrelation[i]))
      }
   } else {
      cat("No strong distance correlations found.\n")
   }
   
   cat("=====================================\n")
   
   # Plot heatmap
   p <- ggplot(results, aes(x = Predictor, y = 1, fill = DistanceCorrelation)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      geom_text(aes(label = sprintf("%.2f", DistanceCorrelation)), color = "black", size = 3) +
      labs(title = "Distance Correlation of Predictors with Target",
           fill = "Distance Correlation")
   
   print(p)
   
   return(invisible(results))
}

check_predictor_ccf <- function(data, predictors, target, max_lag = 48, threshold = 0.2) {
   library(ggplot2)
   library(reshape2)
   
   # Check predictors and target exist
   missing_preds <- predictors[!predictors %in% names(data)]
   if(length(missing_preds) > 0) {
      cat(sprintf("Warning: These predictors not found in data: %s\n", paste(missing_preds, collapse = ", ")))
      predictors <- predictors[predictors %in% names(data)]
   }
   if(!(target %in% names(data))) {
      stop(sprintf("Target variable '%s' not found in data.", target))
   }
   
   # Initialize results storage
   ccf_results <- data.frame(Predictor = character(),
                             MaxAbsCCF = numeric(),
                             LagAtMax = integer(),
                             CCFValue = numeric(),
                             stringsAsFactors = FALSE)
   
   # Loop through predictors
   for(pred in predictors) {
      # Extract time series, removing NA pairs
      complete_idx <- complete.cases(data[[pred]], data[[target]])
      x <- data[[pred]][complete_idx]
      y <- data[[target]][complete_idx]
      
      # Compute cross-correlation function (ccf), no plot
      ccf_obj <- ccf(x, y, lag.max = max_lag, plot = FALSE)
      
      # Find max absolute ccf value and corresponding lag
      abs_ccf <- abs(ccf_obj$acf)
      max_idx <- which.max(abs_ccf)
      max_ccf_val <- ccf_obj$acf[max_idx]
      lag_at_max <- ccf_obj$lag[max_idx]
      
      ccf_results <- rbind(ccf_results,
                           data.frame(Predictor = pred,
                                      MaxAbsCCF = abs(max_ccf_val),
                                      LagAtMax = lag_at_max,
                                      CCFValue = max_ccf_val))
   }
   
   # Filter by threshold
   strong_ccf <- ccf_results[ccf_results$MaxAbsCCF >= threshold, ]
   
   cat(sprintf("CROSS-CORRELATION CHECK (threshold = %.2f):\n", threshold))
   cat("=====================================\n")
   
   if(nrow(strong_ccf) > 0) {
      cat("Strong cross-correlations with target found:\n")
      for(i in 1:nrow(strong_ccf)) {
         row <- strong_ccf[i, ]
         cat(sprintf("  %s: max |CCF|=%.3f at lag %d (CCF=%.3f)\n",
                     row$Predictor, row$MaxAbsCCF, row$LagAtMax, row$CCFValue))
      }
   } else {
      cat("No strong cross-correlations found above threshold.\n")
   }
   cat("=====================================\n")
   
   # Plot heatmap of max CCFs with lag on y-axis
   if(nrow(strong_ccf) > 0) {
      p <- ggplot(strong_ccf, aes(x = Predictor, y = LagAtMax, fill = CCFValue)) +
         geom_tile(color = "white") +
         scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                              limit = c(-1, 1), name = "CCF Value") +
         theme_minimal() +
         labs(title = paste("Max Cross-Correlation with", target),
              y = "Lag at Max CCF", x = "Predictor") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
   }
   
   return(invisible(ccf_results))
}
