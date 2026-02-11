calculate_performance_metrics <- function(data, lead_times, salinity_threshold = NULL) {
   
   nse <- function(obs, pred) {
      1 - sum((obs - pred)^2, na.rm = TRUE) /
         sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
   }
   
   metrics_list <- list()
   
   for (k in lead_times) {
      pred_col <- paste0(k, 'DayForecast')
      
      if (!pred_col %in% names(data)) {
         warning(sprintf("Prediction column %s not found, skipping", pred_col))
         next
      }
      
      # Overall metrics
      overall <- data %>%
         filter(!is.na(.data[[pred_col]]) & !is.na(Salinity)) %>%
         summarise(
            LeadTime = k,
            Subset = "Overall",
            RMSE = sqrt(mean((Salinity - .data[[pred_col]])^2)),
            MAE  = mean(abs(Salinity - .data[[pred_col]])),
            R2   = cor(Salinity, .data[[pred_col]])^2,
            NSE  = nse(Salinity, .data[[pred_col]]),
            Bias = mean(.data[[pred_col]] - Salinity),
            N    = n()
         )
      
      metrics_list[[paste0("overall_lag", k)]] <- overall
      
      # High salinity metrics
      if (!is.null(salinity_threshold)) {
         
         high_sal <- data %>%
            filter(!is.na(.data[[pred_col]]) & !is.na(Salinity)) %>%
            filter(Salinity >= salinity_threshold) %>%
            summarise(
               LeadTime = k,
               Subset = paste0("Salinity >= ", salinity_threshold),
               RMSE = sqrt(mean((Salinity - .data[[pred_col]])^2)),
               MAE  = mean(abs(Salinity - .data[[pred_col]])),
               R2   = cor(Salinity, .data[[pred_col]])^2,
               NSE  = nse(Salinity, .data[[pred_col]]),
               Bias = mean(.data[[pred_col]] - Salinity),
               N    = n()
            )
         
         metrics_list[[paste0("high_sal_lag", k)]] <- high_sal
      }
   }
   
   bind_rows(metrics_list)
}