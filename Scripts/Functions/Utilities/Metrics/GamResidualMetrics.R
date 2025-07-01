# Function to calculate GAM-specific residual metrics
calculate_gam_residual_metrics <- function(model, observed, predicted) {
   if (!inherits(model, "gam")) {
      return(list())
   }
   
   residuals <- observed - predicted
   
   # Basic residual metrics
   metrics <- list(
      mean_residual = mean(residuals),
      median_residual = median(residuals),
      residual_sd = sd(residuals),
      residual_skewness = calculate_skewness(residuals),
      residual_kurtosis = calculate_kurtosis(residuals)
   )
   
   # Residual autocorrelation (important for time series)
   if (length(residuals) > 10) {
      acf_result <- tryCatch({
         acf(residuals, lag.max = min(20, length(residuals)/4), plot = FALSE)
      }, error = function(e) NULL)
      
      if (!is.null(acf_result)) {
         metrics$residual_autocorr_lag1 <- acf_result$acf[2]  # Lag 1 autocorrelation
         metrics$residual_autocorr_max <- max(abs(acf_result$acf[-1]))  # Max autocorr (excluding lag 0)
      }
   }
   
   return(metrics)
}

# Helper functions for residual analysis
calculate_skewness <- function(x) {
   if (length(x) < 3) return(NA)
   n <- length(x)
   mean_x <- mean(x)
   sd_x <- sd(x)
   if (sd_x == 0) return(0)
   
   skew <- (n / ((n-1) * (n-2))) * sum(((x - mean_x) / sd_x)^3)
   return(skew)
}

calculate_kurtosis <- function(x) {
   if (length(x) < 4) return(NA)
   n <- length(x)
   mean_x <- mean(x)
   sd_x <- sd(x)
   if (sd_x == 0) return(0)
   
   kurt <- (n * (n+1) / ((n-1) * (n-2) * (n-3))) * sum(((x - mean_x) / sd_x)^4) - 
      (3 * (n-1)^2 / ((n-2) * (n-3)))
   return(kurt)
}
