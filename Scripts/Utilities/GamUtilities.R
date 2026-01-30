create_ar_start <- function(datetime_vec, max_gap_hours = NULL, max_gap_days = NULL) {
   
   is_posix <- inherits(datetime_vec, "POSIXct")
   is_date <- inherits(datetime_vec, "Date")
   
   if (!is_posix && !is_date) {
      stop("datetime_vec must be POSIXct or Date")
   }
   
   # Set defaults based on input type
   if (is_posix && is.null(max_gap_hours)) {
      max_gap_hours <- 2
   }
   if (is_date && is.null(max_gap_days)) {
      max_gap_days <- 1
   }
   
   if (!all(diff(datetime_vec) > 0)) {
      stop("Datetimes must be sorted in increasing order")
   }
   
   if (is_posix) {
      if (is.null(max_gap_hours)) {
         stop("max_gap_hours must be specified for POSIXct input")
      }
      time_diffs <- c(Inf, as.numeric(diff(datetime_vec), units = "hours"))
      threshold <- max_gap_hours
   } else {
      if (is.null(max_gap_days)) {
         stop("max_gap_days must be specified for Date input")
      }
      time_diffs <- c(Inf, as.numeric(diff(datetime_vec)))
      threshold <- max_gap_days
   }
   
   ar_start <- time_diffs > threshold
   ar_start[1] <- TRUE
   
   return(ar_start)
}

# Function to calculate initial rho for autocorrelation 
calculate_rho_from_residuals <- function(model_object, ar_start) {
   
   residuals <- residuals(model_object)
   
   if (length(residuals) != length(ar_start)) {
      stop("Length mismatch: residuals (", length(residuals), 
           ") vs ar_start (", length(ar_start), ")")
   }
   
   breaks <- which(ar_start)
   n_segments <- length(breaks)
   
   rho_estimates <- numeric(n_segments)
   segment_lengths <- numeric(n_segments)
   
   
   for (i in 1:n_segments) {
      start_idx <- breaks[i]
      end_idx <- if (i < n_segments) breaks[i + 1] - 1 else length(residuals)
      
      segment <- residuals[start_idx:end_idx]
      n_obs <- length(segment)
      segment_lengths[i] <- n_obs
      
      if (n_obs >= 10) {
         # FIXED: Use proper indexing
         lagged <- segment[-length(segment)]  # y[1:(n-1)]
         current <- segment[-1]                # y[2:n]
         
         rho_estimates[i] <- cor(current, lagged, use = "complete.obs")
         
      } else {
         rho_estimates[i] <- NA
         if (i <= 5) {
            
         }
      }
   }
   
   # Remove invalid
   valid <- !is.na(rho_estimates) & is.finite(rho_estimates)
   
   
   # Weighted average
   weighted_rho <- weighted.mean(rho_estimates[valid], segment_lengths[valid])
   weighted_rho <- max(min(weighted_rho, 0.95), -0.95)
   
   # cat("\nRho statistics across", sum(valid), "valid segments:\n")
   # cat("  Min:    ", round(min(rho_estimates, na.rm = TRUE), 3), "\n")
   # cat("  Median: ", round(median(rho_estimates, na.rm = TRUE), 3), "\n")
   # cat("  Max:    ", round(max(rho_estimates, na.rm = TRUE), 3), "\n")
   # cat("  SD:     ", round(sd(rho_estimates, na.rm = TRUE), 3), "\n")
   # cat("  Weighted mean:", round(weighted_rho, 3), "\n\n")
   # 
   # Warning for high variation
   rho_sd <- sd(rho_estimates, na.rm = TRUE)
   if (rho_sd > 0.3) {
      warning("Large variation in rho across segments (SD = ", round(rho_sd, 3), ").\n",
              "AR(1) may not be appropriate for all segments.")
   }
   
   return(weighted_rho)
}
   
   
