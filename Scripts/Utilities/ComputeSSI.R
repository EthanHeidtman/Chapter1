# This function takes discharge data and fits a specified distribution and computes the Standardized Streamflow Index

compute_ssi <- function(flow_vec, datetime = NULL, window_hours = 168, distribution = "gamma") {
   
   library(zoo)
   library(dplyr)
   
   # flow_vec: numeric vector of flow values (e.g., hourly)
   # datetime: optional POSIXct vector for rolling alignment (if NULL, assume equally spaced)
   # window_hours: rolling window size in number of points (hours)
   # distribution: "gamma" (default), "normal", or "lognormal"
   
   if (!is.numeric(flow_vec)) stop("flow_vec must be numeric")
   
   # Rolling mean using zoo::rollapply (align right)
   # If datetime given, create zoo object for rolling with proper index
   if (!is.null(datetime)) {
      flow_zoo <- zoo(flow_vec, order.by = datetime)
      rolling_flow <- rollapply(flow_zoo, width = window_hours, FUN = mean, align = "right", fill = NA)
      rolling_vals <- coredata(rolling_flow)
   } else {
      rolling_vals <- zoo::rollapply(flow_vec, width = window_hours, FUN = mean, align = "right", fill = NA)
   }
   
   # Filter valid positive values for fitting (some dists need positive only)
   valid_vals <- rolling_vals[!is.na(rolling_vals)]
   if (distribution %in% c("gamma", "lognormal")) {
      valid_vals <- valid_vals[valid_vals > 0]
   }
   
   if (length(valid_vals) < 30) {
      warning("Too few valid values for fitting. Returning NA vector.")
      return(rep(NA_real_, length(rolling_vals)))
   }
   
   # Fit parameters and calculate CDF depending on distribution
   cdf_vals <- rep(NA_real_, length(rolling_vals))
   
   if (distribution == "gamma") {
      xbar <- mean(valid_vals)
      s2 <- var(valid_vals)
      shape <- xbar^2 / s2
      rate <- xbar / s2
      cdf_vals <- pgamma(rolling_vals, shape = shape, rate = rate)
   } else if (distribution == "normal") {
      mu <- mean(valid_vals)
      sigma <- sd(valid_vals)
      cdf_vals <- pnorm(rolling_vals, mean = mu, sd = sigma)
   } else if (distribution == "lognormal") {
      # fit on log scale
      log_vals <- log(valid_vals)
      mu <- mean(log_vals)
      sigma <- sd(log_vals)
      cdf_vals <- plnorm(rolling_vals, meanlog = mu, sdlog = sigma)
   } else {
      stop("Unsupported distribution. Choose 'gamma', 'normal', or 'lognormal'.")
   }
   
   # Bound CDF away from 0 and 1 for qnorm stability
   cdf_bounded <- pmin(pmax(cdf_vals, 1e-6), 1 - 1e-6)
   ssi <- qnorm(cdf_bounded)
   
   return(ssi)
}
