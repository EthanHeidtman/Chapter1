library(dplyr)
library(ggplot2)
library(mgcv)
library(MASS)  # for fitdistr
library(gridExtra)

# Function to diagnose appropriate distribution family
diagnose_distribution <- function(data, response = "Salinity") {
   
   y <- data[[response]]
   
   cat("=== DISTRIBUTION DIAGNOSTICS ===\n\n")
   
   # Basic statistics
   cat("--- Basic Statistics ---\n")
   cat("N:", length(y), "\n")
   cat("Min:", min(y), "\n")
   cat("Max:", max(y), "\n")
   cat("Mean:", round(mean(y), 4), "\n")
   cat("Median:", round(median(y), 4), "\n")
   cat("SD:", round(sd(y), 4), "\n")
   cat("CV (SD/Mean):", round(sd(y)/mean(y), 4), "\n")
   cat("Skewness:", round(moments::skewness(y), 4), "\n")
   cat("Kurtosis:", round(moments::kurtosis(y), 4), "\n\n")
   
   # Check for zeros
   n_zeros <- sum(y == 0)
   n_near_zero <- sum(y < 0.001)
   cat("--- Zero/Near-Zero Check ---\n")
   cat("Exact zeros:", n_zeros, "(", round(100*n_zeros/length(y), 2), "%)\n")
   cat("Values < 0.001:", n_near_zero, "(", round(100*n_near_zero/length(y), 2), "%)\n")
   cat("Minimum positive value:", min(y[y > 0]), "\n\n")
   
   # Quantiles
   cat("--- Quantiles ---\n")
   probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
   quants <- quantile(y, probs)
   for(i in seq_along(probs)) {
      cat(sprintf("%3d%%: %.4f\n", probs[i]*100, quants[i]))
   }
   cat("\n")
   
   # Variance-mean relationship
   cat("--- Variance-Mean Relationship ---\n")
   cat("For Gaussian: Var should be constant\n")
   cat("For Poisson: Var = Mean\n")
   cat("For Gamma: Var = Mean^2 / shape\n")
   cat("For Tweedie: Var = phi * Mean^p\n\n")
   
   cat("Observed:\n")
   cat("  Variance:", round(var(y), 6), "\n")
   cat("  Mean:", round(mean(y), 6), "\n")
   cat("  Var/Mean:", round(var(y)/mean(y), 4), "\n")
   cat("  Var/Mean^2:", round(var(y)/mean(y)^2, 4), "\n\n")
   
   # Fit candidate distributions
   cat("--- Fitting Distributions ---\n")
   
   # Remove zeros if present for distributions that require positive values
   y_pos <- y[y > 0]
   
   # Gaussian
   fit_gauss <- list(mean = mean(y), sd = sd(y))
   aic_gauss <- -2 * sum(dnorm(y, fit_gauss$mean, fit_gauss$sd, log = TRUE)) + 2*2
   
   # Log-normal (on positive values)
   if(length(y_pos) > 0) {
      fit_lnorm <- fitdistr(y_pos, "lognormal")
      aic_lnorm <- fit_lnorm$loglik * -2 + 2*2
   } else {
      aic_lnorm <- NA
   }
   
   # Gamma (on positive values)
   if(length(y_pos) > 0 && min(y_pos) > 0) {
      fit_gamma <- tryCatch({
         fitdistr(y_pos, "gamma")
      }, error = function(e) NULL)
      
      if(!is.null(fit_gamma)) {
         aic_gamma <- fit_gamma$loglik * -2 + 2*2
      } else {
         aic_gamma <- NA
      }
   } else {
      aic_gamma <- NA
   }
   
   cat("\nDistribution AICs (lower is better):\n")
   cat("  Gaussian:", round(aic_gauss, 2), "\n")
   if(!is.na(aic_lnorm)) cat("  Log-normal:", round(aic_lnorm, 2), "\n")
   if(!is.na(aic_gamma)) cat("  Gamma:", round(aic_gamma, 2), "\n")
   cat("\n")
   
   # Create diagnostic plots
   par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
   
   # 1. Histogram
   hist(y, breaks = 50, freq = FALSE, main = "Histogram", 
        xlab = response, col = "lightblue")
   
   # Add fitted densities
   x_seq <- seq(min(y), max(y), length.out = 200)
   lines(x_seq, dnorm(x_seq, fit_gauss$mean, fit_gauss$sd), 
         col = "blue", lwd = 2)
   if(!is.na(aic_lnorm)) {
      lines(x_seq[x_seq > 0], 
            dlnorm(x_seq[x_seq > 0], fit_lnorm$estimate[1], fit_lnorm$estimate[2]), 
            col = "red", lwd = 2)
   }
   if(!is.na(aic_gamma)) {
      lines(x_seq[x_seq > 0], 
            dgamma(x_seq[x_seq > 0], fit_gamma$estimate[1], fit_gamma$estimate[2]), 
            col = "darkgreen", lwd = 2)
   }
   legend("topright", c("Gaussian", "Log-normal", "Gamma"), 
          col = c("blue", "red", "darkgreen"), lwd = 2, cex = 0.7)
   
   # 2. Log histogram (to see tail better)
   hist(y, breaks = 50, freq = FALSE, main = "Histogram (log scale)", 
        xlab = response, col = "lightblue", log = "y")
   
   # 3. QQ plot - Gaussian
   qqnorm(y, main = "Normal Q-Q")
   qqline(y, col = "blue", lwd = 2)
   
   # 4. QQ plot - Log-normal
   if(!is.na(aic_lnorm)) {
      qqplot(qlnorm(ppoints(length(y_pos)), fit_lnorm$estimate[1], fit_lnorm$estimate[2]),
             y_pos, main = "Log-normal Q-Q",
             xlab = "Theoretical", ylab = "Observed")
      abline(0, 1, col = "red", lwd = 2)
   } else {
      plot.new()
      text(0.5, 0.5, "Log-normal N/A", cex = 1.5)
   }
   
   # 5. QQ plot - Gamma
   if(!is.na(aic_gamma)) {
      qqplot(qgamma(ppoints(length(y_pos)), fit_gamma$estimate[1], fit_gamma$estimate[2]),
             y_pos, main = "Gamma Q-Q",
             xlab = "Theoretical", ylab = "Observed")
      abline(0, 1, col = "darkgreen", lwd = 2)
   } else {
      plot.new()
      text(0.5, 0.5, "Gamma N/A", cex = 1.5)
   }
   
   # 6. Boxplot
   boxplot(y, main = "Boxplot", ylab = response, col = "lightblue")
   
   # 7. Violin plot approximation with density
   plot(density(y), main = "Density Plot", xlab = response, lwd = 2)
   
   # 8. ECDF
   plot(ecdf(y), main = "Empirical CDF", xlab = response, 
        ylab = "Cumulative Probability")
   
   # 9. Residual plot if fitting simple mean model
   residuals <- y - mean(y)
   plot(residuals, main = "Residuals from Mean", 
        ylab = "Residual", xlab = "Index", pch = ".")
   abline(h = 0, col = "red", lwd = 2)
   
   par(mfrow = c(1, 1))
   
   # Recommendations
   cat("\n=== RECOMMENDATIONS ===\n\n")
   
   # Check for point mass at minimum
   min_count <- sum(y == min(y))
   if(min_count > length(y) * 0.01) {
      cat("WARNING: ", min_count, " observations (", 
          round(100*min_count/length(y), 1), 
          "%) at minimum value.\n")
      cat("  This suggests a measurement floor or added constant.\n")
      cat("  Consider: original data, zero-inflated models, or hurdle models.\n\n")
   }
   
   # Skewness check
   if(abs(moments::skewness(y)) > 1) {
      cat("Data is highly skewed (skewness = ", round(moments::skewness(y), 2), ")\n")
      cat("  Consider: Gamma, log-normal, or Tweedie families\n\n")
   }
   
   # Heavy tail check
   ratio_99_mean <- quantile(y, 0.99) / mean(y)
   if(ratio_99_mean > 5) {
      cat("Heavy right tail detected (99th percentile / mean = ", 
          round(ratio_99_mean, 2), ")\n")
      cat("  Gamma may underpredict extremes\n")
      cat("  Consider: Tweedie with p < 2, or quantile regression\n\n")
   }
   
   # Variance-mean relationship
   var_mean_ratio <- var(y) / mean(y)^2
   if(var_mean_ratio < 0.5) {
      cat("Low variance relative to mean-squared (Var/Mean^2 = ", 
          round(var_mean_ratio, 3), ")\n")
      cat("  Gamma assumes Var = Mean^2/shape, so shape would be > 2\n")
      cat("  This is plausible for Gamma\n\n")
   } else if(var_mean_ratio > 2) {
      cat("High variance relative to mean-squared (Var/Mean^2 = ", 
          round(var_mean_ratio, 3), ")\n")
      cat("  Suggests overdispersion beyond Gamma\n")
      cat("  Consider: Tweedie or negative binomial (if counts)\n\n")
   }
   
   cat("SUGGESTED FAMILIES TO TEST (in order):\n")
   cat("1. Gaussian with identity link (baseline)\n")
   cat("2. Gaussian with log-transformed response (simple, handles skew)\n")
   if(!is.na(aic_lnorm) && !is.na(aic_gamma)) {
      if(aic_lnorm < aic_gamma) {
         cat("3. Gaussian family, log(Y) response - log-normal assumption\n")
         cat("4. Gamma with log link (if no zeros)\n")
         cat("5. Tweedie with p = 1.5-1.8 (allows overdispersion)\n")
      } else {
         cat("3. Gamma with log link (if no zeros)\n")
         cat("4. Gaussian family, log(Y) response - log-normal assumption\n")
         cat("5. Tweedie with p = 1.5-1.8 (allows overdispersion)\n")
      }
   }
   cat("6. Quantile regression (qgam) for 75th or 90th percentile\n")
   cat("7. Location-scale model (gaulss) if variance changes with mean\n\n")
   
   # Return fitted parameters for further use
   invisible(list(
      gaussian = fit_gauss,
      lognormal = if(!is.na(aic_lnorm)) fit_lnorm else NULL,
      gamma = if(!is.na(aic_gamma)) fit_gamma else NULL,
      aics = c(gaussian = aic_gauss, lognormal = aic_lnorm, gamma = aic_gamma),
      summary_stats = list(
         n = length(y),
         mean = mean(y),
         sd = sd(y),
         cv = sd(y)/mean(y),
         skew = moments::skewness(y),
         var_mean2 = var(y)/mean(y)^2
      )
   ))
}

# Example usage:
results <- diagnose_distribution(model_data, response = "Salinity")
