# 3-parameter Generalized Pareto helpers

dgpd3 <- function(x, shape, scale, location = 0, log = FALSE) {
   z <- (x - location) / scale
   out <- rep(if (log) -Inf else 0, length(z))
   valid <- z >= 0
   if (any(!valid)) out[!valid] <- if (log) NaN else NaN
   if (abs(shape) > 1e-6) {
      logdens <- -log(scale) - (1 / shape + 1) * log1p(shape * z[valid])
   } else {
      logdens <- -log(scale) - z[valid]
   }
   out[valid] <- if (log) logdens else exp(logdens)
   return(out)
}

pgpd3 <- function(q, shape, scale, location = 0, lower.tail = TRUE, log.p = FALSE) {
   z <- (q - location) / scale
   out <- rep(if (log.p) -Inf else 0, length(z))
   valid <- z >= 0
   if (abs(shape) > 1e-6) {
      prob <- 1 - (1 + shape * z[valid])^(-1 / shape)
   } else {
      prob <- 1 - exp(-z[valid])
   }
   if (!lower.tail) prob <- 1 - prob
   out[valid] <- if (log.p) log(prob) else prob
   out[!valid] <- if (log.p) NaN else NaN
   return(out)
}

qgpd3 <- function(p, shape, scale, location = 0) {
   if (abs(shape) > 1e-6) {
      location + scale * ((1 - p)^(-shape) - 1) / shape
   } else {
      location - scale * log(1 - p)
   }
}

# Your custom density function (returns log-density)
gpd3_loglik <- function(params, data) {
   shape <- params[1]
   scale <- params[2]
   location <- params[3]
   
   if (scale <= 0) return(1e10)
   if (any(data < location)) return(1e10)
   
   z <- (data - location) / scale
   
   # Domain check: 1 + shape * z > 0 for all z
   if (any(1 + shape * z <= 0)) return(1e10)
   
   if (abs(shape) > 1e-6) {
      logdens <- -log(scale) - (1/shape + 1) * log1p(shape * z)
   } else {
      logdens <- -log(scale) - z
   }
   return(-sum(logdens))
}


# Fit with location fixed (e.g., to min(data))
fit_gpd3 <- function(data, location = min(data)) {
   neg_loglik <- function(par) gpd3_loglik(c(par[1], par[2], location), data)
   start <- c(shape = 0.1, scale = sd(data))
   fit <- optim(start, neg_loglik, method = "L-BFGS-B", lower = c(-5, 1e-6), upper = c(5, Inf))
   
   if (fit$convergence != 0) {
      warning("Optimization did not converge")
   }
   
   list(
      shape = fit$par[1],
      scale = fit$par[2],
      location = location,
      loglik = -fit$value,
      convergence = fit$convergence
   )
}


#-------------------------------
# 1. Fit distribution + calculate diagnostics
#-------------------------------
fit_distribution <- function(data, dist_name, weight_tail = FALSE, tail_quantile = 0.9, tail_weight = 15) {
   library(MASS)
   library(fitdistrplus)
   library(flexsurv)
   library(statmod)
   library(actuar)
   library(evir)
   
   n <- length(data)
   p <- ppoints(n)
   data_sorted <- sort(data)
   
   weights <- rep(1, n)
   if (weight_tail) {
      threshold <- quantile(data_sorted, tail_quantile)
      weights[data_sorted > threshold] <- tail_weight
   }
   
   q_theoretical <- d_theoretical <- loglik <- NULL
   params <- k <- NA

   
   if (dist_name == "normal") {
      mu <- weighted.mean(data_sorted, weights)
      sigma <- sqrt(weighted.mean((data_sorted - mu)^2, weights))
      q_theoretical <- qnorm(p, mean = mu, sd = sigma)
      d_theoretical <- dnorm(data_sorted, mean = mu, sd = sigma)
      loglik <- sum(weights * dnorm(data_sorted, mean = mu, sd = sigma, log = TRUE))
      params <- c(mu = mu, sd = sigma)
      k <- 2
      
   } else if (dist_name == "lognormal") {
      log_data <- log(data_sorted)
      meanlog <- weighted.mean(log_data, weights)
      sdlog <- sqrt(weighted.mean((log_data - meanlog)^2, weights))
      q_theoretical <- qlnorm(p, meanlog = meanlog, sdlog = sdlog)
      d_theoretical <- dlnorm(data_sorted, meanlog = meanlog, sdlog = sdlog)
      loglik <- sum(weights * dlnorm(data_sorted, meanlog = meanlog, sdlog = sdlog, log = TRUE))
      params <- c(meanlog = meanlog, sdlog = sdlog)
      k <- 2
      
   } else if (dist_name == "gamma") {
      m <- weighted.mean(data_sorted, weights)
      v <- weighted.mean((data_sorted - m)^2, weights)
      shape <- m^2 / v
      rate <- m / v
      q_theoretical <- qgamma(p, shape = shape, rate = rate)
      d_theoretical <- dgamma(data_sorted, shape = shape, rate = rate)
      loglik <- sum(weights * dgamma(data_sorted, shape = shape, rate = rate, log = TRUE))
      params <- c(shape = shape, rate = rate)
      k <- 2
      
   } else if (dist_name == "t") {
      fit <- fitdistr(data_sorted, densfun = "t")
      df <- fit$estimate["df"]
      m <- fit$estimate["m"]
      s <- fit$estimate["s"]
      q_theoretical <- qt(p, df = df) * s + m
      d_theoretical <- dt((data_sorted - m)/s, df = df) / s
      loglik <- sum(weights * (dt((data_sorted - m)/s, df = df, log = TRUE) - log(s)))
      params <- fit$estimate
      k <- 3
      
   } else if (dist_name == "weibull") {
      fit <- fitdistr(data_sorted, "weibull")
      shape <- fit$estimate["shape"]
      scale <- fit$estimate["scale"]
      q_theoretical <- qweibull(p, shape = shape, scale = scale)
      d_theoretical <- dweibull(data_sorted, shape = shape, scale = scale)
      loglik <- sum(weights * dweibull(data_sorted, shape = shape, scale = scale, log = TRUE))
      params <- fit$estimate
      k <- 2
      
   } else if (dist_name == "invgauss") {
      mu <- weighted.mean(data_sorted, weights)
      lambda <- mu^3 / weighted.mean((data_sorted - mu)^2, weights)
      q_theoretical <- statmod::qinvgauss(p, mean = mu, shape = lambda)
      d_theoretical <- statmod::dinvgauss(data_sorted, mean = mu, shape = lambda)
      loglik <- sum(weights * statmod::dinvgauss(data_sorted, mean = mu, shape = lambda, log = TRUE))
      params <- c(mu = mu, shape = lambda)
      k <- 2
      
   } else if (dist_name == "gengamma") {
      fit <- flexsurv::flexsurvreg(Surv(data_sorted) ~ 1, dist = "gengamma")
      params <- fit$res[, "est"]
      q_theoretical <- qgengamma(p, mu = params[1], sigma = params[2], Q = params[3])
      d_theoretical <- dgengamma(data_sorted, mu = params[1], sigma = params[2], Q = params[3])
      loglik <- sum(weights * dgengamma(data_sorted, mu = params[1], sigma = params[2], Q = params[3], log = TRUE))
      k <- 3
      
   } else if (dist_name == "loglogistic") {
      fit <- flexsurv::flexsurvreg(Surv(data_sorted) ~ 1, dist = "llogis")
      shape <- fit$res[1, "est"]
      scale <- fit$res[2, "est"]
      q_theoretical <- qllogis(p, shape = shape, scale = scale)
      d_theoretical <- dllogis(data_sorted, shape = shape, scale = scale)
      loglik <- sum(weights * dllogis(data_sorted, shape = shape, scale = scale, log = TRUE))
      params <- c(shape = shape, scale = scale)
      k <- 2
      
   } else if (dist_name == "burr") {
      fit <- fitdist(data_sorted, "burr", start = list(shape1 = 1, shape2 = 1, rate = 1))
      shape1 <- fit$estimate["shape1"]
      shape2 <- fit$estimate["shape2"]
      rate <- fit$estimate["rate"]
      q_theoretical <- qburr(p, shape1 = shape1, shape2 = shape2, rate = rate)
      d_theoretical <- dburr(data_sorted, shape1 = shape1, shape2 = shape2, rate = rate)
      loglik <- sum(weights * dburr(data_sorted, shape1 = shape1, shape2 = shape2, rate = rate, log = TRUE))
      params <- fit$estimate
      k <- 3
      
   } else if (dist_name == "gpd") {
      # Fit 3-parameter GPD with location fixed at min(data)
      location <- min(data_sorted)
      fit <- fit_gpd3(data_sorted, location)
      
      shape <- fit$shape
      scale <- fit$scale
      
      q_theoretical <- qgpd3(p, shape = shape, scale = scale, location = location)
      d_theoretical <- dgpd3(data_sorted, shape = shape, scale = scale, location = location)
      loglik <- fit$loglik
      params <- c(shape = shape, scale = scale, location = location)
      k <- 3
   } else {
      stop("Unsupported or not yet implemented distribution.")
   }
   
   aic <- 2 * k - 2 * loglik
   
   list(
      dist = dist_name,
      q = q_theoretical,
      d = d_theoretical,
      data_sorted = data_sorted,
      loglik = loglik,
      aic = aic,
      params = params
   )
}

   
   

#-------------------------------------------
# Faceted Q-Q plot for multiple distributions
#-------------------------------------------
make_faceted_qq_plot <- function(fit_results) {
   qq_df <- do.call(rbind, lapply(fit_results, function(fr) {
      data.frame(
         Sample = fr$data_sorted,
         Theoretical = fr$q,
         Distribution = toupper(fr$dist)
      )
   }))
   
   ggplot(qq_df, aes(x = Theoretical, y = Sample)) +
      geom_point(alpha = 0.4, color = "black", size = 1.1) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      facet_wrap(~ Distribution) +
      labs(
         title = "Q-Q Plots for Fitted Distributions",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles"
      ) +
      theme_minimal(base_size = 14)
}

#-------------------------------
# 3. Density Overlay Function
#-------------------------------
make_density_plot <- function(fit_results, original_data) {
   plot_data <- do.call(rbind, lapply(fit_results, function(fr) {
      data.frame(
         x = fr$data_sorted,
         density = fr$d,
         dist = fr$dist
      )
   }))
   
   ggplot() +
      geom_histogram(aes(x = original_data, y = ..density..),
                     bins = 60, fill = "gray80", color = "black", alpha = 0.6) +
      geom_line(data = plot_data, aes(x = x, y = density, color = dist), size = 1.2) +
      labs(title = "Fitted Distributions", x = "Salinity", y = "Density") +
      theme_minimal() 
      # scale_color_manual(values = c("normal" = "blue", "lognormal" = "red",
      #                               "gamma" = "darkgreen", "t" = "purple"))
}

# ggplot() + 
#    geom_histogram(aes(x = sal), bins = 60, fill = 'gray80', color = 'black') + theme_minimal() + 
#    labs(x = 'Salinity (psu)', y = 'Count', title = 'Histogram of observed salinity data @ HdG')
#-------------------------------
# 4. AIC Comparison Table
#-------------------------------
compare_aic <- function(fit_results) {
   aic_table <- data.frame(
      Distribution = sapply(fit_results, function(x) x$dist),
      AIC = sapply(fit_results, function(x) x$aic)
   )
   aic_table[order(aic_table$AIC), ]
}

#-------------------------------
# 5. High Tail RMSE Table
#-------------------------------
# Computes RMSE for the top X% of the distribution
tail_rmse <- function(fit_result, tail_quantile = 0.9) {
   n <- length(fit_result$data_sorted)
   cutoff <- ceiling(n * tail_quantile)
   
   empirical <- fit_result$data_sorted[cutoff:n]
   theoretical <- fit_result$q[cutoff:n]
   
   sqrt(mean((empirical - theoretical)^2))
}

#-------------------------------
# 6. Make High Tail Q-Q Plot
#-------------------------------
make_tail_qq_plot <- function(fit_results, tail_quantile = 0.9) {
   tail_qq_df <- do.call(rbind, lapply(fit_results, function(fr) {
      n <- length(fr$data_sorted)
      cutoff <- ceiling(n * tail_quantile)
      data.frame(
         Sample = fr$data_sorted[cutoff:n],
         Theoretical = fr$q[cutoff:n],
         Distribution = toupper(fr$dist)
      )
   }))
   
   ggplot(tail_qq_df, aes(x = Theoretical, y = Sample)) +
      geom_point(alpha = 0.5, color = "black", size = 1.1) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      facet_wrap(~ Distribution, scales = "free") +
      labs(
         title = paste0("Upper ", 100 * (1 - tail_quantile), "% Tail Q-Q Plots"),
         x = "Theoretical Quantiles",
         y = "Empirical Quantiles"
      ) +
      theme_minimal(base_size = 14)
}

#-------------------------------
# 6. Make High Tail Density Plot
#-------------------------------
make_tail_density_plot <- function(fit_results, original_data, tail_quantile = 0.9) {
   cutoff <- quantile(original_data, probs = tail_quantile)
   filtered_data <- original_data[original_data >= cutoff]
   
   density_df <- do.call(rbind, lapply(fit_results, function(fr) {
      data.frame(
         x = fr$data_sorted[fr$data_sorted >= cutoff],
         density = fr$d[fr$data_sorted >= cutoff],
         dist = toupper(fr$dist)
      )
   }))
   
   ggplot() +
      geom_histogram(aes(x = filtered_data, y = ..density..),
                     bins = 40, fill = "gray80", color = "black", alpha = 0.6) +
      geom_line(data = density_df, aes(x = x, y = density, color = dist), size = 1.2) +
      labs(title = paste0("Upper ", 100 * (1 - tail_quantile), "% Tail Density Overlay"),
           x = "Salinity", y = "Density") +
      theme_minimal(base_size = 14)
}




# Load and clean data
data <- read.csv('Data/Tidied/Final/CleanFinalModelData.csv')
sal <- na.omit(data$Salinity)
sal <- sal[sal > 0]

# Fit distributions
dists_to_test <- c("normal", "lognormal", "gengamma", "loglogistic", "burr", 'gpd')
fits <- lapply(dists_to_test, function(d) fit_distribution(sal, d, weight_tail = TRUE, tail_quantile = 0.9, tail_weight = 10))
names(fits) <- dists_to_test

# Plot density overlays
make_density_plot(fits, sal)

# Plot Q-Q plots
# Show all Q-Q plots in a single faceted plot
make_faceted_qq_plot(fits)

# Print AIC table
compare_aic(fits)

tail_rmse_results <- sapply(fits, tail_rmse, tail_quantile = 0.9)
sort(tail_rmse_results)


# After initial screening
make_tail_qq_plot(fits[c("burr", "gengamma", "loglogistic", 'gpd')], tail_quantile = 0.9)
make_tail_density_plot(fits[c("burr", "gengamma", "loglogistic", "t")], sal, tail_quantile = 0.9)





