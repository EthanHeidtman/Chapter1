library(ggplot2)
library(mgcv)
library(gratia)
library(patchwork)
library(dplyr)
library(tidyr)

# Color palette
gam_colors <- list(
   primary = "#f58220",    # orange
   secondary = "#009bba",  # blue
   tertiary = "#fdb515",   # yellow
   dark = "#002030",       # dark blue
   threshold = "#002030"
)

#' Plot residual ACF for a GAM model
#' @param gam_object A fitted GAM object
#' @param max_lag Maximum lag for ACF (default 40)
#' @param title Plot title
plot_gam_acf <- function(gam_object, max_lag = 100, title = NULL) {
   
   resids <- residuals(gam_object, type = "deviance")
   acf_result <- acf(resids, lag.max = max_lag, plot = FALSE, na.action = na.pass)
   
   acf_df <- data.frame(
      lag = acf_result$lag[-1],
      acf = acf_result$acf[-1]
   )
   
   ci <- qnorm((1 + 0.95)/2) / sqrt(acf_result$n.used)
   
   ggplot(acf_df, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.8) +
      geom_hline(yintercept = c(-ci, ci), 
                 color = gam_colors$dark, 
                 linetype = "dashed", 
                 linewidth = 0.5) +
      geom_segment(aes(xend = lag, yend = 0), 
                   color = gam_colors$primary, 
                   linewidth = 0.8) +
      geom_point(color = gam_colors$primary, size = 2) +
      labs(
         title = title %||% "Residual Autocorrelation (ACF)",
         x = "Lag",
         y = "ACF"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
}

#' Plot residual PACF for a GAM model
#' @param gam_object A fitted GAM object
#' @param max_lag Maximum lag for PACF (default 40)
#' @param title Plot title
plot_gam_pacf <- function(gam_object, max_lag = 100, title = NULL) {
   
   resids <- residuals(gam_object, type = "deviance")
   pacf_result <- pacf(resids, lag.max = max_lag, plot = FALSE, na.action = na.pass)
   
   pacf_df <- data.frame(
      lag = pacf_result$lag,
      pacf = pacf_result$acf
   )
   
   ci <- qnorm((1 + 0.95)/2) / sqrt(pacf_result$n.used)
   
   ggplot(pacf_df, aes(x = lag, y = pacf)) +
      geom_hline(yintercept = 0, color = gam_colors$dark, linewidth = 0.8) +
      geom_hline(yintercept = c(-ci, ci), 
                 color = gam_colors$dark, 
                 linetype = "dashed", 
                 linewidth = 0.5) +
      geom_segment(aes(xend = lag, yend = 0), 
                   color = gam_colors$secondary, 
                   linewidth = 0.8) +
      geom_point(color = gam_colors$secondary, size = 2) +
      labs(
         title = title %||% "Partial Autocorrelation (PACF)",
         x = "Lag",
         y = "PACF"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
}

#' Plot QQ plot for GAM residuals
#' @param gam_object A fitted GAM object
#' @param title Plot title
plot_gam_qq <- function(gam_object, title = NULL) {
   
   resids <- residuals(gam_object, type = "deviance")
   
   # Create QQ data (same as qqnorm)
   n <- length(resids)
   theoretical <- qnorm(ppoints(n))
   observed <- sort(resids)
   
   # Calculate the qqline parameters (same as qqline)
   # qqline draws through the 1st and 3rd quartiles
   y <- quantile(resids, c(0.25, 0.75), na.rm = TRUE)
   x <- qnorm(c(0.25, 0.75))
   slope <- diff(y) / diff(x)
   intercept <- y[1] - slope * x[1]
   
   qq_df <- data.frame(
      theoretical = theoretical,
      observed = observed
   )
   
   ggplot(qq_df, aes(x = theoretical, y = observed)) +
      geom_abline(intercept = intercept, slope = slope, 
                  color = gam_colors$primary, 
                  linewidth = 0.8) +
      geom_point(color = gam_colors$dark, alpha = 0.6, size = 2) +
      labs(
         title = title %||% "Normal Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
}


#' Plot residuals vs fitted with quantile fan for heteroscedasticity
#' @param gam_object A fitted GAM object
#' @param n_bins Number of bins for quantile calculation (default 20)
#' @param title Plot title
plot_gam_resid_fitted <- function(gam_object, n_bins = 20, title = NULL) {
   
   fitted_vals <- fitted(gam_object)
   resids <- residuals(gam_object, type = "deviance")
   
   df <- data.frame(fitted = fitted_vals, resid = resids)
   
   # Create bins based on fitted values
   df$bin <- cut(df$fitted, breaks = n_bins, labels = FALSE)
   
   # Calculate quantiles within each bin
   quantile_df <- df %>%
      group_by(bin) %>%
      summarise(
         fitted_mid = mean(fitted, na.rm = TRUE),
         q05 = quantile(resid, 0.05, na.rm = TRUE),
         q25 = quantile(resid, 0.25, na.rm = TRUE),
         q50 = quantile(resid, 0.50, na.rm = TRUE),
         q75 = quantile(resid, 0.75, na.rm = TRUE),
         q95 = quantile(resid, 0.95, na.rm = TRUE),
         .groups = 'drop'
      )
   
   ggplot(df, aes(x = fitted, y = resid)) +
      # Quantile ribbons (fan)
      geom_ribbon(data = quantile_df, 
                  aes(x = fitted_mid, ymin = q05, ymax = q95),
                  fill = gam_colors$tertiary, alpha = 0.3, inherit.aes = FALSE) +
      geom_ribbon(data = quantile_df, 
                  aes(x = fitted_mid, ymin = q25, ymax = q75),
                  fill = gam_colors$tertiary, alpha = 0.5, inherit.aes = FALSE) +
      geom_line(data = quantile_df, 
                aes(x = fitted_mid, y = q50),
                color = gam_colors$dark, linewidth = 1, inherit.aes = FALSE) +
      # Zero line
      geom_hline(yintercept = 0, color = gam_colors$dark, linetype = "dashed", linewidth = 0.5) +
      # Points
      geom_point(color = gam_colors$primary, alpha = 0.3, size = 1) +
      # Loess smooth
      geom_smooth(method = "loess", se = FALSE, 
                  color = gam_colors$secondary, linewidth = 1) +
      labs(
         title = title %||% "Residuals vs Fitted (with Quantile Fan)",
         x = "Fitted values",
         y = "Residuals"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
}

#' Simplified scale-location: absolute residuals vs fitted
#' This shows heteroscedasticity without the sqrt transformation
#' Purpose: Check if variance of residuals increases/decreases with fitted values
#' @param gam_object A fitted GAM object
#' @param title Plot title
plot_gam_abs_resid <- function(gam_object, title = NULL) {
   
   fitted_vals <- fitted(gam_object)
   resids <- residuals(gam_object, type = "deviance")
   abs_resids <- abs(resids)
   
   df <- data.frame(fitted = fitted_vals, abs_resid = abs_resids)
   
   # Use running mean instead of loess for large datasets
   n <- nrow(df)
   if (n > 10000) {
      # Bin the data
      df$bin <- cut(df$fitted, breaks = 50, labels = FALSE)
      smooth_df <- df %>%
         group_by(bin) %>%
         summarise(
            fitted_mean = mean(fitted, na.rm = TRUE),
            abs_resid_mean = mean(abs_resid, na.rm = TRUE),
            .groups = 'drop'
         )
      
      ggplot(df, aes(x = fitted, y = abs_resid)) +
         geom_point(color = gam_colors$primary, alpha = 0.3, size = 1) +
         geom_line(data = smooth_df, 
                   aes(x = fitted_mean, y = abs_resid_mean),
                   color = gam_colors$secondary, linewidth = 1) +
         labs(
            title = title %||% "Absolute Residuals vs Fitted",
            x = "Fitted values",
            y = "|Residuals|"
         ) +
         theme_bw() +
         theme(
            plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
            axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
            axis.text = element_text(size = 12, color = gam_colors$dark),
            panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
         )
   } else {
      # Use loess for smaller datasets
      ggplot(df, aes(x = fitted, y = abs_resid)) +
         geom_point(color = gam_colors$primary, alpha = 0.5, size = 1.5) +
         geom_smooth(method = "loess", se = FALSE, span = 0.5,
                     color = gam_colors$secondary, linewidth = 1) +
         labs(
            title = title %||% "Absolute Residuals vs Fitted",
            x = "Fitted values",
            y = "|Residuals|"
         ) +
         theme_bw() +
         theme(
            plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
            axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
            axis.text = element_text(size = 12, color = gam_colors$dark),
            panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
         )
   }
}

#' Plot predicted vs observed with 1:1 reference line
#' @param gam_object A fitted GAM object
#' @param title Plot title
#' @param show_metrics If TRUE, display R² and RMSE on plot
plot_gam_pred_obs <- function(gam_object, title = NULL, show_metrics = TRUE) {
   
   fitted_vals <- fitted(gam_object)
   observed <- gam_object$y
   
   df <- data.frame(predicted = fitted_vals, observed = observed)
   
   # Calculate metrics if requested
   if (show_metrics) {
      rsq <- cor(observed, fitted_vals, use = "complete.obs")^2
      rmse <- sqrt(mean((observed - fitted_vals)^2, na.rm = TRUE))
      
      label_text <- sprintf("R² = %.3f\nRMSE = %.3f", rsq, rmse)
      
      # Position label in upper left
      x_pos <- min(fitted_vals, na.rm = TRUE) + 
         0.05 * diff(range(fitted_vals, na.rm = TRUE))
      y_pos <- max(observed, na.rm = TRUE) - 
         0.05 * diff(range(observed, na.rm = TRUE))
   }
   
   p <- ggplot(df, aes(x = predicted, y = observed)) +
      geom_abline(intercept = 0, slope = 1, 
                  color = gam_colors$dark, 
                  linetype = "solid", 
                  linewidth = 1) +
      geom_point(color = gam_colors$primary, alpha = 0.5, size = 1.5) +
      labs(
         title = title %||% "Predicted vs Observed",
         x = "Predicted values",
         y = "Observed values"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
   
   if (show_metrics) {
      p <- p + annotate("text", x = x_pos, y = y_pos, 
                        label = label_text, 
                        hjust = 0, vjust = 1,
                        size = 5, color = gam_colors$dark,
                        fontface = "bold")
   }
   
   return(p)
}

#' Plot predicted vs observed for values above/below threshold
#' @param gam_object A fitted GAM object
#' @param threshold Threshold value for subsetting
#' @param above If TRUE, plot values above threshold; if FALSE, below threshold
#' @param title Plot title
#' @param show_metrics If TRUE, display R² and RMSE on plot
plot_gam_pred_obs_threshold <- function(gam_object, 
                                        threshold, 
                                        above = TRUE,
                                        title = NULL, 
                                        show_metrics = TRUE) {
   
   fitted_vals <- fitted(gam_object)
   observed <- gam_object$y
   
   # Apply threshold
   if (above) {
      mask <- observed >= threshold
      default_title <- sprintf("Predicted vs Observed (≥ %.2f)", threshold)
   } else {
      mask <- observed < threshold
      default_title <- sprintf("Predicted vs Observed (< %.2f)", threshold)
   }
   
   df <- data.frame(
      predicted = fitted_vals[mask],
      observed = observed[mask]
   )
   
   n_points <- sum(mask, na.rm = TRUE)
   
   # Calculate metrics if requested
   if (show_metrics && n_points > 0) {
      rsq <- cor(df$observed, df$predicted, use = "complete.obs")^2
      rmse <- sqrt(mean((df$observed - df$predicted)^2, na.rm = TRUE))
      
      label_text <- sprintf("n = %d\nR² = %.3f\nRMSE = %.3f", 
                            n_points, rsq, rmse)
      
      x_pos <- min(df$predicted, na.rm = TRUE) + 
         0.05 * diff(range(df$predicted, na.rm = TRUE))
      y_pos <- max(df$observed, na.rm = TRUE) - 
         0.05 * diff(range(df$observed, na.rm = TRUE))
   }
   
   p <- ggplot(df, aes(x = predicted, y = observed)) +
      geom_abline(intercept = 0, slope = 1, 
                  color = gam_colors$dark, 
                  linetype = "solid", 
                  linewidth = 1) +
      geom_point(color = gam_colors$primary, alpha = 0.5, size = 1.5) +
      labs(
         title = title %||% default_title,
         x = "Predicted values",
         y = "Observed values"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
   
   if (show_metrics && n_points > 0) {
      p <- p + annotate("text", x = x_pos, y = y_pos, 
                        label = label_text, 
                        hjust = 0, vjust = 1,
                        size = 5, color = gam_colors$dark,
                        fontface = "bold")
   }
   
   return(p)
}

#' Plot predicted vs observed split by threshold
#' Creates side-by-side plots for above and below threshold
#' @param gam_object A fitted GAM object
#' @param threshold Threshold value for splitting
#' @param title Overall plot title
#' @param show_metrics If TRUE, display R² and RMSE on each plot
plot_gam_pred_obs_split <- function(gam_object, 
                                    threshold,
                                    title = NULL, 
                                    show_metrics = TRUE) {
   
   p1 <- plot_gam_pred_obs_threshold(gam_object, threshold, 
                                     above = FALSE, 
                                     show_metrics = show_metrics)
   p2 <- plot_gam_pred_obs_threshold(gam_object, threshold, 
                                     above = TRUE, 
                                     show_metrics = show_metrics)
   
   combined <- p1 | p2
   
   if (!is.null(title)) {
      combined <- combined + plot_annotation(
         title = title,
         theme = theme(plot.title = element_text(size = 18, face = 'bold', 
                                                 color = gam_colors$dark))
      )
   }
   
   return(combined)
}

#' Plot histogram of residuals
#' @param gam_object A fitted GAM object
#' @param bins Number of bins (default 30)
#' @param title Plot title
plot_gam_resid_hist <- function(gam_object, bins = 30, title = NULL) {
   
   resids <- residuals(gam_object, type = "deviance")
   
   df <- data.frame(resid = resids)
   
   ggplot(df, aes(x = resid)) +
      geom_histogram(bins = bins, 
                     fill = gam_colors$primary, 
                     color = gam_colors$dark,
                     alpha = 0.7) +
      geom_vline(xintercept = 0, 
                 color = gam_colors$dark, 
                 linetype = "dashed",
                 linewidth = 0.8) +
      labs(
         title = title %||% "Histogram of Residuals",
         x = "Residuals",
         y = "Count"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
}

#' Plot smooth effects from GAM with shaded confidence intervals
#' Handles by variables (e.g., by = WindDir)
#' @param gam_object A fitted GAM object
#' @param select Which smooth to plot (numeric index or character name)
#' @param n_points Number of points for prediction (default 200)
#' @param title Plot title (if NULL, uses smooth term name)
#' @param lag_name Name of the lag (e.g., "Lag1", "Lag5") for title formatting
plot_gam_smooth_single <- function(gam_object, select = 1, n_points = 200, title = NULL, lag_name = NULL) {
   
   # Get smooth terms
   smooth_terms <- gam_object$smooth
   
   if (is.numeric(select)) {
      if (select > length(smooth_terms)) {
         stop("select index exceeds number of smooths")
      }
      smooth_obj <- smooth_terms[[select]]
   } else {
      # Find smooth by name
      smooth_names <- sapply(smooth_terms, function(x) x$label)
      idx <- which(smooth_names == select)
      if (length(idx) == 0) {
         stop("Smooth '", select, "' not found")
      }
      smooth_obj <- smooth_terms[[idx]]
   }
   
   # Get the variable name and clean it
   var_name <- smooth_obj$term
   # Remove trailing _1, _2, etc. that mgcv adds for by variables
   var_name_clean <- sub("_\\d+$", "", var_name)
   
   # Check if this is a "by" smooth
   by_var <- smooth_obj$by
   has_by <- by_var != "NA"
   
   # Get predictor values
   model_data <- gam_object$model
   x_vals <- model_data[[var_name]]
   x_range <- range(x_vals, na.rm = TRUE)
   
   # If this is a "by" smooth, we need to handle it differently
   if (has_by) {
      # Get the "by" variable levels
      by_vals <- model_data[[by_var]]
      by_level <- unique(by_vals)[1]  # This smooth is for a specific level
      
      # The by variable in the smooth object tells us which level
      # For factor by variables, the smooth$by.level tells us the level
      if (!is.null(smooth_obj$by.level)) {
         by_level <- smooth_obj$by.level
      }
      
      # Create prediction data for this specific by level
      newdata <- data.frame(x = seq(x_range[1], x_range[2], length.out = n_points))
      names(newdata) <- var_name
      newdata[[by_var]] <- by_level
      
      # Add other predictors at their means
      for (var in names(model_data)) {
         if (var != var_name && var != by_var && var != names(model_data)[1]) {
            if (is.numeric(model_data[[var]])) {
               newdata[[var]] <- mean(model_data[[var]], na.rm = TRUE)
            } else {
               newdata[[var]] <- model_data[[var]][1]
            }
         }
      }
      
      # Predict with standard errors
      preds <- predict(gam_object, newdata = newdata, se.fit = TRUE, type = "terms")
      
      # Extract the smooth term effect
      smooth_idx <- which(colnames(preds$fit) == smooth_obj$label)
      
      pred_df <- data.frame(
         x = newdata[[var_name]],
         fit = preds$fit[, smooth_idx],
         se = preds$se.fit[, smooth_idx]
      )
      
      pred_df$lower <- pred_df$fit - 1.96 * pred_df$se
      pred_df$upper <- pred_df$fit + 1.96 * pred_df$se
      
      # Filter x_vals to only this by level for rug plot
      x_vals_filtered <- x_vals[by_vals == by_level]
      
      # Create plot title
      if (is.null(title)) {
         if (!is.null(lag_name)) {
            title <- paste0(var_name_clean, " (", by_level, ") at ", lag_name, " Smooth")
         } else {
            title <- paste0(var_name_clean, " (", by_level, ") Smooth")
         }
      }
      
   } else {
      # Non-by smooth (original code)
      newdata <- data.frame(x = seq(x_range[1], x_range[2], length.out = n_points))
      names(newdata) <- var_name
      
      # Add other predictors at their means
      for (var in names(model_data)) {
         if (var != var_name && var != names(model_data)[1]) {
            if (is.numeric(model_data[[var]])) {
               newdata[[var]] <- mean(model_data[[var]], na.rm = TRUE)
            } else {
               newdata[[var]] <- model_data[[var]][1]
            }
         }
      }
      
      # Predict with standard errors
      preds <- predict(gam_object, newdata = newdata, se.fit = TRUE, type = "terms")
      
      # Extract the smooth term effect
      smooth_idx <- which(colnames(preds$fit) == smooth_obj$label)
      
      pred_df <- data.frame(
         x = newdata[[var_name]],
         fit = preds$fit[, smooth_idx],
         se = preds$se.fit[, smooth_idx]
      )
      
      pred_df$lower <- pred_df$fit - 1.96 * pred_df$se
      pred_df$upper <- pred_df$fit + 1.96 * pred_df$se
      
      x_vals_filtered <- x_vals
      
      # Create plot title
      if (is.null(title)) {
         if (!is.null(lag_name)) {
            title <- paste0(var_name_clean, " at ", lag_name, " Smooth")
         } else {
            title <- paste0(var_name_clean, " Smooth")
         }
      }
   }
   
   ggplot(pred_df, aes(x = x, y = fit)) +
      geom_hline(yintercept = 0, color = gam_colors$dark, 
                 linetype = "dashed", linewidth = 0.5) +
      geom_ribbon(aes(ymin = lower, ymax = upper), 
                  fill = gam_colors$secondary, alpha = 0.3) +
      geom_line(color = gam_colors$primary, linewidth = 1) +
      geom_rug(data = data.frame(x = x_vals_filtered), 
               aes(x = x), inherit.aes = FALSE,
               sides = "b", alpha = 0.3, color = gam_colors$dark) +
      labs(
         title = title,
         x = var_name_clean,
         y = paste0("s(", var_name_clean, ")")
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1)
      )
}

#' Plot all smooths from GAM in a grid
#' @param gam_object A fitted GAM object
#' @param n_points Number of points for prediction (default 200)
#' @param title Overall title
#' @param lag_name Name of the lag (e.g., "Lag1", "Lag5") for panel titles
plot_gam_smooths <- function(gam_object, n_points = 200, title = NULL, lag_name = NULL) {
   
   smooth_terms <- gam_object$smooth
   n_smooths <- length(smooth_terms)
   
   if (n_smooths == 0) {
      stop("No smooth terms in this GAM")
   }
   
   # Create individual plots - pass lag_name to each
   plot_list <- lapply(1:n_smooths, function(i) {
      plot_gam_smooth_single(gam_object, select = i, n_points = n_points, lag_name = lag_name)
   })
   
   # Arrange in grid
   if (n_smooths == 1) {
      combined <- plot_list[[1]]
   } else if (n_smooths == 2) {
      combined <- plot_list[[1]] | plot_list[[2]]
   } else if (n_smooths <= 4) {
      combined <- wrap_plots(plot_list, ncol = 2)
   } else {
      combined <- wrap_plots(plot_list, ncol = 3)
   }
   
   if (!is.null(title)) {
      combined <- combined + plot_annotation(
         title = title,
         theme = theme(plot.title = element_text(size = 18, face = 'bold', 
                                                 color = gam_colors$dark))
      )
   }
   
   combined
}

#' Create comprehensive 6-panel diagnostic plot
#' @param gam_object A fitted GAM object
#' @param title Overall title
#' @param max_lag Maximum lag for ACF/PACF plots
plot_gam_diagnostics_full <- function(gam_object, title = NULL, max_lag = 500) {
   
   p1 <- plot_gam_resid_fitted(gam_object, title = "Residuals vs Fitted")
   p2 <- plot_gam_qq(gam_object, title = "Q-Q Plot")
   p3 <- plot_gam_scale_location(gam_object, title = "Scale-Location")
   p4 <- plot_gam_response_fitted(gam_object, title = "Response vs Fitted")
   p5 <- plot_gam_acf(gam_object, max_lag = max_lag, title = "ACF")
   p6 <- plot_gam_pacf(gam_object, max_lag = max_lag, title = "PACF")
   
   combined <- (p1 | p2 | p3) / (p4 | p5 | p6)
   
   if (!is.null(title)) {
      combined <- combined + plot_annotation(
         title = title,
         theme = theme(plot.title = element_text(size = 18, face = 'bold', 
                                                 color = gam_colors$dark))
      )
   }
   
   combined
}

#' Create focused 4-panel diagnostic plot (most important diagnostics)
#' @param gam_object A fitted GAM object
#' @param title Overall title
#' @param max_lag Maximum lag for ACF/PACF plots
plot_gam_diagnostics <- function(gam_object, title = NULL, max_lag = 500) {
   
   p1 <- plot_gam_resid_fitted(gam_object, title = "Residuals vs Fitted")
   p2 <- plot_gam_qq(gam_object, title = "Q-Q Plot")
   p3 <- plot_gam_acf(gam_object, max_lag = max_lag, title = "ACF")
   p4 <- plot_gam_pacf(gam_object, max_lag = max_lag, title = "PACF")
   
   combined <- (p1 | p2) / (p3 | p4)
   
   if (!is.null(title)) {
      combined <- combined + plot_annotation(
         title = title,
         theme = theme(plot.title = element_text(size = 18, face = 'bold', 
                                                 color = gam_colors$dark))
      )
   }
   
   combined
}

#' Print numerical diagnostics summary
#' @param gam_object A fitted GAM object
print_gam_diagnostics <- function(gam_object) {
   
   resids <- residuals(gam_object, type = "deviance")
   
   cat("=== GAM Diagnostic Summary ===\n\n")
   
   cat("Model Summary:\n")
   cat("  Deviance explained:", 
       sprintf("%.2f%%", summary(gam_object)$dev.expl * 100), "\n")
   cat("  R-squared (adjusted):", 
       sprintf("%.4f", summary(gam_object)$r.sq), "\n")
   cat("  GCV score:", sprintf("%.4f", gam_object$gcv.ubre), "\n")
   cat("  N observations:", length(resids), "\n\n")
   
   cat("Residual Statistics:\n")
   cat("  Mean:", sprintf("%.6f", mean(resids, na.rm = TRUE)), "\n")
   cat("  SD:", sprintf("%.4f", sd(resids, na.rm = TRUE)), "\n")
   cat("  Min:", sprintf("%.4f", min(resids, na.rm = TRUE)), "\n")
   cat("  Max:", sprintf("%.4f", max(resids, na.rm = TRUE)), "\n\n")
   
   # Normality tests
   if (length(resids) < 5000) {
      sw_test <- shapiro.test(resids)
      cat("Shapiro-Wilk normality test:\n")
      cat("  W =", sprintf("%.4f", sw_test$statistic), 
          ", p-value =", sprintf("%.4e", sw_test$p.value), "\n\n")
   }
   
   # Ljung-Box test for autocorrelation
   lb_test <- Box.test(resids, lag = min(20, floor(length(resids)/5)), type = "Ljung-Box")
   cat("Ljung-Box test for autocorrelation (lag 20):\n")
   cat("  X-squared =", sprintf("%.4f", lb_test$statistic), 
       ", p-value =", sprintf("%.4e", lb_test$p.value), "\n")
   if (lb_test$p.value < 0.05) {
      cat("  WARNING: Significant autocorrelation detected\n")
   }
   cat("\n")
   
   # Durbin-Watson test
   if (requireNamespace("lmtest", quietly = TRUE)) {
      dw_test <- lmtest::dwtest(resids ~ 1)
      cat("Durbin-Watson test:\n")
      cat("  DW =", sprintf("%.4f", dw_test$statistic), 
          ", p-value =", sprintf("%.4e", dw_test$p.value), "\n\n")
   }
   
   cat("==============================\n")
}

#' Plot performance metrics across lead times
#' @param metrics_df Data frame with columns: LeadTime, Subset, and metric columns
#' @param metric Name of the metric column to plot (e.g., "RMSE", "MAE", "R2")
#' @param title Plot title (if NULL, auto-generated)
#' @param y_label Y-axis label (if NULL, uses metric name)
plot_performance_by_leadtime <- function(metrics_df, 
                                         metric = "RMSE",
                                         title = NULL,
                                         y_label = NULL,
                                         x_label = NULL) {
   
   # Set default labels if not provided
   if (is.null(title)) {
      title <- paste(metric, "Across Lead Times")
   }
   
   if (is.null(y_label)) {
      y_label <- metric
   }
   
   # Get unique subsets
   subsets <- unique(metrics_df$Subset)
   
   # Find the non-Overall subset
   other_subset <- subsets[subsets != "Overall"]
   
   # Define colors for subsets dynamically
   subset_colors <- c(gam_colors$secondary, gam_colors$primary)
   names(subset_colors) <- c("Overall", other_subset)
   
   # Create plot
   p <- ggplot(metrics_df, aes(x = LeadTime, y = .data[[metric]], 
                               color = Subset, shape = Subset)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3.5) +
      scale_color_manual(values = subset_colors) +
      scale_shape_manual(values = c(16, 17)) +  # circle, triangle
      labs(
         title = title,
         x = x_label,
         y = y_label,
         color = "Data Subset",
         shape = "Data Subset"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.position = "bottom",
         legend.title = element_text(size = 12, face = 'bold', color = gam_colors$dark),
         legend.text = element_text(size = 11, color = gam_colors$dark),
         legend.background = element_rect(fill = "white", color = NA),
         legend.key = element_rect(fill = "white", color = NA)
      )
   
   return(p)
}