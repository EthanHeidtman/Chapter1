# =============================================================================
# Script Name:    SimpleModelEvaluationPlots.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-11-25
# Last Updated:   2025-11-25
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
plot_salinity_with_models <- function(data, 
                                      year = NULL,
                                      date_range = NULL,
                                      models = NULL,
                                      highlight_start = NULL,
                                      highlight_end = NULL,
                                      epa_line = TRUE,
                                      title = NULL) {
   
   # ---- Styling Parameters ----
   observed_linewidth <- 0.7
   model_linewidth <- 0.7
   observed_alpha <- 0.8
   model_alpha <- 1.0
   
   observed_color <- "#f58220"
   model_palette <- c("#3b7ea1",  "#6a994e", "#bc4b51", 
                      "#8338ec", "#fb5607", "#ffbe0b", "#06ffa5", "#c4820e")
   
   # ---- Filter Data ----
   if (!is.null(year)) {
      plot_data <- data %>% 
         dplyr::filter(Year == year)
   } else if (!is.null(date_range)) {
      plot_data <- data %>% 
         dplyr::filter(DateTime >= as_datetime(date_range[1]) & 
                          DateTime <= as_datetime(date_range[2]))
   } else {
      plot_data <- data
   }
   
   # Ensure chronological ordering
   plot_data <- plot_data %>% 
      arrange(DateTime)
   
   # ---- Identify Model Columns ----
   if (is.null(models)) {
      non_model_cols <- c('DateTime', 'Date', 'Year', 'Month', 'Day', 'DayOfYear', 
                          'FERC', 'Salinity', 'Inflows', 'LogInflows',
                          grep('^Rolling|Range|Cos|Sin', names(plot_data), value = TRUE))
      models <- setdiff(names(plot_data), non_model_cols)
   }
   models <- models[models %in% names(plot_data)]
   
   # ---- Build Aesthetic Scales ----
   color_scale <- c("Observed" = observed_color)
   for (i in seq_along(models)) {
      color_scale[models[i]] <- model_palette[i]
   }
   
   size_scale <- c("Observed" = observed_linewidth)
   for (model in models) {
      size_scale[model] <- model_linewidth
   }
   
   alpha_scale <- c("Observed" = observed_alpha)
   for (model in models) {
      alpha_scale[model] <- model_alpha
   }
   
   # ---- Reshape Data ----
   plot_data_long <- plot_data %>%
      dplyr::select(DateTime, Salinity, all_of(models)) %>%
      tidyr::pivot_longer(cols = c(Salinity, all_of(models)), 
                          names_to = "Series", 
                          values_to = "Value") %>%
      dplyr::mutate(Series = ifelse(Series == "Salinity", "Observed", Series),
                    Series = factor(Series, levels = c("Observed", models))) %>%
      dplyr::arrange(Series, DateTime) %>%
      dplyr::group_by(Series) %>%
      dplyr::mutate(
         dt = as.numeric(difftime(DateTime, lag(DateTime), units = "secs")),
         base_dt = median(dt, na.rm = TRUE),
         segment = cumsum(is.na(dt) | dt > 1.5 * base_dt)
      ) %>%
      dplyr::ungroup()
   
   # ---- Initialize Plot ----
   p <- ggplot(plot_data_long, aes(
      x = DateTime, 
      y = Value, 
      color = Series, 
      size = Series, 
      alpha = Series,
      group = interaction(Series, segment)
   ))
   
   # ---- Add Highlight Rectangle ----
   # if (!is.null(highlight_start) && !is.null(highlight_end)) {
   #    p <- p + annotate("rect",
   #                      xmin = highlight_start, 
   #                      xmax = highlight_end,
   #                      ymin = -Inf, 
   #                      ymax = Inf,
   #                      fill = "#fdb515", 
   #                      alpha = 0.2)
   # }
   
   # ---- Add EPA Reference Line ----
   if (epa_line) {
      p <- p + 
         geom_hline(yintercept = 0.5, 
                    color = '#002030', 
                    linetype = 2) +
         annotate("text",
                  x = min(plot_data$DateTime),
                  y = 0.52,
                  label = "EPA Secondary Drinking Water Standard for TDS",
                  hjust = 0,
                  vjust = 0,
                  size = 5,
                  colour = "#002030")
   }
   
   # ---- Add Time Series Lines ----
   p <- p + geom_line()
   
   # ---- Apply Scales ----
   p <- p + 
      scale_color_manual(values = color_scale, name = "Model") +
      scale_size_manual(values = size_scale, guide = "none") +
      scale_alpha_manual(values = alpha_scale, guide = "none") +
      scale_y_continuous(name = "Salinity (psu)")
   
   # ---- Apply Theme ----
   p <- p +
      theme_bw() +
      labs(title = title %||% "Salinity and Model Predictions", 
           x = "Date") +
      theme(
         plot.title         = element_text(size = 30, face = 'bold', color = '#002030'),
         axis.title.x       = element_text(size = 28, face = 'bold', color = '#002030'),
         axis.title.y.left  = element_text(size = 28, face = 'bold', colour = "#f58220"),
         axis.text.y.left   = element_text(colour = "#f58220", size = 24),
         axis.text.x        = element_text(size = 20),
         panel.border       = element_rect(colour = '#002030', fill = NA, linewidth = 1),
         legend.position    = "bottom",
         legend.title       = element_text(size = 20, face = 'bold'),
         legend.text        = element_text(size = 16)
      )
   
   return(p)
}

plot_salinity_forecast_panels <- function(data,
                                          date_range = NULL,
                                          year = NULL,
                                          models = NULL,
                                          epa_line = TRUE,
                                          title = NULL) {
   
   # ---- Styling Parameters ----
   observed_linewidth <- 0.9
   model_linewidth    <- 0.7
   observed_alpha     <- 0.8
   model_alpha        <- 1.0
   
   observed_color <- "#f58220"
   model_palette  <- c("#3b7ea1", "#6a994e", "#8338ec", "#bc4b51",
                       "#fb5607", "#ffbe0b", "#06ffa5", "#c4820e")
   
   # ---- Filter to date window (identical logic to original function) ----
   if (!is.null(year)) {
      base_data <- data %>% dplyr::filter(Year == year)
   } else if (!is.null(date_range)) {
      base_data <- data %>%
         dplyr::filter(DateTime >= as_datetime(date_range[1]) &
                          DateTime <= as_datetime(date_range[2]))
   } else {
      base_data <- data
   }
   
   base_data <- base_data %>% dplyr::arrange(DateTime)
   
   # ---- Identify Model Columns ----
   if (is.null(models)) {
      non_model_cols <- c('DateTime', 'Date', 'Year', 'Month', 'Day', 'DayOfYear',
                          'FERC', 'Salinity', 'Inflows', 'LogInflows',
                          grep('^Rolling|Range|Cos|Sin', names(base_data), value = TRUE))
      models <- setdiff(names(base_data), non_model_cols)
   }
   models <- models[models %in% names(base_data)]
   models <- models[1:min(3, length(models))]
   
   # ---- Gap-detection helper ----
   add_segments <- function(df) {
      df %>%
         dplyr::mutate(
            dt      = as.numeric(difftime(DateTime, lag(DateTime), units = "secs")),
            base_dt = median(dt, na.rm = TRUE),
            segment = cumsum(is.na(dt) | dt > 1.5 * base_dt)
         )
   }
   
   # ---- Helper: build one panel ----
   make_panel <- function(model_name, model_color,
                          show_x_axis  = FALSE,
                          show_y_axis  = TRUE,
                          show_y_label = FALSE,
                          panel_title  = NULL) {
      
      # No cutoff — use the full filtered date range as-is
      obs_df <- base_data %>%
         dplyr::select(DateTime, Value = Salinity) %>%
         dplyr::mutate(Series = "Observed") %>%
         add_segments()
      
      mod_df <- base_data %>%
         dplyr::select(DateTime, Value = dplyr::all_of(model_name)) %>%
         dplyr::mutate(Series = model_name) %>%
         add_segments()
      
      plot_long <- dplyr::bind_rows(obs_df, mod_df) %>%
         dplyr::mutate(Series = factor(Series, levels = c("Observed", model_name)))
      
      label_row <- mod_df %>%
         dplyr::filter(!is.na(Value)) %>%
         dplyr::slice_max(DateTime, n = 1)
      
      p <- ggplot(plot_long,
                  aes(x = DateTime, y = Value,
                      color = Series, size = Series, alpha = Series,
                      group = interaction(Series, segment))) +
         geom_line()
      
      if (epa_line) {
         p <- p +
            geom_hline(yintercept = 0.5, color = '#002030', linetype = 2) +
            annotate("text",
                     x      = min(base_data$DateTime),
                     y      = 0.52,
                     label  = "EPA Secondary Drinking Water Standard for TDS",
                     hjust  = 0, vjust = 0,
                     size   = 4,
                     colour = "#002030")
      }
      
      p <- p +
         annotate("text",
                  x        = label_row$DateTime,
                  y        = label_row$Value,
                  label    = model_name,
                  hjust    = 1.05,
                  vjust    = -0.5,
                  size     = 5,
                  fontface = "bold",
                  colour   = model_color) +
         scale_color_manual(values = c("Observed" = observed_color,
                                       setNames(model_color, model_name))) +
         scale_size_manual( values = c("Observed" = observed_linewidth,
                                       setNames(model_linewidth, model_name))) +
         scale_alpha_manual(values = c("Observed" = observed_alpha,
                                       setNames(model_alpha, model_name))) +
         scale_y_continuous(name = if (show_y_label) "Salinity (psu)" else NULL) +
         labs(
            x     = if (show_x_axis) "Date" else NULL,
            title = panel_title
         ) +
         theme_bw() +
         theme(
            plot.title        = element_text(size = 18, face = 'bold', color = '#002030'),
            axis.title.x      = element_text(size = 16, face = 'bold', color = '#002030'),
            axis.title.y.left = element_text(size = 16, face = 'bold', colour = "#f58220"),
            axis.text.y.left  = element_text(colour = "#f58220", size = 13),
            axis.text.x       = if (show_x_axis) element_text(size = 13) else element_blank(),
            axis.ticks.x      = if (show_x_axis) element_line() else element_blank(),
            panel.border      = element_rect(colour = '#002030', fill = NA, linewidth = 1),
            legend.position   = "none"
         )
      
      if (!show_y_axis) {
         p <- p + theme(axis.text.y  = element_blank(),
                        axis.ticks.y = element_blank())
      }
      
      p
   }
   
   # ---- Build panels (one per model, same date range, different model each time) ----
   panel_colors <- model_palette[seq_along(models)]
   
   panels <- purrr::pmap(
      list(
         model_name   = models,
         model_color  = panel_colors,
         show_x_axis  = c(FALSE, FALSE, TRUE)[seq_along(models)],
         show_y_axis  = rep(TRUE, length(models)),
         show_y_label = c(FALSE, TRUE, FALSE)[seq_along(models)],
         panel_title  = c(list(title), rep(list(NULL), length(models) - 1))
      ),
      make_panel
   )
   
   # ---- Combine ----
   patchwork::wrap_plots(panels, ncol = 1) &
      theme(plot.margin = margin(2, 10, 2, 10))
}


plot_fold_performance <- function(fold_metrics, metric = "rmse") {
   
   metric_labels <- list(
      rmse = "RMSE",
      rsq = "R²",
      mae = "MAE"
   )
   
   fold_metrics %>%
      filter(.metric == metric) %>%
      mutate(fold_num = as.numeric(gsub("Fold", "", id))) %>%
      ggplot(aes(x = fold_num, y = .estimate, color = model, group = model)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      labs(
         title = paste(metric_labels[[metric]], "Evolution Across Expanding Window Folds"),
         x = "Fold Number",
         y = metric_labels[[metric]],
         color = "Model"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         legend.position = "bottom",
         plot.title = element_text(face = "bold", size = 14)
      )
}

plot_cv_summary <- function(cv_summary, metric = "rmse") {
   
   metric_labels <- list(
      rmse = "RMSE",
      rsq = "R²",
      mae = "MAE"
   )
   
   cv_summary %>%
      filter(.metric == metric) %>%
      ggplot(aes(x = reorder(model, mean), y = mean, fill = model)) +
      geom_col(alpha = 0.7, width = 0.6) +
      geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                    width = 0.2, linewidth = 1) +
      geom_text(aes(label = round(mean, 3)), vjust = -0.5, nudge_y = 0.02, 
                fontface = "bold") +
      labs(
         title = paste("Average CV", metric_labels[[metric]], "(±SE) Across Models"),
         x = "Model",
         y = paste("Mean", metric_labels[[metric]]),
         fill = "Model"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         legend.position = "none",
         plot.title = element_text(face = "bold", size = 14),
         axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
}

plot_obs_pred <- function(data, 
                          start_date = NULL, 
                          end_date = NULL, 
                          models = c("Elastic", "Lasso", "Ridge", 'RF', 'GamAllVars', 'GamNoTide', 'GamNoTideNoTime', 'GamNoInflows'),
                          show_metrics = TRUE,
                          alpha = 0.3,
                          point_size = 0.5) {
   
   # Filter by date if specified
   plot_data <- data
   if (!is.null(start_date) & !is.null(end_date)) {
      plot_data <- plot_data %>%
         filter(DateTime >= as.POSIXct(start_date), 
                DateTime <= as.POSIXct(end_date))
   }
   
   # Reshape for plotting
   plot_data_long <- plot_data %>%
      dplyr::select(DateTime, Salinity, Elastic, Lasso, Ridge, RF, GamAllVars, GamNoTide, GamNoTideNoTime, GamNoInflows) %>%
      pivot_longer(cols = c(Elastic, Lasso, Ridge, RF, GamAllVars, GamNoTide, GamNoTideNoTime, GamNoInflows),
                   names_to = "model", values_to = "predicted") %>%
      mutate(model = case_when(
         model == "Elastic" ~ "Elastic",
         model == "Lasso" ~ "Lasso",
         model == "Ridge" ~ "Ridge",
         model == 'RF' ~ 'RF',
         model == 'GamAllVars' ~ 'GamAllVars',
         model == 'GamNoTide' ~ 'GamNoTide',
         model == 'GamNoTideNoTime' ~ 'GamNoTideNoTime',
         model == 'GamNoInflows' ~ 'GamNoInflows',
      )) %>%
      filter(model %in% models)
   
   # Calculate metrics
   metrics <- plot_data_long %>%
      group_by(model) %>%
      summarize(
         rmse = sqrt(mean((Salinity - predicted)^2)),
         rsq = cor(Salinity, predicted)^2,
         n = n(),
         .groups = "drop"
      )
   
   # Base plot
   p <- ggplot(plot_data_long, aes(x = Salinity, y = predicted)) +
      geom_point(alpha = alpha, size = point_size) +
      geom_abline(intercept = 0, slope = 1, color = "red", 
                  linetype = "dashed", linewidth = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
      facet_wrap(~model) +
      labs(
         title = "Observed vs Predicted Salinity",
         subtitle = if (!is.null(start_date)) {
            paste("Period:", start_date, "to", end_date, "|", 
                  format(nrow(plot_data), big.mark = ","), "observations")
         } else {
            paste("Full Time Series |", format(nrow(plot_data), big.mark = ","), "observations")
         },
         x = "Observed Salinity",
         y = "Predicted Salinity"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         strip.text = element_text(face = "bold", size = 11),
         plot.title = element_text(face = "bold", size = 14)
      )
   
   # Add metrics if requested
   if (show_metrics) {
      p <- p + geom_text(
         data = metrics,
         aes(x = -Inf, y = Inf, 
             label = paste0("RMSE: ", round(rmse, 2), 
                            "\nR²: ", round(rsq, 3),
                            "\nn: ", format(n, big.mark = ","))),
         hjust = -0.1, vjust = 1.2, size = 3, fontface = "bold"
      )
   }
   
   return(p)
}

plot_timeseries <- function(data,
                            start_date = NULL,
                            end_date = NULL,
                            models = c("Elastic", "Lasso", "Ridge", 'RF', 'GAM'),
                            show_residuals = FALSE) {
   
   # Filter by date
   plot_data <- data
   if (!is.null(start_date) & !is.null(end_date)) {
      plot_data <- plot_data %>%
         filter(DateTime >= as.POSIXct(start_date), 
                DateTime <= as.POSIXct(end_date))
   }
   
   # Reshape for plotting
   plot_data_long <- plot_data %>%
      select(DateTime, Salinity, Elastic, Lasso, Ridge, RF, GAM) %>%
      pivot_longer(cols = c(Elastic, Lasso, Ridge, RF, GAM),
                   names_to = "model", values_to = "predicted") %>%
      mutate(
         model = case_when(
            model == "Elastic" ~ "Elastic",
            model == "Lasso" ~ "Lasso",
            model == "Ridge" ~ "Ridge",
            model == 'RF' ~ 'RF',
            model == 'GAM' ~ 'GAM'
         ),
         residual = Salinity - predicted
      ) %>%
      filter(model %in% models)
   
   if (!show_residuals) {
      # Main time series plot
      p <- ggplot(plot_data_long, aes(x = DateTime)) +
         geom_line(aes(y = Salinity), color = "black", linewidth = 0.8, alpha = 0.7) +
         geom_line(aes(y = predicted, color = model), linewidth = 0.6, alpha = 0.8) +
         labs(
            title = "Observed vs Predicted Salinity Over Time",
            subtitle = if (!is.null(start_date)) {
               paste("Period:", start_date, "to", end_date)
            } else {
               "Full Time Series"
            },
            x = "Date",
            y = "Salinity",
            color = "Model"
         ) +
         scale_color_manual(
            values = c("Elastic" = "#E41A1C", "Lasso" = "#377EB8", "Ridge" = "#4DAF4A", 'RF' = 'thistle', 'GAM' = 'orange'),
            labels = c("Elastic", "Lasso", "Ridge", 'RF', 'GAM')
         ) +
         theme_minimal(base_size = 12) +
         theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", size = 14)
         )
      
   } else {
      # Residuals plot
      p <- ggplot(plot_data_long, aes(x = DateTime, y = residual, color = model)) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
         geom_line(linewidth = 0.5, alpha = 0.7) +
         facet_wrap(~model, ncol = 1) +
         labs(
            title = "Prediction Residuals Over Time",
            subtitle = if (!is.null(start_date)) {
               paste("Period:", start_date, "to", end_date)
            } else {
               "Full Time Series"
            },
            x = "Date",
            y = "Residual (Observed - Predicted)",
            color = "Model"
         ) +
         theme_minimal(base_size = 12) +
         theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", size = 14),
            strip.text = element_text(face = "bold")
         )
   }
   
   return(p)
}

plot_residual_diagnostics <- function(data, model_name = "Elastic") {

   
   plot_data <- data %>%
      mutate(
         predicted = .data[[model_name]],
         residual = Salinity - predicted
      )
   
   # Residuals vs fitted
   p1 <- ggplot(plot_data, aes(x = predicted, y = residual)) +
      geom_point(alpha = 0.3, size = 0.5) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(se = TRUE, color = "blue", linewidth = 0.8) +
      labs(
         title = paste(model_name, "- Residuals vs Fitted"),
         x = "Fitted Values",
         y = "Residuals"
      ) +
      theme_minimal(base_size = 12)
   
   # Q-Q plot
   p2 <- ggplot(plot_data, aes(sample = residual)) +
      stat_qq(alpha = 0.3, size = 0.5) +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
         title = paste(model_name, "- Normal Q-Q Plot"),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles"
      ) +
      theme_minimal(base_size = 12)
   
   # Histogram
   p3 <- ggplot(plot_data, aes(x = residual)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(
         title = paste(model_name, "- Residual Distribution"),
         x = "Residuals",
         y = "Count"
      ) +
      theme_minimal(base_size = 12)
   
   # Return list of plots
   return(list(
      residuals_vs_fitted = p1,
      qq_plot = p2,
      histogram = p3
   ))
}

plot_all_metrics_comparison <- function(cv_summary) {
   
   cv_summary %>%
      mutate(
         .metric = case_when(
            .metric == "rmse" ~ "RMSE",
            .metric == "rsq" ~ "R²",
            .metric == "mae" ~ "MAE",
            TRUE ~ .metric
         )
      ) %>%
      ggplot(aes(x = reorder(model, mean), y = mean, fill = model)) +
      geom_col(alpha = 0.7) +
      geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                    width = 0.2, linewidth = 0.8) +
      geom_text(aes(label = round(mean, 3)), vjust = -0.5, size = 3, fontface = "bold") +
      facet_wrap(~.metric, scales = "free_y") +
      labs(
         title = "Model Performance Comparison Across All Metrics",
         x = "Model",
         y = "Mean Value",
         fill = "Model"
      ) +
      theme_minimal(base_size = 12) +
      theme(
         legend.position = "none",
         plot.title = element_text(face = "bold", size = 14),
         strip.text = element_text(face = "bold", size = 11),
         axis.text.x = element_text(angle = 45, hjust = 1)
      )
}
