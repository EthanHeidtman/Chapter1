# =============================================================================
# Script Name:    GetRollingModelResults.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Gathers the outputs from the covariance experiment runs created
#                 by RunWindowExperiments.R. 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(ggplot2)
library(dplyr)
library(patchwork)


create_salinity_predictor_plot <- function(data,
                                           year = NULL,
                                           date_range = NULL,
                                           models = NULL,
                                           predictors = c('LogRollingDischarge24', 'TideRange24', 'RollingV168'),
                                           highlight_start = NULL,
                                           highlight_end = NULL,
                                           epa_line = TRUE,
                                           title = NULL) {
   
   library(patchwork)
   library(ggplot2)
   library(dplyr)
   library(tidyr)
   
   # ============================================================================
   # STYLING PARAMETERS
   # ============================================================================
   
   observed_linewidth <- 0.7
   model_linewidth <- 0.7
   observed_alpha <- 0.8
   model_alpha <- 1.0
   
   observed_color <- "#f58220"
   model_palette <- c("#3b7ea1", "#c4820e", "#6a994e", "#bc4b51", 
                      "#8338ec", "#fb5607", "#ffbe0b", "#06ffa5")
   
   predictor_color <- "#2d6a4f"
   
   # ============================================================================
   # FILTER DATA
   # ============================================================================
   
   if (!is.null(year)) {
      plot_data <- data %>% filter(Year == year)
   } else if (!is.null(date_range)) {
      plot_data <- data %>% 
         filter(DateTime >= as_datetime(date_range[1]) & 
                   DateTime <= as_datetime(date_range[2]))
   } else {
      plot_data <- data
   }
   
   # ============================================================================
   # IDENTIFY MODEL COLUMNS
   # ============================================================================
   
   if (is.null(models)) {
      non_model_cols <- c('DateTime', 'Date', 'Year', 'Month', 'Day', 'DayOfYear', 
                          'FERC', 'Salinity', 'Inflows', 'LogInflows',
                          grep('^Rolling|Range|Cos|Sin|log_|Norm_', names(plot_data), value = TRUE))
      models <- setdiff(names(plot_data), non_model_cols)
   }
   models <- models[models %in% names(plot_data)]
   
   # ============================================================================
   # PANEL 1: SALINITY WITH MODEL PREDICTIONS
   # ============================================================================
   
   # Build aesthetic scales
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
   
   # Reshape data for plotting
   plot_data_long <- plot_data %>%
      dplyr::select(DateTime, Salinity, all_of(models)) %>%
      pivot_longer(cols = c(Salinity, all_of(models)), 
                   names_to = "Series", 
                   values_to = "Value") %>%
      mutate(Series = ifelse(Series == "Salinity", "Observed", Series)) %>%
      mutate(Series = factor(Series, levels = c("Observed", models)))
   
   # Initialize salinity plot
   p_salinity <- ggplot(plot_data_long, aes(x = DateTime, y = Value, 
                                            color = Series, 
                                            size = Series, 
                                            alpha = Series))
   
   # Add highlight rectangle if specified
   if (!is.null(highlight_start) && !is.null(highlight_end)) {
      p_salinity <- p_salinity + 
         annotate("rect",
                  xmin = highlight_start, 
                  xmax = highlight_end,
                  ymin = -Inf, 
                  ymax = Inf,
                  fill = "#fdb515", 
                  alpha = 0.2)
   }
   
   # Add EPA reference line
   if (epa_line) {
      p_salinity <- p_salinity + 
         geom_hline(yintercept = 0.5, 
                    color = '#002030', 
                    linetype = 2, 
                    linewidth = 0.8) +
         annotate("text",
                  x = min(plot_data$DateTime),
                  y = 0.52,
                  label = "EPA Secondary Standard (0.5 psu)",
                  hjust = 0,
                  vjust = 0,
                  size = 4,
                  colour = "#002030")
   }
   
   # Add time series lines
   p_salinity <- p_salinity + geom_line()
   
   # Apply scales and theme
   p_salinity <- p_salinity + 
      scale_color_manual(values = color_scale, name = NULL) +
      scale_size_manual(values = size_scale, guide = "none") +
      scale_alpha_manual(values = alpha_scale, guide = "none") +
      scale_y_continuous(name = "Salinity (psu)") +
      theme_bw() +
      labs(title = "A) Observed and Predicted Salinity", 
           x = NULL) +
      theme(
         plot.title         = element_text(size = 14, face = 'bold', color = '#002030'),
         axis.title.y       = element_text(size = 12, face = 'bold', colour = "#f58220"),
         axis.text.y        = element_text(colour = "#f58220", size = 10),
         axis.text.x        = element_blank(),
         axis.ticks.x       = element_blank(),
         panel.border       = element_rect(colour = '#002030', fill = NA, linewidth = 1),
         legend.position    = "bottom",
         legend.text        = element_text(size = 10),
         legend.key.width   = unit(1.5, "cm")
      ) +
      guides(color = guide_legend(nrow = 1))
   
   # ============================================================================
   # PANEL 2+: PREDICTOR VARIABLES
   # ============================================================================
   
   # Define nice labels for common predictors
   predictor_labels <- c(
      "log_discharge" = "Log Discharge",
      "RollingDischarge48" = "48-hr Rolling Discharge (m³/s)",
      "log_inflows" = "Log Inflows",
      "RollingInflows90" = "90-day Rolling Inflows (m³/s)",
      "TideRange24" = "24-hr Tide Range (m)",
      "RollingV168" = "168-hr Rolling Velocity (m/s)",
      "WindSpeed" = "Wind Speed (m/s)",
      "WindN" = "Northward Wind (m/s)",
      "DayOfYear" = "Day of Year",
      "quarter" = "Quarter",
      "Norm_InflowDeficit" = "Normalized Inflow Deficit",
      "Norm_PowDischarge" = "Normalized Discharge"
   )
   
   predictor_plots <- list()
   
   for (i in seq_along(predictors)) {
      var_name <- predictors[i]
      
      # Check if variable exists in data
      if (!var_name %in% names(plot_data)) {
         warning(paste("Variable", var_name, "not found in data. Skipping."))
         next
      }
      
      # Get label
      y_label <- ifelse(var_name %in% names(predictor_labels), 
                        predictor_labels[var_name], 
                        var_name)
      
      # Determine if this is the last predictor (for x-axis)
      is_last <- (i == length(predictors))
      
      # Create predictor plot
      p_pred <- ggplot(plot_data, aes(x = DateTime, y = !!sym(var_name)))
      
      # Add highlight rectangle if specified
      if (!is.null(highlight_start) && !is.null(highlight_end)) {
         p_pred <- p_pred + 
            annotate("rect",
                     xmin = highlight_start, 
                     xmax = highlight_end,
                     ymin = -Inf, 
                     ymax = Inf,
                     fill = "#fdb515", 
                     alpha = 0.2)
      }
      
      # Add line
      p_pred <- p_pred + 
         geom_line(color = predictor_color, linewidth = 0.6) +
         labs(
            title = paste0(LETTERS[i + 1], ") ", y_label),
            y = y_label,
            x = if (is_last) "Date" else NULL
         ) +
         theme_bw() +
         theme(
            plot.title    = element_text(size = 14, face = 'bold', color = '#002030'),
            axis.title.y  = element_text(size = 12, face = 'bold', color = predictor_color),
            axis.text.y   = element_text(color = predictor_color, size = 10),
            axis.title.x  = element_text(size = 12, face = 'bold', color = '#002030'),
            axis.text.x   = if (is_last) element_text(size = 10) else element_blank(),
            axis.ticks.x  = if (is_last) element_line() else element_blank(),
            panel.border  = element_rect(colour = '#002030', fill = NA, linewidth = 1)
         )
      
      predictor_plots[[i]] <- p_pred
   }
   
   # Remove NULL plots (from missing variables)
   predictor_plots <- predictor_plots[!sapply(predictor_plots, is.null)]
   
   # ============================================================================
   # COMBINE PLOTS
   # ============================================================================
   
   if (length(predictor_plots) == 0) {
      combined_plot <- p_salinity
   } else {
      # Stack all plots vertically
      combined_plot <- p_salinity
      for (p in predictor_plots) {
         combined_plot <- combined_plot / p
      }
   }
   
   # Add overall title
   overall_title <- title
   if (is.null(overall_title)) {
      if (!is.null(year)) {
         overall_title <- paste("Salinity and Predictors -", year)
      } else if (!is.null(date_range)) {
         overall_title <- paste("Salinity and Predictors -", 
                                format(date_range[1], "%Y-%m-%d"), "to",
                                format(date_range[2], "%Y-%m-%d"))
      } else {
         overall_title <- "Salinity and Predictors"
      }
   }
   
   combined_plot <- combined_plot + 
      plot_annotation(
         title = overall_title,
         theme = theme(
            plot.title = element_text(size = 18, face = 'bold', color = '#002030')
         )
      )
   
   return(combined_plot)
}

# create_salinity_exceedance_plot <- function(data, 
#                                             years = NULL,
#                                             months = NULL, 
#                                             threshold = 1.0,
#                                             plot_title = NULL,
#                                             predictor_vars = c('Norm_InflowDeficit', 'Norm_PowDischarge'),
#                                             single_group = NULL) {
#    
#    # Default filtering: if no years/months specified, use all data
#    filtered_data <- data
#    
#    # Apply year filter if specified
#    if (!is.null(years)) {
#       filtered_data <- filtered_data %>% filter(Year %in% years)
#    }
#    
#    # Apply month filter if specified
#    if (!is.null(months)) {
#       filtered_data <- filtered_data %>% filter(Month %in% months)
#    }
#    
#    # Default title if none provided
#    if (is.null(plot_title)) {
#       year_text <- if (is.null(years)) "All Years" else paste(years, collapse = ", ")
#       month_text <- if (is.null(months)) "All Months" else paste(month.name[months], collapse = "/")
#       plot_title <- paste0("Simple Logistic Regression (Threshold: ", threshold, 
#                            ") - ", year_text, ", ", month_text)
#    }
#    
#    # Panel 1: Predicted Exceedance Probability
#    p1 <- ggplot(filtered_data, aes(x = Date, y = exceedance_probability)) +
#       geom_line() +
#       labs(
#          title = "A) Predicted Exceedance Probability",
#          y = "Probability",
#          x = NULL
#       ) +
#       theme_minimal() + 
#       theme(axis.ticks.x = element_blank()) + 
#       theme(axis.text.x = element_blank())
#    
#    # Handle legend based on single_group parameter
#    if (!is.null(single_group)) {
#       p1 <- p1 + theme(legend.position = "none")
#    } else {
#       p1 <- p1 +
#          theme(legend.position = "bottom", legend.title = element_blank()) +
#          guides(color = guide_legend(nrow = 1))
#    }
#    
#    # Panel 2: Observed Salinity
#    # Create exceedance indicators based on threshold
#    filtered_data$threshold_exceedance <- filtered_data$Salinity > threshold
#    
#    p2 <- ggplot(filtered_data, aes(x = Date, y = Salinity)) +
#       geom_line(color = "darkgrey") +
#       # Add threshold line
#       geom_hline(yintercept = threshold, 
#                  color = "red", linetype = "dashed", alpha = 0.7) +
#       # Highlight exceedances (if actual_exceedance column exists)
#       {if ("actual_exceedance" %in% names(filtered_data)) {
#          geom_point(
#             data = filtered_data %>% filter(actual_exceedance == TRUE),
#             aes(y = Salinity), color = "red", size = 0.8, alpha = 0.8
#          )
#       }} +
#       # Highlight threshold exceedances
#       geom_point(data = filtered_data %>% filter(threshold_exceedance), 
#                  aes(y = Salinity), color = 'darkred', size = 0.8, alpha = 0.8) +
#       labs(
#          title = paste0("B) Observed Salinity (Threshold: ", threshold, ")"),
#          y = "Salinity (psu)",
#          x = NULL
#       ) +
#       theme_minimal() +
#       theme(legend.position = "none") + 
#       theme(axis.ticks.x = element_blank()) + 
#       theme(axis.text.x = element_blank())
#    
#    # Panel 3: Predictor Variables
#    predictor_plots <- list()
#    
#    # Variable labels mapping
#    var_labels <- c(
#       "Norm_InflowDeficit" = "Normalized Inflow Deficit",
#       "Norm_PowDischarge" = "Normalized Discharge",
#       "DayOfYear" = "Day of Year"
#    )
#    
#    for (i in seq_along(predictor_vars)) {
#       var_name <- predictor_vars[i]
#       
#       # Check if variable exists in data
#       if (!var_name %in% names(filtered_data)) {
#          warning(paste("Variable", var_name, "not found in data. Skipping."))
#          next
#       }
#       
#       y_label <- ifelse(var_name %in% names(var_labels), 
#                         var_labels[var_name], var_name)
#       
#       p_pred <- ggplot(filtered_data, aes(x = Date, y = !!sym(var_name))) +
#          geom_line(color = "darkgreen") +
#          labs(
#             title = paste0(LETTERS[i + 2], ") ", y_label),
#             y = y_label,
#             x = if (i == length(predictor_vars)) "Date" else NULL
#          ) +
#          theme_minimal() +
#          theme(legend.position = "none") + 
#          theme(axis.text.x = element_blank())
#       
#       predictor_plots[[i]] <- p_pred
#    }
#    
#    # Remove NULL plots (from missing variables)
#    predictor_plots <- predictor_plots[!sapply(predictor_plots, is.null)]
#    predictor_plots[[2]] + theme(axis.text.x = element_text(size = 14, face = 'bold')) + 
#                           theme(axis.ticks.x = element_text(size = 12, face = 'bold'))
#    
#    # Combine all plots
#    if (length(predictor_plots) == 0) {
#       combined_plot <- p1 / p2
#    } else if (length(predictor_plots) == 1) {
#       combined_plot <- p1 / p2 / predictor_plots[[1]]
#    } else if (length(predictor_plots) == 2) {
#       combined_plot <- p1 / p2 / predictor_plots[[1]] / predictor_plots[[2]]
#    } else {
#       # For more than 2 predictors, arrange them in a grid
#       pred_combined <- wrap_plots(predictor_plots, ncol = 2)
#       combined_plot <- p1 / p2 / pred_combined
#    }
#    
#    # Add overall title
#    combined_plot <- combined_plot + 
#       plot_annotation(title = plot_title,
#                       theme = theme(
#                          plot.title = element_text(size = 16, face = 'bold')
#                       ))
#    
#    return(combined_plot)
# }