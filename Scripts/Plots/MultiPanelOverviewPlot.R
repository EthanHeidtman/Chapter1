# =============================================================================
# Script Name:    MultiPanelOverviewPlot.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-28
# Last Updated:   2025-08-28
# Description:    Takes the output of the rolling window experiments and generates
#                 a plot that contains a panel for the predicted exceedance,
#                 the raw salinity, and each of the predictors
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(purrr)

plot_multi_panel_overview <- function(data, 
                                      start_date = NULL, 
                                      end_date = NULL,
                                      group_var = "distribution_family",
                                      predictor_vars = c("Norm_InflowDeficit", "Norm_PowDischarge"),
                                      single_group = NULL) {
   
   # Filter by date if specified
   if (!is.null(start_date) && !is.null(end_date)) {
      data <- data %>%
         filter(DateTime >= as.POSIXct(start_date) & 
                   DateTime <= as.POSIXct(end_date))
   }
   
   # Filter by single group if specified
   if (!is.null(single_group)) {
      data <- data %>% filter(!!sym(group_var) == single_group)
   }
   
   title_text <- "Model Output Breakdown: "
   if (!is.null(start_date) && !is.null(end_date)) {
      title_text <- paste0(title_text, 
                           " (", format(as.Date(start_date), "%b %Y"), ')')
   }
   if (!is.null(single_group)) {
      title_text <- paste0(title_text, " — Salinity Threshold = ", single_group)
   } else {
      title_text <- paste0(title_text, " — All groups")
   }
   
   # Panel 1: Predicted Exceedance Probability
   p1 <- ggplot(data, aes(x = DateTime, y = exceedance_probability)) +
      geom_line(aes(color = !!sym(group_var))) +
      labs(
         title = "A) Predicted Exceedance Probability",
         y = "Probability",
         x = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(color = guide_legend(nrow = 1))
   
   if (!is.null(single_group)) {
      p1 <- p1 + theme(legend.position = "none")
   } else {
      p1 <- p1 +
         theme(legend.position = "bottom", legend.title = element_blank()) +
         guides(color = guide_legend(nrow = 1))
   }
   
   # Panel 2: Observed Salinity
   p2 <- ggplot(data, aes(x = DateTime, y = Salinity)) +
      geom_line(color = "darkgrey") +
      # Add threshold line
      geom_hline(yintercept = unique(data$salinity_threshold)[1], 
                 color = "red", linetype = "dashed", alpha = 0.7) +
      # Highlight exceedances
      geom_point(
         data = data %>% filter(actual_exceedance == TRUE),
         aes(y = Salinity), color = "red", size = 0.8, alpha = 0.8
      ) +
      geom_point(data = data %>% filter(Salinity > 1.0), 
                 aes(y = Salinity), color = 'darkred', size = 0.8, alpha = 0.8) + 
      geom_hline(yintercept = 1.0, 
                 color = "darkred", linetype = "dashed", alpha = 0.7) +
      labs(
         title = "B) Observed Salinity",
         y = "Salinity (psu)",
         x = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "none")
   
   # Panel 3 & 4: Predictor Variables
   predictor_plots <- list()
   
   for (i in seq_along(predictor_vars)) {
      var_name <- predictor_vars[i]
      
      var_labels <- c(
         "Norm_InflowDeficit" = "Normalized Inflow Deficit",
         "Norm_PowDischarge" = "Normalized Discharge",
         "DayOfYear" = "Day of Year"
      )
      
      y_label <- ifelse(var_name %in% names(var_labels), 
                        var_labels[var_name], var_name)
      
      p_pred <- ggplot(data, aes(x = DateTime, y = !!sym(var_name))) +
         geom_line(color = "darkgreen") +
         labs(
            title = paste0(LETTERS[i + 2], ") ", y_label),
            y = y_label,
            x = if (i == length(predictor_vars)) "Date" else NULL
         ) +
         theme_minimal() +
         theme(legend.position = "none")
      
      predictor_plots[[i]] <- p_pred
   }
   
   # Combine all plots
   if (length(predictor_plots) == 1) {
      combined_plot <- p1 / p2 / predictor_plots[[1]]
   } else if (length(predictor_plots) == 2) {
      combined_plot <- p1 / p2 / predictor_plots[[1]] / predictor_plots[[2]]
   } else {
      # For more than 2 predictors, arrange them in a grid
      pred_combined <- wrap_plots(predictor_plots, ncol = 2)
      combined_plot <- p1 / p2 / pred_combined
   }
   
   combined_plot <- combined_plot + 
      plot_annotation(title = title_text,
                      theme = theme(
                         plot.title = element_text(size = 16, face = 'bold')
                      ))
   
   return(combined_plot)
}
