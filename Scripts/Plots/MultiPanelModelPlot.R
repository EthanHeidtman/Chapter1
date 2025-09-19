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

create_salinity_exceedance_plot <- function(data, 
                                            years = NULL,
                                            months = NULL, 
                                            threshold = 1.0,
                                            plot_title = NULL,
                                            predictor_vars = c('Norm_InflowDeficit', 'Norm_PowDischarge'),
                                            single_group = NULL) {
   
   # Default filtering: if no years/months specified, use all data
   filtered_data <- data
   
   # Apply year filter if specified
   if (!is.null(years)) {
      filtered_data <- filtered_data %>% filter(Year %in% years)
   }
   
   # Apply month filter if specified
   if (!is.null(months)) {
      filtered_data <- filtered_data %>% filter(Month %in% months)
   }
   
   # Default title if none provided
   if (is.null(plot_title)) {
      year_text <- if (is.null(years)) "All Years" else paste(years, collapse = ", ")
      month_text <- if (is.null(months)) "All Months" else paste(month.name[months], collapse = "/")
      plot_title <- paste0("Simple Logistic Regression (Threshold: ", threshold, 
                           ") - ", year_text, ", ", month_text)
   }
   
   # Panel 1: Predicted Exceedance Probability
   p1 <- ggplot(filtered_data, aes(x = Date, y = exceedance_probability)) +
      geom_line() +
      labs(
         title = "A) Predicted Exceedance Probability",
         y = "Probability",
         x = NULL
      ) +
      theme_minimal() + 
      theme(axis.ticks.x = element_blank()) + 
      theme(axis.text.x = element_blank())
   
   # Handle legend based on single_group parameter
   if (!is.null(single_group)) {
      p1 <- p1 + theme(legend.position = "none")
   } else {
      p1 <- p1 +
         theme(legend.position = "bottom", legend.title = element_blank()) +
         guides(color = guide_legend(nrow = 1))
   }
   
   # Panel 2: Observed Salinity
   # Create exceedance indicators based on threshold
   filtered_data$threshold_exceedance <- filtered_data$Salinity > threshold
   
   p2 <- ggplot(filtered_data, aes(x = Date, y = Salinity)) +
      geom_line(color = "darkgrey") +
      # Add threshold line
      geom_hline(yintercept = threshold, 
                 color = "red", linetype = "dashed", alpha = 0.7) +
      # Highlight exceedances (if actual_exceedance column exists)
      {if ("actual_exceedance" %in% names(filtered_data)) {
         geom_point(
            data = filtered_data %>% filter(actual_exceedance == TRUE),
            aes(y = Salinity), color = "red", size = 0.8, alpha = 0.8
         )
      }} +
      # Highlight threshold exceedances
      geom_point(data = filtered_data %>% filter(threshold_exceedance), 
                 aes(y = Salinity), color = 'darkred', size = 0.8, alpha = 0.8) +
      labs(
         title = paste0("B) Observed Salinity (Threshold: ", threshold, ")"),
         y = "Salinity (psu)",
         x = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "none") + 
      theme(axis.ticks.x = element_blank()) + 
      theme(axis.text.x = element_blank())
   
   # Panel 3+: Predictor Variables
   predictor_plots <- list()
   
   # Variable labels mapping
   var_labels <- c(
      "Norm_InflowDeficit" = "Normalized Inflow Deficit",
      "Norm_PowDischarge" = "Normalized Discharge",
      "DayOfYear" = "Day of Year"
   )
   
   for (i in seq_along(predictor_vars)) {
      var_name <- predictor_vars[i]
      
      # Check if variable exists in data
      if (!var_name %in% names(filtered_data)) {
         warning(paste("Variable", var_name, "not found in data. Skipping."))
         next
      }
      
      y_label <- ifelse(var_name %in% names(var_labels), 
                        var_labels[var_name], var_name)
      
      p_pred <- ggplot(filtered_data, aes(x = Date, y = !!sym(var_name))) +
         geom_line(color = "darkgreen") +
         labs(
            title = paste0(LETTERS[i + 2], ") ", y_label),
            y = y_label,
            x = if (i == length(predictor_vars)) "Date" else NULL
         ) +
         theme_minimal() +
         theme(legend.position = "none") + 
         theme(axis.text.x = element_blank())
      
      predictor_plots[[i]] <- p_pred
   }
   
   # Remove NULL plots (from missing variables)
   predictor_plots <- predictor_plots[!sapply(predictor_plots, is.null)]
   predictor_plots[[2]] + theme(axis.text.x = element_text(size = 14, face = 'bold')) + 
                          theme(axis.ticks.x = element_text(size = 12, face = 'bold'))
   
   # Combine all plots
   if (length(predictor_plots) == 0) {
      combined_plot <- p1 / p2
   } else if (length(predictor_plots) == 1) {
      combined_plot <- p1 / p2 / predictor_plots[[1]]
   } else if (length(predictor_plots) == 2) {
      combined_plot <- p1 / p2 / predictor_plots[[1]] / predictor_plots[[2]]
   } else {
      # For more than 2 predictors, arrange them in a grid
      pred_combined <- wrap_plots(predictor_plots, ncol = 2)
      combined_plot <- p1 / p2 / pred_combined
   }
   
   # Add overall title
   combined_plot <- combined_plot + 
      plot_annotation(title = plot_title,
                      theme = theme(
                         plot.title = element_text(size = 16, face = 'bold')
                      ))
   
   return(combined_plot)
}