# Get October 2016 data for comparison
oct_data <- stage1_data %>%
   mutate(DateTime = as.POSIXct(DateTime)) %>%
   filter(month(DateTime) == 10 & year(DateTime) == 2016) %>%
   mutate(Actual_Exceedance = Salinity > threshold)# Simple Plotting Suite for POT Model Proof of Concept
# Focus on clear, readable visualizations

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)

# Simple, clean theme
simple_theme <- function() {
   theme_minimal(base_size = 14) +
      theme(
         panel.grid.minor = element_blank(),
         panel.grid.major = element_line(color = "grey90", size = 0.5),
         strip.text = element_text(face = "bold", size = 12),
         legend.position = "bottom",
         axis.title = element_text(face = "bold", size = 12),
         plot.title = element_text(face = "bold", size = 16),
         plot.subtitle = element_text(size = 12, color = "grey40"),
         legend.title = element_text(face = "bold")
      )
}

# ============================================================================
# 1. SIMPLE MODEL PERFORMANCE COMPARISON
# ============================================================================

#' Simple bar chart comparing model performance
plot_simple_performance <- function(metrics_df, metrics_to_plot = c("accuracy", "auc", 'precision', 'recall', 'f1')) {
   
   plot_data <- metrics_df %>%
      filter(Metric_Name %in% metrics_to_plot, 
             Metric_Type %in% c("classification", "hybrid")) %>%
      mutate(
         Distribution = gsub("TailDistributions\\.", "", Distribution),
         Distribution = gsub("_", " ", Distribution),
         Distribution = tools::toTitleCase(Distribution),
         Metric_Name = tools::toTitleCase(Metric_Name)
      )
   
   ggplot(plot_data, aes(x = reorder(Distribution, Value), y = Value, fill = Metric_Type)) +
      geom_col(position = "dodge", alpha = 0.8, color = "white", size = 0.5) +
      scale_fill_manual(values = c("classification" = "steelblue", "hybrid" = "orange"),
                        labels = c("classification" = "Stage 1 Only", "hybrid" = "Two-Stage Model")) +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      coord_flip() +
      facet_wrap(~ Metric_Name, scales = "free_x") +
      labs(
         title = "Model Performance Comparison",
         subtitle = "Comparing Stage 1 classification vs. full two-stage hybrid model",
         x = "Distribution",
         y = "Metric Value",
         fill = "Model Type"
      ) +
      simple_theme()
}

# ============================================================================
# 2. OCTOBER 2016 EVENT ANALYSIS
# ============================================================================

#' Focus on October 2016 salinity event
plot_october_2016_event <- function(stage1_data, threshold = 0.8, 
                                    best_distribution = "gpd", 
                                    extend_days = 7) {
   
   # Define October 2016 period with some buffer
   start_date <- as.Date("2016-10-01") - extend_days
   end_date <- as.Date("2016-10-31") + extend_days
   
   # Find the actual column name (case insensitive)
   pred_col <- names(stage1_data)[grepl(paste0("hybrid_prob_", best_distribution), 
                                        names(stage1_data), ignore.case = TRUE)]
   
   if(length(pred_col) == 0) {
      stop(paste("No prediction column found for distribution:", best_distribution))
   }
   
   # Filter and prepare data
   event_data <- stage1_data %>%
      mutate(DateTime = as.POSIXct(DateTime)) %>%
      filter(as.Date(DateTime) >= start_date & as.Date(DateTime) <= end_date) %>%
      mutate(
         Date = as.Date(DateTime),
         Actual_Exceedance = Salinity > threshold,
         Best_Prediction = !!sym(pred_col[1])
      ) %>%
      filter(!is.na(Best_Prediction))
   
   # Create the plot
   p <- ggplot(event_data, aes(x = DateTime)) +
      # Salinity time series
      geom_line(aes(y = Salinity), color = "blue", size = 1, alpha = 0.8) +
      
      # Threshold line
      geom_hline(yintercept = threshold, color = "red", linetype = "dashed", size = 1) +
      
      # Highlight actual exceedances
      geom_point(data = filter(event_data, Actual_Exceedance), 
                 aes(y = Salinity), color = "red", size = 2, alpha = 0.8) +
      
      # Add prediction intensity as color background
      geom_point(aes(y = Salinity, alpha = Best_Prediction), 
                 color = "orange", size = 1.5) +
      
      scale_alpha_continuous(name = "Predicted\nExceedance\nProbability", 
                             range = c(0.1, 0.9),
                             labels = percent_format()) +
      
      scale_x_datetime(date_labels = "%b %d", date_breaks = "3 days") +
      
      # Highlight October 2016 period
      annotate("rect", xmin = as.POSIXct("2016-10-01"), xmax = as.POSIXct("2016-10-31"),
               ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow") +
      
      labs(
         title = paste("October 2016 Salinity Event - Model Performance: ", tools::toTitleCase(best_distribution), "Distribution"),
         subtitle = paste("Red points = Actual exceedances above", threshold, "psu | Orange intensity = Predicted probability"),
         x = "Date",
         y = "Salinity (psu)"
      ) +
      simple_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   return(p)
}

#' Detailed view of just October 2016
plot_october_2016_detailed <- function(stage1_data, threshold = 0.8, 
                                       distributions = c("gpd", "lognormal", "gamma", 'loglogistic', 'burr', 'gengamma')) {
   
   # Filter to October 2016 only
   oct_data <- stage1_data %>%
      mutate(DateTime = as.POSIXct(DateTime)) %>%
      filter(month(DateTime) == 10 & year(DateTime) == 2016) %>%
      mutate(
         Actual_Exceedance = Salinity > threshold,
         Date = as.Date(DateTime)
      )
   
   # Get prediction columns (case insensitive matching)
   pred_cols <- c()
   for(dist in distributions) {
      matching_cols <- names(oct_data)[grepl(paste0("hybrid_prob_", dist), 
                                             names(oct_data), ignore.case = TRUE)]
      pred_cols <- c(pred_cols, matching_cols)
   }
   pred_cols <- unique(pred_cols)
   
   # If no columns found, use all available hybrid_prob columns
   if(length(pred_cols) == 0) {
      pred_cols <- names(oct_data)[grepl("^hybrid_prob_", names(oct_data), ignore.case = TRUE)]
   }
   
   # Reshape for plotting
   plot_data <- oct_data %>%
      select(DateTime, Date, Salinity, Actual_Exceedance, all_of(pred_cols)) %>%
      pivot_longer(cols = all_of(pred_cols), 
                   names_to = "Distribution", 
                   values_to = "Predicted_Prob") %>%
      mutate(
         Distribution = gsub("hybrid_prob_", "", Distribution),
         Distribution = tools::toTitleCase(Distribution)
      ) %>%
      filter(!is.na(Predicted_Prob))
   
   ggplot(plot_data, aes(x = DateTime)) +
      # Background for actual exceedances
      geom_rect(data = filter(oct_data, Actual_Exceedance),
                aes(xmin = DateTime - hours(2), xmax = DateTime + hours(2)),
                ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red", inherit.aes = FALSE) +
      
      # Prediction lines
      geom_line(aes(y = Predicted_Prob, color = Distribution), size = 1.2, alpha = 0.8) +
      
      scale_color_brewer(type = "qual", palette = "Dark2") +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      scale_x_datetime(date_labels = "%b %d, %Y", date_breaks = "3 days") +
      
      labs(
         title = "October 2016 Event - Exceedance Probability Predictions",
         subtitle = "Red background shows periods of actual exceedance",
         x = "Date",
         y = "Predicted Exceedance Probability",
         color = "Distribution"
      ) +
      simple_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ============================================================================
# 3. SIMPLE TIME SERIES OVERVIEW
# ============================================================================

#' Simple overview of model performance over time
plot_simple_timeseries <- function(stage1_data, threshold = 0.8, 
                                   best_distribution = "gpd",
                                   sample_points = 1000) {
   
   # Sample data for readability if dataset is large
   if (nrow(stage1_data) > sample_points) {
      plot_data <- stage1_data %>%
         slice_sample(n = sample_points) %>%
         arrange(DateTime)
   } else {
      plot_data <- stage1_data
   }
   
   # Find the actual column name (case insensitive)
   pred_col <- names(stage1_data)[grepl(paste0("hybrid_prob_", best_distribution), 
                                        names(stage1_data), ignore.case = TRUE)]
   
   if(length(pred_col) == 0) {
      stop(paste("No prediction column found for distribution:", best_distribution))
   }
   
   plot_data <- plot_data %>%
      mutate(
         DateTime = as.POSIXct(DateTime),
         Actual_Exceedance = Salinity > threshold,
         Best_Prediction = !!sym(pred_col[1]),
         Year = year(DateTime)
      ) %>%
      filter(!is.na(Best_Prediction))
   
   ggplot(plot_data, aes(x = DateTime)) +
      # Predictions as line
      geom_line(aes(y = Best_Prediction), color = "blue", alpha = 0.7) +
      
      # Actual exceedances as points
      geom_point(data = filter(plot_data, Actual_Exceedance),
                 aes(y = 1), color = "red", size = 1.5, alpha = 0.8) +
      
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
      
      labs(
         title = paste("Model Performance Over Time -", tools::toTitleCase(best_distribution), "Distribution"),
         subtitle = "Blue line = Predicted exceedance probability | Red points = Actual exceedances",
         x = "Year",
         y = "Exceedance Probability"
      ) +
      simple_theme()
}

# ============================================================================
# 4. SIMPLE MODEL COMPARISON
# ============================================================================

#' Compare top 3 distributions side by side
plot_top_distributions <- function(stage1_data, metrics_df, threshold = 0.8, n_top = 6) {
   
   # Find top performing distributions and their actual column names
   top_dist_names <- metrics_df %>%
      filter(Metric_Name == "f1", Metric_Type == "hybrid") %>%
      arrange(desc(Value)) %>%
      slice_head(n = n_top) %>%
      mutate(Distribution = gsub("TailDistributions\\.", "", Distribution)) %>%
      pull(Distribution)
   
   # Find actual columns (case insensitive)
   pred_cols <- c()
   for(dist in top_dist_names) {
      matching_cols <- names(oct_data)[grepl(paste0("hybrid_prob_", dist), 
                                             names(oct_data), ignore.case = TRUE)]
      pred_cols <- c(pred_cols, matching_cols)
   }
   pred_cols <- unique(pred_cols)
   
   # If no specific columns found, use all available
   if(length(pred_cols) == 0) {
      pred_cols <- names(oct_data)[grepl("^hybrid_prob_", names(oct_data), ignore.case = TRUE)]
      pred_cols <- pred_cols[1:min(n_top, length(pred_cols))]  # Limit to n_top
   }
   
   comparison_data <- oct_data %>%
      select(DateTime, Salinity, Actual_Exceedance, all_of(pred_cols)) %>%
      pivot_longer(cols = all_of(pred_cols), 
                   names_to = "Distribution", 
                   values_to = "Predicted_Prob") %>%
      mutate(
         Distribution = gsub("hybrid_prob_", "", Distribution),
         Distribution = tools::toTitleCase(Distribution)
      ) %>%
      filter(!is.na(Predicted_Prob))
   
   ggplot(comparison_data, aes(x = DateTime)) +
      geom_line(aes(y = Predicted_Prob), color = "blue", alpha = 0.8) +
      geom_point(data = filter(comparison_data, Actual_Exceedance),
                 aes(y = 1), color = "red", size = 1, alpha = 0.7) +
      
      facet_wrap(~ Distribution, ncol = 2) +
      
      scale_y_continuous(labels = percent_format()) +
      scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
      
      labs(
         title = "Top 3 Distributions - October 2016 Performance",
         subtitle = "Blue line = Predictions | Red points = Actual exceedances",
         x = "Date",
         y = "Exceedance Probability"
      ) +
      simple_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ============================================================================
# 5. PROOF OF CONCEPT SUMMARY
# ============================================================================

#' Create a simple proof-of-concept dashboard
create_simple_dashboard <- function(stage1_data, metrics_df, threshold = 0.8) {
   
   # Find best distribution and get actual column name
   best_dist_row <- metrics_df %>%
      filter(Metric_Name == "f1", Metric_Type == "hybrid") %>%
      arrange(desc(Value)) %>%
      slice_head(n = 1)
   
   best_dist_clean <- gsub("TailDistributions\\.", "", best_dist_row$Distribution)
   
   # Find the actual column name in stage1_data (case insensitive)
   pred_col <- names(stage1_data)[grepl(paste0("hybrid_prob_", best_dist_clean), 
                                        names(stage1_data), ignore.case = TRUE)]
   
   if(length(pred_col) == 0) {
      stop(paste("No prediction column found for distribution:", best_dist_clean))
   }
   
   best_dist <- gsub("hybrid_prob_", "", pred_col[1])  # Use actual column name
   
   cat("Best performing distribution:", best_dist_clean, "\n")
   cat("Using column:", pred_col[1], "\n")
   
   dashboard <- list(
      # Overall performance
      performance = plot_simple_performance(metrics_df, c("accuracy", "auc", 'precision', 'recall', 'f1')),
      
      # October 2016 event focus
      october_event = plot_october_2016_event(stage1_data, threshold, best_dist),
      
      # Detailed October view
      october_detailed = plot_october_2016_detailed(stage1_data, threshold),
      
      # Overall time series
      timeseries = plot_simple_timeseries(stage1_data, threshold, best_dist),
      
      # Top 3 comparison
      top_comparison = plot_top_distributions(stage1_data, metrics_df, threshold)
   )
   
   return(dashboard)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# Simple usage:
# dashboard <- create_simple_dashboard(stage1_data, metrics_df, threshold = 0.8)
# 
# # Print individual plots
# print(dashboard$performance)
# print(dashboard$october_event)
# print(dashboard$october_detailed)
# 
# # Save plots
# ggsave("performance.png", dashboard$performance, width = 10, height = 6, dpi = 300)
# ggsave("october_2016_event.png", dashboard$october_event, width = 12, height = 6, dpi = 300)
# ggsave("october_detailed.png", dashboard$october_detailed, width = 12, height = 6, dpi = 300)



#' # Comprehensive Plotting Suite for Two-Stage POT Salinity Model
#' # Author: [Your Name]
#' # Description: Flexible visualization tools for POT model analysis
#' 
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#' library(scales)
#' library(RColorBrewer)
#' library(gridExtra)
#' library(lubridate)
#' library(viridis)
#' library(stringr)
#' library(purrr)
#' 
#' # ============================================================================
#' # CONFIGURATION AND THEMES
#' # ============================================================================
#' 
#' # Custom theme for consistent styling
#' pot_theme <- function(base_size = 12) {
#'    theme_minimal(base_size = base_size) +
#'       theme(
#'          panel.grid.minor = element_blank(),
#'          panel.grid.major = element_line(color = "grey90", size = 0.3),
#'          strip.background = element_rect(fill = "grey95", color = "grey80"),
#'          strip.text = element_text(face = "bold", size = rel(0.9)),
#'          legend.position = "bottom",
#'          legend.title = element_text(face = "bold"),
#'          axis.title = element_text(face = "bold"),
#'          plot.title = element_text(face = "bold", size = rel(1.2)),
#'          plot.subtitle = element_text(color = "grey40"),
#'          axis.text.x = element_text(angle = 45, hjust = 1)
#'       )
#' }
#' 
#' # Color palettes for different distribution types
#' get_distribution_colors <- function(distributions) {
#'    n_dist <- length(unique(distributions))
#'    if (n_dist <= 8) {
#'       colors <- brewer.pal(max(3, n_dist), "Dark2")
#'    } else {
#'       colors <- viridis_discrete(n_dist)
#'    }
#'    names(colors) <- unique(distributions)
#'    return(colors)
#' }
#' 
#' # Extract distribution name from column names
#' extract_distribution_name <- function(col_name) {
#'    gsub("hybrid_prob_", "", col_name)
#' }
#' 
#' # ============================================================================
#' # METRIC COMPARISON PLOTS
#' # ============================================================================
#' 
#' #' Plot metric comparisons across distributions
#' #' 
#' #' @param metrics_df Data frame with Distribution, Metric_Type, Sub_Type, Metric_Name, Value columns
#' #' @param metric_type Filter for specific metric type ("classification", "hybrid", etc.)
#' #' @param metrics_to_plot Vector of metric names to include
#' #' @param facet_by Column to facet by (default: "Metric_Type")
#' #' @param show_error_bars Include error bars if std columns available
#' #' @return ggplot object
#' plot_metric_comparison <- function(metrics_df, 
#'                                    metric_type = NULL,
#'                                    metrics_to_plot = c("accuracy", "auc"),
#'                                    facet_by = "Metric_Type",
#'                                    show_error_bars = TRUE) {
#'    
#'    # Filter data
#'    plot_data <- metrics_df
#'    if (!is.null(metric_type)) {
#'       plot_data <- plot_data %>% filter(Metric_Type == !!metric_type)
#'    }
#'    
#'    plot_data <- plot_data %>%
#'       filter(Metric_Name %in% metrics_to_plot) %>%
#'       mutate(
#'          Distribution_Clean = gsub("TailDistributions\\.", "", Distribution),
#'          Distribution_Clean = gsub("_", " ", Distribution_Clean)
#'       )
#'    
#'    # Get error bar data if available
#'    if (show_error_bars) {
#'       error_data <- metrics_df %>%
#'          filter(grepl("_std$", Metric_Name)) %>%
#'          mutate(
#'             Base_Metric = gsub("_std$", "", Metric_Name),
#'             Distribution_Clean = gsub("TailDistributions\\.", "", Distribution),
#'             Distribution_Clean = gsub("_", " ", Distribution_Clean)
#'          ) %>%
#'          filter(Base_Metric %in% metrics_to_plot) %>%
#'          select(Distribution_Clean, Metric_Type, Base_Metric, Std_Value = Value)
#'       
#'       plot_data <- plot_data %>%
#'          left_join(error_data, 
#'                    by = c("Distribution_Clean", "Metric_Type", "Metric_Name" = "Base_Metric"))
#'    }
#'    
#'    # Create base plot
#'    p <- ggplot(plot_data, aes(x = Distribution_Clean, y = Value, fill = Distribution_Clean)) +
#'       geom_col(alpha = 0.8, color = "white", size = 0.5) +
#'       scale_fill_manual(values = get_distribution_colors(plot_data$Distribution_Clean)) +
#'       labs(
#'          title = "Model Performance Comparison Across Distributions",
#'          subtitle = paste("Metrics:", paste(metrics_to_plot, collapse = ", ")),
#'          x = "Distribution",
#'          y = "Metric Value",
#'          fill = "Distribution"
#'       ) +
#'       pot_theme() +
#'       theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'    
#'    # Add error bars if available
#'    if (show_error_bars && "Std_Value" %in% names(plot_data)) {
#'       p <- p + geom_errorbar(
#'          aes(ymin = Value - Std_Value, ymax = Value + Std_Value),
#'          width = 0.3, alpha = 0.7
#'       )
#'    }
#'    
#'    # Add faceting
#'    if (facet_by == "Metric_Name") {
#'       p <- p + facet_wrap(~ Metric_Name, scales = "free_y")
#'    } else if (facet_by == "Metric_Type") {
#'       p <- p + facet_wrap(~ Metric_Type, scales = "free_y")
#'    } else if (facet_by == "both") {
#'       p <- p + facet_grid(Metric_Type ~ Metric_Name, scales = "free_y")
#'    }
#'    
#'    return(p)
#' }
#' 
#' #' Create radar chart for multi-metric comparison
#' #' 
#' #' @param metrics_df Data frame with metrics
#' #' @param metric_type Filter for specific metric type
#' #' @param metrics_to_plot Vector of metrics to include
#' #' @return ggplot object
#' plot_metric_radar <- function(metrics_df,
#'                               metric_type = "classification",
#'                               metrics_to_plot = c("accuracy", "auc", "precision", "recall")) {
#'    
#'    # Prepare data
#'    radar_data <- metrics_df %>%
#'       filter(Metric_Type == !!metric_type, 
#'              Metric_Name %in% metrics_to_plot) %>%
#'       mutate(
#'          Distribution_Clean = gsub("TailDistributions\\.", "", Distribution),
#'          Distribution_Clean = gsub("_", " ", Distribution_Clean)
#'       ) %>%
#'       select(Distribution_Clean, Metric_Name, Value) %>%
#'       pivot_wider(names_from = Metric_Name, values_from = Value) %>%
#'       pivot_longer(-Distribution_Clean, names_to = "Metric", values_to = "Value")
#'    
#'    # Create radar plot
#'    ggplot(radar_data, aes(x = Metric, y = Value, color = Distribution_Clean, group = Distribution_Clean)) +
#'       geom_polygon(alpha = 0.2, aes(fill = Distribution_Clean)) +
#'       geom_point(size = 3) +
#'       geom_line(size = 1) +
#'       coord_polar() +
#'       scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
#'       scale_color_manual(values = get_distribution_colors(unique(radar_data$Distribution_Clean))) +
#'       scale_fill_manual(values = get_distribution_colors(unique(radar_data$Distribution_Clean))) +
#'       labs(
#'          title = paste("Multi-Metric Performance Radar Chart:", metric_type),
#'          color = "Distribution",
#'          fill = "Distribution"
#'       ) +
#'       pot_theme() +
#'       theme(
#'          axis.text.x = element_text(angle = 0),
#'          axis.title = element_blank()
#'       )
#' }
#' 
#' # ============================================================================
#' # TIME SERIES PLOTS
#' # ============================================================================
#' 
#' #' Plot time series of actual vs predicted exceedance probabilities
#' #' 
#' #' @param stage1_data Data frame with DateTime, Salinity, and hybrid_prob_* columns
#' #' @param threshold Salinity threshold for exceedance (default: 0.8)
#' #' @param distributions Vector of distributions to plot (NULL for all)
#' #' @param date_range Vector of two dates to zoom in (optional)
#' #' @param facet_distributions Whether to facet by distribution
#' #' @return ggplot object
#' plot_exceedance_timeseries <- function(stage1_data,
#'                                        threshold = 0.8,
#'                                        distributions = NULL,
#'                                        date_range = NULL,
#'                                        facet_distributions = FALSE) {
#'    
#'    # Create actual exceedance indicator
#'    plot_data <- stage1_data %>%
#'       mutate(
#'          Actual_Exceedance = as.numeric(Salinity > threshold),
#'          DateTime = as.POSIXct(DateTime)
#'       )
#'    
#'    # Filter date range if specified
#'    if (!is.null(date_range)) {
#'       plot_data <- plot_data %>%
#'          filter(DateTime >= as.POSIXct(date_range[1]) & 
#'                    DateTime <= as.POSIXct(date_range[2]))
#'    }
#'    
#'    # Get hybrid probability columns
#'    hybrid_cols <- names(plot_data)[grepl("^hybrid_prob_", names(plot_data))]
#'    
#'    if (!is.null(distributions)) {
#'       target_cols <- paste0("hybrid_prob_", distributions)
#'       hybrid_cols <- hybrid_cols[hybrid_cols %in% target_cols]
#'    }
#'    
#'    # Reshape data for plotting
#'    ts_data <- plot_data %>%
#'       select(DateTime, Salinity, Actual_Exceedance, all_of(hybrid_cols)) %>%
#'       pivot_longer(
#'          cols = all_of(hybrid_cols),
#'          names_to = "Distribution",
#'          values_to = "Predicted_Prob"
#'       ) %>%
#'       mutate(
#'          Distribution = extract_distribution_name(Distribution),
#'          Distribution = gsub("_", " ", Distribution)
#'       ) %>%
#'       filter(!is.na(Predicted_Prob))
#'    
#'    # Base plot
#'    p <- ggplot(ts_data) +
#'       geom_line(aes(x = DateTime, y = Predicted_Prob, color = Distribution), 
#'                 alpha = 0.8, size = 0.7) +
#'       geom_point(aes(x = DateTime, y = Actual_Exceedance), 
#'                  alpha = 0.6, size = 0.8, color = "red") +
#'       scale_color_manual(values = get_distribution_colors(unique(ts_data$Distribution))) +
#'       scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
#'       labs(
#'          title = "Actual vs Predicted Salinity Exceedance",
#'          subtitle = paste("Threshold:", threshold, "psu | Red points = Actual exceedances"),
#'          x = "Date",
#'          y = "Exceedance Probability",
#'          color = "Distribution"
#'       ) +
#'       pot_theme()
#'    
#'    # Add faceting if requested
#'    if (facet_distributions) {
#'       p <- p + facet_wrap(~ Distribution, ncol = 2)
#'    }
#'    
#'    return(p)
#' }
#' 
#' #' Plot salinity time series with threshold and predictions
#' #' 
#' #' @param stage1_data Data frame with salinity data
#' #' @param threshold Salinity threshold
#' #' @param distribution Single distribution to show predictions for
#' #' @param date_range Optional date range
#' #' @return ggplot object
#' plot_salinity_with_predictions <- function(stage1_data,
#'                                            threshold = 0.8,
#'                                            distribution = "gpd",
#'                                            date_range = NULL) {
#'    
#'    plot_data <- stage1_data %>%
#'       mutate(DateTime = as.POSIXct(DateTime))
#'    
#'    if (!is.null(date_range)) {
#'       plot_data <- plot_data %>%
#'          filter(DateTime >= as.POSIXct(date_range[1]) & 
#'                    DateTime <= as.POSIXct(date_range[2]))
#'    }
#'    
#'    pred_col <- paste0("hybrid_prob_", distribution)
#'    
#'    if (!pred_col %in% names(plot_data)) {
#'       stop(paste("Distribution", distribution, "not found in data"))
#'    }
#'    
#'    # Create prediction intensity categories
#'    plot_data <- plot_data %>%
#'       mutate(
#'          Pred_Prob = !!sym(pred_col),
#'          Pred_Category = case_when(
#'             is.na(Pred_Prob) ~ "No Prediction",
#'             Pred_Prob < 0.1 ~ "Very Low (< 10%)",
#'             Pred_Prob < 0.3 ~ "Low (10-30%)",
#'             Pred_Prob < 0.7 ~ "Medium (30-70%)",
#'             TRUE ~ "High (> 70%)"
#'          ),
#'          Pred_Category = factor(Pred_Category, 
#'                                 levels = c("No Prediction", "Very Low (< 10%)", 
#'                                            "Low (10-30%)", "Medium (30-70%)", "High (> 70%)"))
#'       )
#'    
#'    ggplot(plot_data, aes(x = DateTime)) +
#'       geom_line(aes(y = Salinity), color = "blue", alpha = 0.7) +
#'       geom_point(aes(y = Salinity, color = Pred_Category), alpha = 0.8, size = 1) +
#'       geom_hline(yintercept = threshold, linetype = "dashed", color = "red", size = 1) +
#'       scale_color_manual(
#'          values = c("grey80", "#2E8B57", "#FFD700", "#FF8C00", "#FF4500"),
#'          name = "Exceedance\nProbability"
#'       ) +
#'       labs(
#'          title = paste("Salinity Time Series with Exceedance Predictions:", str_to_title(distribution)),
#'          subtitle = paste("Red dashed line: threshold =", threshold, "psu"),
#'          x = "Date",
#'          y = "Salinity (psu)"
#'       ) +
#'       pot_theme()
#' }
#' 
#' # ============================================================================
#' # DIAGNOSTIC PLOTS
#' # ============================================================================
#' 
#' #' Plot prediction vs actual scatter with performance metrics
#' #' 
#' #' @param stage1_data Data frame with data
#' #' @param threshold Salinity threshold
#' #' @param distribution Distribution to analyze
#' #' @return ggplot object
#' plot_prediction_scatter <- function(stage1_data, threshold = 0.8, distribution = "gpd") {
#'    
#'    pred_col <- paste0("hybrid_prob_", distribution)
#'    
#'    plot_data <- stage1_data %>%
#'       mutate(
#'          Actual_Exceedance = as.numeric(Salinity > threshold),
#'          Predicted_Prob = !!sym(pred_col)
#'       ) %>%
#'       filter(!is.na(Predicted_Prob)) %>%
#'       mutate(
#'          Prob_Bin = cut(Predicted_Prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
#'       )
#'    
#'    # Calculate calibration data
#'    calibration_data <- plot_data %>%
#'       group_by(Prob_Bin) %>%
#'       summarise(
#'          Mean_Predicted = mean(Predicted_Prob, na.rm = TRUE),
#'          Observed_Rate = mean(Actual_Exceedance, na.rm = TRUE),
#'          Count = n(),
#'          .groups = "drop"
#'       ) %>%
#'       filter(Count >= 5)  # Only include bins with sufficient data
#'    
#'    # Main scatter plot
#'    p1 <- ggplot(plot_data, aes(x = Predicted_Prob, y = Actual_Exceedance)) +
#'       geom_jitter(alpha = 0.3, height = 0.05, width = 0) +
#'       geom_smooth(method = "loess", se = TRUE, color = "red") +
#'       geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
#'       labs(
#'          title = paste("Prediction Calibration:", str_to_title(distribution)),
#'          x = "Predicted Probability",
#'          y = "Actual Exceedance (0/1)"
#'       ) +
#'       pot_theme()
#'    
#'    # Calibration plot
#'    p2 <- ggplot(calibration_data, aes(x = Mean_Predicted, y = Observed_Rate)) +
#'       geom_point(aes(size = Count), alpha = 0.7) +
#'       geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
#'       geom_smooth(method = "lm", se = TRUE, color = "red") +
#'       scale_size_continuous(name = "Sample Size") +
#'       labs(
#'          title = "Calibration Plot",
#'          x = "Mean Predicted Probability",
#'          y = "Observed Exceedance Rate"
#'       ) +
#'       pot_theme()
#'    
#'    return(list(scatter = p1, calibration = p2))
#' }
#' 
#' #' Plot residuals and model diagnostics
#' #' 
#' #' @param stage1_data Data frame with data
#' #' @param threshold Salinity threshold
#' #' @param distribution Distribution to analyze
#' #' @return List of ggplot objects
#' plot_model_diagnostics <- function(stage1_data, threshold = 0.8, distribution = "gpd") {
#'    
#'    pred_col <- paste0("hybrid_prob_", distribution)
#'    
#'    plot_data <- stage1_data %>%
#'       mutate(
#'          Actual_Exceedance = as.numeric(Salinity > threshold),
#'          Predicted_Prob = !!sym(pred_col),
#'          Residual = Actual_Exceedance - Predicted_Prob,
#'          DateTime = as.POSIXct(DateTime)
#'       ) %>%
#'       filter(!is.na(Predicted_Prob))
#'    
#'    # Residuals vs time
#'    p1 <- ggplot(plot_data, aes(x = DateTime, y = Residual)) +
#'       geom_point(alpha = 0.5) +
#'       geom_smooth(se = TRUE, color = "red") +
#'       geom_hline(yintercept = 0, linetype = "dashed") +
#'       labs(
#'          title = "Residuals vs Time",
#'          x = "Date",
#'          y = "Residual (Actual - Predicted)"
#'       ) +
#'       pot_theme()
#'    
#'    # Residuals vs predicted
#'    p2 <- ggplot(plot_data, aes(x = Predicted_Prob, y = Residual)) +
#'       geom_point(alpha = 0.5) +
#'       geom_smooth(se = TRUE, color = "red") +
#'       geom_hline(yintercept = 0, linetype = "dashed") +
#'       labs(
#'          title = "Residuals vs Predicted",
#'          x = "Predicted Probability",
#'          y = "Residual (Actual - Predicted)"
#'       ) +
#'       pot_theme()
#'    
#'    # Residual histogram
#'    p3 <- ggplot(plot_data, aes(x = Residual)) +
#'       geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue", color = "white") +
#'       geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
#'       labs(
#'          title = "Residual Distribution",
#'          x = "Residual (Actual - Predicted)",
#'          y = "Frequency"
#'       ) +
#'       pot_theme()
#'    
#'    return(list(time = p1, predicted = p2, histogram = p3))
#' }
#' 
#' # ============================================================================
#' # COMPOSITE DASHBOARD FUNCTIONS
#' # ============================================================================
#' 
#' #' Create comprehensive dashboard for a single distribution
#' #' 
#' #' @param stage1_data Data frame with stage 1 data
#' #' @param metrics_df Data frame with metrics
#' #' @param distribution Distribution name to analyze
#' #' @param threshold Salinity threshold
#' #' @param date_range Optional date range for time series
#' #' @return List of plots
#' create_distribution_dashboard <- function(stage1_data, 
#'                                           metrics_df,
#'                                           distribution = "gpd",
#'                                           threshold = 0.8,
#'                                           date_range = NULL) {
#'    
#'    # Filter metrics for this distribution
#'    dist_metrics <- metrics_df %>%
#'       filter(grepl(distribution, Distribution, ignore.case = TRUE))
#'    
#'    dashboard <- list(
#'       # Performance metrics
#'       metrics_bar = plot_metric_comparison(
#'          dist_metrics, 
#'          metrics_to_plot = c("accuracy", "auc", "precision", "recall"),
#'          facet_by = "Metric_Type"
#'       ),
#'       
#'       # Time series
#'       timeseries = plot_exceedance_timeseries(
#'          stage1_data, threshold, distribution, date_range
#'       ),
#'       
#'       # Salinity with predictions
#'       salinity_pred = plot_salinity_with_predictions(
#'          stage1_data, threshold, distribution, date_range
#'       ),
#'       
#'       # Calibration
#'       calibration = plot_prediction_scatter(stage1_data, threshold, distribution),
#'       
#'       # Diagnostics
#'       diagnostics = plot_model_diagnostics(stage1_data, threshold, distribution)
#'    )
#'    
#'    return(dashboard)
#' }
#' 
#' #' Create multi-distribution comparison dashboard
#' #' 
#' #' @param stage1_data Data frame with stage 1 data
#' #' @param metrics_df Data frame with metrics
#' #' @param distributions Vector of distributions to compare
#' #' @param threshold Salinity threshold
#' #' @return List of comparison plots
#' create_comparison_dashboard <- function(stage1_data,
#'                                         metrics_df,
#'                                         distributions = c("gpd", "lognormal", "gamma"),
#'                                         threshold = 0.8) {
#'    
#'    dashboard <- list(
#'       # Metric comparisons
#'       metrics_comparison = plot_metric_comparison(
#'          metrics_df,
#'          metrics_to_plot = c("accuracy", "auc"),
#'          facet_by = "Metric_Type"
#'       ),
#'       
#'       metrics_radar = plot_metric_radar(
#'          metrics_df,
#'          metric_type = "classification",
#'          metrics_to_plot = c("accuracy", "auc")
#'       ),
#'       
#'       # Time series comparison
#'       timeseries_comparison = plot_exceedance_timeseries(
#'          stage1_data, threshold, distributions, facet_distributions = TRUE
#'       ),
#'       
#'       # Individual calibration plots for each distribution
#'       calibration_plots = purrr::map(distributions, ~{
#'          plot_prediction_scatter(stage1_data, threshold, .x)
#'       }) %>% purrr::set_names(distributions)
#'    )
#'    
#'    return(dashboard)
#' }
#' 
#' # ============================================================================
#' # UTILITY FUNCTIONS
#' # ============================================================================
#' 
#' #' Save plots to files
#' #' 
#' #' @param plot_list List of plots
#' #' @param output_dir Directory to save plots
#' #' @param prefix Filename prefix
#' #' @param width Plot width in inches
#' #' @param height Plot height in inches
#' save_plot_dashboard <- function(plot_list, 
#'                                 output_dir = "plots",
#'                                 prefix = "stage1",
#'                                 width = 12, 
#'                                 height = 8) {
#'    
#'    if (!dir.exists(output_dir)) {
#'       dir.create(output_dir, recursive = TRUE)
#'    }
#'    
#'    iwalk(plot_list, ~{
#'       if (is.ggplot(.x)) {
#'          filename <- file.path(output_dir, paste0(prefix, "_", .y, ".png"))
#'          ggsave(filename, .x, width = width, height = height, dpi = 300)
#'          cat("Saved:", filename, "\n")
#'       } else if (is.list(.x)) {
#'          # Handle nested lists (like calibration plots)
#'          iwalk(.x, ~{
#'             if (is.ggplot(.x)) {
#'                filename <- file.path(output_dir, paste0(prefix, "_", .y, "_", ..2, ".png"))
#'                ggsave(filename, .x, width = width, height = height, dpi = 300)
#'                cat("Saved:", filename, "\n")
#'             }
#'          })
#'       }
#'    })
#' }
#' 
#' #' Print summary statistics for model performance
#' #' 
#' #' @param metrics_df Data frame with metrics
#' print_performance_summary <- function(metrics_df) {
#'    
#'    summary_stats <- metrics_df %>%
#'       filter(Metric_Name %in% c("accuracy", "auc")) %>%
#'       group_by(Distribution, Metric_Type, Metric_Name) %>%
#'       summarise(
#'          Mean_Value = mean(Value, na.rm = TRUE),
#'          .groups = "drop"
#'       ) %>%
#'       pivot_wider(names_from = c(Metric_Type, Metric_Name), 
#'                   values_from = Mean_Value) %>%
#'       arrange(desc(classification_accuracy))
#'    
#'    cat("\n=== MODEL PERFORMANCE SUMMARY ===\n")
#'    print(summary_stats)
#'    cat("\n")
#' }
#' 
#' # ============================================================================
#' # EXAMPLE USAGE
#' # ============================================================================
#' 
#' # Example of how to use the plotting suite:
#' #
#' # # Create dashboard for GPD distribution
#' # gpd_dashboard <- create_distribution_dashboard(
#' #   stage1_data, metrics_df, "gpd", threshold = 0.8
#' # )
#' #
#' # # Create comparison dashboard
#' # comparison_dashboard <- create_comparison_dashboard(
#' #   stage1_data, metrics_df, 
#' #   distributions = c("gpd", "lognormal", "gamma")
#' # )
#' #
#' # # Save all plots
#' # save_plot_dashboard(gpd_dashboard, "plots/gpd", "gpd_analysis")
#' # save_plot_dashboard(comparison_dashboard, "plots/comparison", "distribution_comparison")
#' #
#' # # Print performance summary
#' # print_performance_summary(metrics_df)