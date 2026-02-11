# Color palette
gam_colors <- list(
   primary = "#f58220",    # orange
   secondary = "#009bba",  # blue
   tertiary = "#fdb515",   # yellow
   dark = "#002030",       # dark blue
   threshold = "#002030"
)

#' Plot relative variable importance by group across lead times
#' @param group_importance_df Data frame with columns: LeadTime, Group, RelativeImportance
#' @param title Plot title (if NULL, uses default)
plot_relative_importance <- function(group_importance_df, title = NULL, x_label) {
   
   if (is.null(title)) {
      title <- "Relative Variable Importance by Group Across Lead Times"
   }
   
   # Define colors for groups
   group_colors <- c(
      gam_colors$primary,      # orange
      gam_colors$secondary,    # blue
      gam_colors$tertiary,     # yellow
      "#8B4789",               # purple
      "#2E8B57",               # sea green
      "#CD5C5C"                # indian red
   )
   
   p <- ggplot(group_importance_df, aes(x = LeadTime, y = RelativeImportance, fill = Group)) +
      geom_area(alpha = 0.7, color = gam_colors$dark, linewidth = 0.3) +
      scale_fill_manual(values = group_colors) +
      labs(
         title = title,
         x = x_label,
         y = "Relative Importance",
         fill = "Variable Group"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.position = "right",
         legend.title = element_text(size = 12, face = 'bold', color = gam_colors$dark),
         legend.text = element_text(size = 11, color = gam_colors$dark),
         legend.background = element_rect(fill = "white", color = gam_colors$dark, linewidth = 0.5),
         legend.key = element_rect(fill = "white", color = NA)
      )
   
   return(p)
}

#' Plot absolute mean importance by group across lead times
#' @param group_importance_df Data frame with columns: LeadTime, Group, MeanImportance
#' @param title Plot title (if NULL, uses default)
plot_absolute_importance <- function(group_importance_df, title = NULL, x_label) {
   
   if (is.null(title)) {
      title <- "Absolute Mean Importance by Group Across Lead Times"
   }
   
   # Define colors for groups (same as above for consistency)
   group_colors <- c(
      gam_colors$primary,      # orange
      gam_colors$secondary,    # blue
      gam_colors$tertiary,     # yellow
      "#8B4789",               # purple
      "#2E8B57",               # sea green
      "#CD5C5C"                # indian red
   )
   
   p <- ggplot(group_importance_df, aes(x = LeadTime, y = MeanImportance, 
                                        color = Group, shape = Group)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3.5) +
      scale_color_manual(values = group_colors) +
      scale_shape_manual(values = c(16, 17, 15, 18, 3, 4)) +  # variety of shapes
      labs(
         title = title,
         x = x_label,
         y = "Mean IncMSE_OOB",
         color = "Variable Group",
         shape = "Variable Group"
      ) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.position = "right",
         legend.title = element_text(size = 12, face = 'bold', color = gam_colors$dark),
         legend.text = element_text(size = 11, color = gam_colors$dark),
         legend.background = element_rect(fill = "white", color = gam_colors$dark, linewidth = 0.5),
         legend.key = element_rect(fill = "white", color = NA)
      )
   
   return(p)
}


#' Plot error metrics across folds
#' @param metrics_df Data frame with columns: Fold, Test_Years, RMSE, MAE
plot_error_metrics <- function(metrics_df) {
   # Reshape data for plotting
   metrics_long <- metrics_df %>%
      select(Fold, Test_Years, RMSE, MAE) %>%
      pivot_longer(cols = c(RMSE, MAE), 
                   names_to = "Metric", 
                   values_to = "Value")
   
   # Create the plot
   p <- ggplot(metrics_long, aes(x = Fold, y = Value, color = Metric, group = Metric)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3.5) +
      scale_color_manual(values = c("RMSE" = gam_colors$primary, 
                                    "MAE" = gam_colors$secondary)) +
      labs(title = "Model Performance Across Expanding Window Folds",
           subtitle = "RMSE and MAE on Test Sets",
           x = "Fold (Test Year)",
           y = "Error Value",
           color = "Metric") +
      scale_x_continuous(breaks = metrics_df$Fold,
                         labels = metrics_df$Test_Years) +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         plot.subtitle = element_text(size = 13, color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         axis.text.x = element_text(angle = 45, hjust = 1),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.position = "bottom",
         legend.title = element_text(size = 12, face = 'bold', color = gam_colors$dark),
         legend.text = element_text(size = 11, color = gam_colors$dark),
         legend.background = element_rect(fill = "white", color = NA),
         legend.key = element_rect(fill = "white", color = NA)
      )
   
   return(p)
}

#' Plot mean variable importance
#' @param importance_df Data frame with variable importance across folds
#' @param top_vars_list List of top variables by category
#' @param top_n_per_group Number of top variables per group
plot_mean_importance <- function(importance_df, top_vars_list, top_n_per_group = 2) {
   
   # Extract variable names from each category in top_vars_list
   top_variables <- c(
      top_vars_list$salinity$Variable,
      top_vars_list$inflow$Variable,
      top_vars_list$discharge$Variable,
      top_vars_list$tide$Variable,
      top_vars_list$wind$Variable,
      top_vars_list$time$Variable
   )
   
   # Calculate mean importance and SD from the full importance_df
   mean_imp <- importance_df %>%
      filter(Variable %in% top_variables) %>%
      group_by(Variable) %>%
      summarise(avg_imp = mean(IncMSE_OOB, na.rm = TRUE),
                sd_imp = sd(IncMSE_OOB, na.rm = TRUE)) %>%
      ungroup()
   
   # Add category labels based on which list each variable came from
   mean_imp <- mean_imp %>%
      mutate(Category = case_when(
         Variable %in% top_vars_list$salinity$Variable ~ 'Salinity',
         Variable %in% top_vars_list$inflow$Variable ~ "Inflow",
         Variable %in% top_vars_list$discharge$Variable ~ "Discharge",
         Variable %in% top_vars_list$tide$Variable ~ "Tide",
         Variable %in% top_vars_list$wind$Variable ~ "Wind",
         Variable %in% top_vars_list$time$Variable ~ "Time",
         TRUE ~ "Other"
      ))
   
   # Create plot
   p <- ggplot(mean_imp, aes(x = reorder(Variable, avg_imp), y = avg_imp, fill = Category)) +
      geom_col(alpha = 0.9) +
      geom_errorbar(aes(ymin = avg_imp - sd_imp,
                        ymax = avg_imp + sd_imp),
                    width = 0.3, colour = gam_colors$dark, linewidth = 0.5) +
      coord_flip() +
      
      # Color scheme using your palette
      scale_fill_manual(values = c(
         'Salinity' = 'forestgreen',
         "Inflow" = gam_colors$secondary,     # blue
         "Discharge" = gam_colors$primary,    # orange
         "Tide" = gam_colors$dark,            # dark blue
         "Wind" = gam_colors$tertiary,        # yellow
         "Time" = "gray50"
      )) +
      
      # Labels
      labs(
         title = "Variable Importance for Salinity Prediction",
         subtitle = "Error bars show ±1 SD across folds",
         x = "Variable",
         y = "Mean Importance (% Increase in MSE)",
         fill = "Category"
      ) +
      
      # Theme
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         plot.subtitle = element_text(size = 13, color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.position = 'none'
      )
   
   return(p)
}

#' Plot importance heatmap
#' @param importance_df Data frame with variable importance across folds
#' @param top_n Number of top variables to show
plot_importance_heatmap <- function(importance_df, top_n = 50) {
   # Get top variables by mean importance
   top_vars <- importance_df %>%
      group_by(Variable) %>%
      summarise(Mean_Imp = mean(IncMSE_OOB, na.rm = TRUE)) %>%
      arrange(desc(Mean_Imp)) %>%
      slice_head(n = top_n) %>%
      pull(Variable)
   
   # Filter and prepare data
   imp_filtered <- importance_df %>%
      filter(Variable %in% top_vars) %>%
      mutate(Variable = factor(Variable, levels = rev(top_vars)))
   
   # Create heatmap
   p <- ggplot(imp_filtered, aes(x = Fold, y = Variable, fill = IncMSE_OOB)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = gam_colors$secondary,      # blue
                           mid = gam_colors$tertiary,       # yellow
                           high = gam_colors$primary,       # orange
                           midpoint = median(imp_filtered$IncMSE_OOB, na.rm = TRUE)) +
      labs(title = "Variable Importance Evolution Across Folds",
           subtitle = paste("Top", top_n, "variables by mean IncMSE"),
           x = "Fold",
           y = "Variable",
           fill = "IncMSE_OOB") +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         plot.subtitle = element_text(size = 13, color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         axis.text.x = element_text(angle = 0, hjust = 0.5),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.title = element_text(size = 11, face = 'bold', color = gam_colors$dark),
         legend.text = element_text(size = 10, color = gam_colors$dark)
      )
   
   return(p)
}

#' Plot variable group heatmap
#' @param importance_df Data frame with variable importance across folds
#' @param pattern Pattern to match variables (e.g., "Discharge", "Tid")
#' @param pattern_name Display name for the pattern
plot_variable_group_heatmap <- function(importance_df, 
                                        pattern, 
                                        pattern_name = "Variables") {
   # Filter variables matching pattern
   if (pattern == 'Tid') {
      matched_vars <- importance_df %>%
         filter(
            grepl(pattern, Variable, ignore.case = TRUE)
         ) %>%
         pull(Variable) %>%
         unique()
   } else {
      matched_vars <- importance_df %>%
         filter(
            grepl(pattern, Variable, ignore.case = TRUE),
            !grepl("Hour|Tide|Tidal", Variable, ignore.case = TRUE)
         ) %>%
         pull(Variable) %>%
         unique()
      
      if(length(matched_vars) == 0) {
         stop(paste("No variables found matching pattern:", pattern))
      }
   }
   
   # Prepare data
   plot_data <- importance_df %>%
      filter(Variable %in% matched_vars) %>%
      mutate(Variable = factor(Variable, levels = rev(sort(matched_vars))))
   
   # Create heatmap
   p <- ggplot(plot_data, aes(x = Fold, y = Variable, fill = IncMSE_OOB)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = gam_colors$secondary,      # blue
                           mid = gam_colors$tertiary,       # yellow
                           high = gam_colors$primary,       # orange
                           midpoint = median(plot_data$IncMSE_OOB, na.rm = TRUE)) +
      labs(title = paste(pattern_name, "Importance Evolution"),
           subtitle = paste("Pattern:", pattern),
           x = "Fold",
           y = "Variable",
           fill = "IncMSE") +
      theme_bw() +
      theme(
         plot.title = element_text(size = 16, face = 'bold', color = gam_colors$dark),
         plot.subtitle = element_text(size = 13, color = gam_colors$dark),
         axis.title = element_text(size = 14, face = 'bold', color = gam_colors$dark),
         axis.text = element_text(size = 12, color = gam_colors$dark),
         axis.text.y = element_text(size = 10),
         panel.border = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
         legend.title = element_text(size = 11, face = 'bold', color = gam_colors$dark),
         legend.text = element_text(size = 10, color = gam_colors$dark)
      )
   
   return(p)
}