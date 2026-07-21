# =============================================================================
# Script Name:    03_EvaluateStackedRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Evaluates RF screening results from the unified stacked
#                 dataset. Produces a suite of visualizations to inform scale-
#                 dependent predictor selection for GAM construction.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggridges)
library(patchwork)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')

# =============================================================================
# PARAMETERS
# =============================================================================

H_MAX <- 20 # number of horizon days

base_dir <- "Outputs/Plots/StackedRF"
if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

within_group_dir <- file.path(base_dir, 'WithinGroup')
if (!dir.exists(within_group_dir)) dir.create(within_group_dir, recursive = TRUE)

group_colors <- c(
   FlushingDischarge  = "#2E8B57",
   SustainedDischarge = "#4A90D9",
   LagSalinity        = "#E07B3F",
   Tide               = "#D4AC0D",
   Wind               = "#8B4789"
)

theme_rf <- function() {
   theme_bw() +
      theme(
         plot.title        = element_text(size = 14, face = 'bold', color = 'grey20'),
         plot.subtitle     = element_text(size = 11, color = 'grey20'),
         axis.title        = element_text(size = 12, face = 'bold', color = 'grey20'),
         axis.text         = element_text(size = 10, color = 'grey20'),
         panel.border      = element_rect(colour = 'grey20', fill = NA, linewidth = 1),
         legend.title      = element_text(size = 11, face = 'bold', color = 'grey20'),
         legend.text       = element_text(size = 10, color = 'grey20'),
         legend.background = element_rect(fill = 'white', color = 'grey20', linewidth = 0.5),
         legend.key        = element_rect(fill = 'white', color = NA),
         strip.text        = element_text(size = 11, face = 'bold', color = 'grey20'),
         strip.background  = element_rect(fill = 'grey92', color = 'grey20')
      )
}

# =============================================================================
# LOAD AND PREPARE
# =============================================================================

rf_stacked         <- read_qs_files('Outputs/Models/StackedRF/RFStacked.qs')
importance_summary <- read_qs_files('Outputs/Models/StackedRF/RFImportanceSummary.qs')
stability_summary  <- read_qs_files('Outputs/Models/StackedRF/RFStabilitySummary.qs')

# Seed-averaged h-stratified importance: each seed contributes one
# fold-averaged MeanImportance per Variable x h; values here are the
# mean and SD of those seed-level estimates across N_STABLE_SEEDS seeds.
# This is the appropriate object for importance surface plots.
h_importance <- read_qs_files('Outputs/Models/StackedRF/RFImportanceByHorizonSeeded.qs')

recode_groups <- function(df) {
   df %>% mutate(Group = recode(Group,
                                lag_salinity        = 'LagSalinity',
                                sustained_discharge = 'SustainedDischarge',
                                flushing_discharge  = 'FlushingDischarge',
                                tide                = 'Tide',
                                wind                = 'Wind',
                                horizon             = 'Horizon'
   ))
}

importance_summary <- recode_groups(importance_summary)
h_importance       <- recode_groups(h_importance)
stability_summary  <- recode_groups(stability_summary)

# Floor negatives, filter to physical groups
h_imp <- h_importance %>%
   filter(Group %in% names(group_colors)) %>%
   mutate(MeanImportance = pmax(MeanImportance, 0))

h_imp <- h_imp %>%
   mutate(WindowWidth = as.integer(str_extract(Variable, "[0-9]+$")))

# =============================================================================
# GROUP-LEVEL NORMALIZED IMPORTANCE
# =============================================================================

group_by_h <- h_imp %>%
   group_by(h, Group) %>%
   summarise(MeanImportance = mean(MeanImportance, na.rm = TRUE), .groups = 'drop') %>%
   group_by(h) %>%
   mutate(
      TotalImportance    = sum(MeanImportance),
      RelativeImportance = if_else(TotalImportance > 0,
                                   MeanImportance / TotalImportance, 0)
   ) %>%
   ungroup() %>%
   rename(LeadTime = h)

# =============================================================================
# PLOT 1: RELATIVE GROUP IMPORTANCE — STREAM
# =============================================================================

p_stream <- ggplot(group_by_h,
                   aes(x = LeadTime, y = RelativeImportance, fill = Group)) +
   geom_area(alpha = 0.75, color = 'grey20', linewidth = 0.3) +
   scale_fill_manual(values = group_colors, breaks = names(group_colors)) +
   scale_x_continuous(breaks = seq(0, H_MAX, 2)) +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(x = "Lead Time (days)", y = "Relative Importance", fill = "Group",
        title = "Group Importance Across Lead Times") +
   theme_rf() +
   theme(legend.position = 'bottom')

ggsave(file.path(base_dir, 'RelativeGroupImportance.png'),
       plot = p_stream, width = 12, height = 8, dpi = 600)
ggsave(file.path(base_dir, 'RelativeGroupImportance.svg'),
       plot = p_stream, width = 12, height = 8, dpi = 600)

# =============================================================================
# PLOT 2: ABSOLUTE GROUP IMPORTANCE — LINE
# =============================================================================

p_absolute <- ggplot(group_by_h,
                     aes(x = LeadTime, y = MeanImportance,
                         color = Group, shape = Group)) +
   geom_line(linewidth = 1.2) +
   geom_point(size = 3.5) +
   scale_color_manual(values = group_colors, breaks = names(group_colors)) +
   # Set guide = "none" to hide shapes from the legend entirely
   scale_shape_manual(values = c(16, 17, 15, 18, 3), guide = "none") + 
   scale_x_continuous(breaks = seq(0, H_MAX, 2)) +
   labs(x = "Lead Time (days)", y = "Mean Permutation Importance",
        color = "Group", # Removed shape assignment here
        title = "Absolute Group Importance Across Lead Times") +
   theme_rf() +
   theme(legend.position = 'bottom')

ggsave(file.path(base_dir, 'AbsoluteGroupImportance.png'),
       plot = p_absolute, width = 12, height = 8, dpi = 600)

# =============================================================================
# PLOT 3: WITHIN-GROUP HEATMAPS
# =============================================================================

plot_group_heatmap <- function(df, grp, col) {
   
   df_grp <- df %>%
      filter(Group == grp)
   
   var_order <- df_grp %>%
      group_by(Variable) %>%
      summarise(total_imp = sum(MeanImportance, na.rm = TRUE)) %>%
      arrange(total_imp) %>%
      pull(Variable)
   
   df_grp <- df_grp %>%
      mutate(Variable = factor(Variable, levels = var_order))
   
   ggplot(df_grp, aes(x = h, y = Variable, fill = MeanImportance)) +
      geom_tile(color = 'grey85', linewidth = 0.2) +
      scale_fill_gradient(low = 'white', high = col, name = 'Importance') +
      scale_x_continuous(breaks = seq(0, H_MAX, 2), expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(x = 'Lead Time (days)', y = NULL, title = grp) +
      theme_rf() +
      theme(panel.grid  = element_blank(),
            axis.text.y = element_text(size = 9))
}

for (grp in names(group_colors)) {
   p_heat <- plot_group_heatmap(h_imp, grp, group_colors[[grp]])
   ggsave(file.path(within_group_dir, paste0('Heatmap_', grp, '.png')),
          plot = p_heat, width = 9, height = 6, dpi = 600)
   ggsave(file.path(within_group_dir, paste0('Heatmap_', grp, '.svg')),
          plot = p_heat, width = 9, height = 6, dpi = 600)
}

# =============================================================================
# COMBINED TOP-N HEATMAPS (PATCHWORK REFACTOR)
# =============================================================================

# TOP_N_HEATMAP <- 30
# 
# top_vars <- h_imp %>%
#    group_by(Variable, Group) %>%
#    summarise(MeanImp = mean(MeanImportance), .groups = 'drop') %>%
#    slice_max(MeanImp, n = TOP_N_HEATMAP) %>%
#    arrange(Group, MeanImp) %>%
#    mutate(Variable = fct_inorder(Variable))
# 
# h_imp_top <- h_imp %>%
#    filter(Variable %in% top_vars$Variable) %>%
#    left_join(top_vars %>% select(Variable, Group, MeanImp),
#              by = c('Variable', 'Group')) %>%
#    mutate(Variable = factor(Variable, levels = levels(top_vars$Variable)))

TOP_N_PER_GROUP <- 6

top_vars <- h_imp %>%
   group_by(Variable, Group) %>%
   summarise(MeanImp = mean(MeanImportance), .groups = 'drop') %>%
   # Slice within each group instead of globally
   group_by(Group) %>%
   slice_max(MeanImp, n = TOP_N_PER_GROUP, with_ties = FALSE) %>%
   ungroup() %>%
   arrange(Group, MeanImp) %>%
   mutate(Variable = factor(Variable, levels = unique(Variable)))

h_imp_top <- h_imp %>%
   filter(Variable %in% top_vars$Variable) %>%
   left_join(top_vars %>% select(Variable, Group, MeanImp),
             by = c('Variable', 'Group')) %>%
   mutate(Variable = factor(Variable, levels = levels(top_vars$Variable)))

# Identify unique groups present in the top-N data
present_groups <- names(group_colors)[names(group_colors) %in% unique(h_imp_top$Group)]

# Dynamic layout engine: calculate rows per group to maintain proportional sizes
group_counts <- h_imp_top %>%
   group_by(Group) %>%
   summarise(n_vars = n_distinct(Variable), .groups = 'drop') %>%
   deframe()

# Set a minimum relative height floor (3) so isolated panels like
# LagSalinity have plenty of vertical room for vertical text labels
layout_heights <- sapply(present_groups, function(g) max(group_counts[[g]], 3))

# Calculate global limits across the entire subset for the monochromatic scale
global_limits <- c(0, max(h_imp_top$MeanImportance, na.rm = TRUE))

# Modular panel builder function
build_heatmap_panels <- function(df, groups, mono_color = NULL, shared_limits = NULL) {
   plot_list <- list()
   
   for (i in seq_along(groups)) {
      grp     <- groups[i]
      is_last <- (i == length(groups))
      df_grp  <- df %>% filter(Group == grp)
      
      # Assign palette mapping (group-specific vs fallback monochromatic)
      high_col <- if (is.null(mono_color)) group_colors[[grp]] else mono_color
      
      p <- ggplot(df_grp, aes(x = h, y = Variable, fill = MeanImportance)) +
         geom_tile(color = 'grey85', linewidth = 0.2) +
         facet_grid(Group ~ ., scales = 'free_y') +
         scale_fill_gradient(
            low    = 'white',
            high   = high_col,
            name   = 'Importance',
            limits = shared_limits
         ) +
         scale_x_continuous(breaks = seq(0, H_MAX, 2), expand = c(0, 0)) +
         scale_y_discrete(expand = c(0, 0)) +
         theme_rf() +
         theme(
            panel.grid   = element_blank(),
            axis.text.y  = element_text(size = 8),
            strip.text.y = element_text(face = 'bold')
         )
      
      if (!is_last) {
         p <- p + theme(
            axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank()
         )
      } else {
         p <- p + labs(x = 'Lead Time (days)')
      }
      
      plot_list[[grp]] <- p
   }
   return(plot_list)
}

# --- VERSION 1: Multi-Color (Group-Specific Colors & Scales) ---
plots_multi  <- build_heatmap_panels(h_imp_top, present_groups, mono_color = NULL, shared_limits = NULL)
p_heat_multi <- wrap_plots(plots_multi, ncol = 1, heights = layout_heights) +
   plot_layout(guides = 'keep') +
   plot_annotation(
      title = "Top Predictors Per Group — Importance Surface",
      theme = theme(plot.title = element_text(size = 14, face = 'bold', color = 'grey20'))
   )

ggsave(file.path(base_dir, 'Heatmap_TopPredictors_MultiColor.png'),
       plot = p_heat_multi, width = 11, height = 14, dpi = 600)
ggsave(file.path(base_dir, 'Heatmap_TopPredictors_MultiColor.svg'),
       plot = p_heat_multi, width = 11, height = 14, dpi = 600)

# --- VERSION 2: Monochromatic (Orange Theme, Shared Global Scale) ---
plots_orange  <- build_heatmap_panels(h_imp_top, present_groups, mono_color = "#E07B3F", shared_limits = global_limits)
p_heat_orange <- wrap_plots(plots_orange, ncol = 1, heights = layout_heights) +
   plot_layout(guides = 'collect') +
   plot_annotation(
      title = "Top Predictors Per Group — Importance Surface",
      theme = theme(plot.title = element_text(size = 14, face = 'bold', color = 'grey20'))
   )

ggsave(file.path(base_dir, 'Heatmap_TopPredictors_Orange.png'),
       plot = p_heat_orange, width = 11, height = 14, dpi = 600)
ggsave(file.path(base_dir, 'Heatmap_TopPredictors_Orange.svg'),
       plot = p_heat_orange, width = 11, height = 14, dpi = 600)

# # Identify unique groups present in the top-N data
# present_groups <- names(group_colors)[names(group_colors) %in% unique(h_imp_top$Group)]
# 
# # Dynamic layout engine: calculate rows per group to maintain proportional sizes
# group_counts <- h_imp_top %>%
#    group_by(Group) %>%
#    summarise(n_vars = n_distinct(Variable), .groups = 'drop') %>%
#    deframe()
# 
# # Set a minimum relative height floor (3) so isolated panels like
# # LagSalinity have plenty of vertical room for vertical text labels
# layout_heights <- sapply(present_groups, function(g) max(group_counts[[g]], 3))
# 
# # Calculate global limits across the entire subset for the monochromatic scale
# global_limits <- c(0, max(h_imp_top$MeanImportance, na.rm = TRUE))
# 
# # Modular panel builder function
# build_heatmap_panels <- function(df, groups, mono_color = NULL, shared_limits = NULL) {
#    plot_list <- list()
#    
#    for (i in seq_along(groups)) {
#       grp     <- groups[i]
#       is_last <- (i == length(groups))
#       df_grp  <- df %>% filter(Group == grp)
#       
#       # Assign palette mapping (group-specific vs fallback monochromatic)
#       high_col <- if (is.null(mono_color)) group_colors[[grp]] else mono_color
#       
#       p <- ggplot(df_grp, aes(x = h, y = Variable, fill = MeanImportance)) +
#          geom_tile(color = 'grey85', linewidth = 0.2) +
#          facet_grid(Group ~ ., scales = 'free_y') +
#          scale_fill_gradient(
#             low    = 'white',
#             high   = high_col,
#             name   = 'Importance',
#             limits = shared_limits
#          ) +
#          scale_x_continuous(breaks = seq(0, H_MAX, 2), expand = c(0, 0)) +
#          scale_y_discrete(expand = c(0, 0)) +
#          theme_rf() +
#          theme(
#             panel.grid   = element_blank(),
#             axis.text.y  = element_text(size = 8),
#             strip.text.y = element_text(face = 'bold')
#          )
#       
#       if (!is_last) {
#          p <- p + theme(
#             axis.title.x = element_blank(),
#             axis.text.x  = element_blank(),
#             axis.ticks.x = element_blank()
#          )
#       } else {
#          p <- p + labs(x = 'Lead Time (days)')
#       }
#       
#       plot_list[[grp]] <- p
#    }
#    return(plot_list)
# }
# 
# # --- VERSION 1: Multi-Color (Group-Specific Colors & Scales) ---
# plots_multi  <- build_heatmap_panels(h_imp_top, present_groups, mono_color = NULL, shared_limits = NULL)
# p_heat_multi <- wrap_plots(plots_multi, ncol = 1, heights = layout_heights) +
#    plot_layout(guides = 'keep') +
#    plot_annotation(
#       title = paste0("Top ", TOP_N_HEATMAP, " Predictors — Importance Surface"),
#       theme = theme(plot.title = element_text(size = 14, face = 'bold', color = 'grey20'))
#    )
# 
# ggsave(file.path(base_dir, 'Heatmap_TopPredictors_MultiColor.png'),
#        plot = p_heat_multi, width = 11, height = 14, dpi = 600)
# ggsave(file.path(base_dir, 'Heatmap_TopPredictors_MultiColor.svg'),
#        plot = p_heat_multi, width = 11, height = 14, dpi = 600)
# 
# # --- VERSION 2: Monochromatic (Orange Theme, Shared Global Scale) ---
# plots_orange  <- build_heatmap_panels(h_imp_top, present_groups, mono_color = "#E07B3F", shared_limits = global_limits)
# p_heat_orange <- wrap_plots(plots_orange, ncol = 1, heights = layout_heights) +
#    plot_layout(guides = 'collect') +
#    plot_annotation(
#       title = paste0("Top ", TOP_N_HEATMAP, " Predictors — Importance Surface"),
#       theme = theme(plot.title = element_text(size = 14, face = 'bold', color = 'grey20'))
#    )
# 
# ggsave(file.path(base_dir, 'Heatmap_TopPredictors_Orange.png'),
#        plot = p_heat_orange, width = 11, height = 14, dpi = 600)
# ggsave(file.path(base_dir, 'Heatmap_TopPredictors_Orange.svg'),
#        plot = p_heat_orange, width = 11, height = 14, dpi = 600)

# # =============================================================================
# # PLOT 4: RIDGELINES
# # =============================================================================
# 
# for (grp in names(group_colors)) {
#    
#    df_grp <- h_imp %>%
#       filter(Group == grp)
#    
#    var_order <- df_grp %>%
#       group_by(Variable) %>%
#       summarise(total_imp = sum(MeanImportance, na.rm = TRUE)) %>%
#       arrange(total_imp) %>%
#       pull(Variable)
#    
#    df_grp <- df_grp %>%
#       mutate(Variable = factor(Variable, levels = var_order))
#    
#    p_ridge <- ggplot(df_grp,
#                      aes(x = h, y = Variable,
#                          height = MeanImportance * 1000,
#                          fill = Variable)) +
#       geom_ridgeline(scale = 3, alpha = 0.75, color = 'grey20',
#                      linewidth = 0.3, min_height = 0) +
#       scale_fill_manual(
#          values = colorRampPalette(c('white', group_colors[[grp]]))(
#             length(unique(df_grp$Variable))
#          ),
#          guide = 'none'
#       ) +
#       scale_x_continuous(breaks = seq(0, H_MAX, 2)) +
#       labs(x = 'Lead Time (days)', y = NULL,
#            title = paste0(grp, " — Importance Profile by Lead Time")) +
#       theme_rf()
#    
#    ggsave(file.path(within_group_dir, paste0('Ridge_', grp, '.png')),
#           plot = p_ridge, width = 10, height = 7, dpi = 600)
#    ggsave(file.path(within_group_dir, paste0('Ridge_', grp, '.svg')),
#           plot = p_ridge, width = 10, height = 7, dpi = 600)
# }

# # =============================================================================
# # PLOT 5: PEAK IMPORTANCE HORIZON vs WINDOW WIDTH
# # =============================================================================
# 
# peak_horizon <- h_imp %>%
#    filter(!is.na(WindowWidth)) %>%
#    group_by(Variable, Group, WindowWidth) %>%
#    summarise(
#       PeakH          = h[which.max(MeanImportance)],
#       MeanImportance = mean(MeanImportance),
#       .groups = 'drop'
#    )
# 
# p_peak <- ggplot(peak_horizon,
#                  aes(x = WindowWidth, y = PeakH,
#                      color = Group, size = MeanImportance)) +
#    geom_point(alpha = 0.8) +
#    geom_smooth(aes(group = Group, color = Group),
#                method = 'lm', se = FALSE, linewidth = 0.8, linetype = 'dashed') +
#    scale_color_manual(values = group_colors, breaks = names(group_colors)) +
#    scale_size_continuous(range = c(1.5, 6), name = 'Mean Importance') +
#    scale_x_continuous(breaks = c(1, 2, 3, 4, 6, 7, 10, 12, 14, 21, 30)) +
#    scale_y_continuous(breaks = seq(0, H_MAX, 2)) +
#    labs(x = 'Predictor Window Width (days)', y = 'Peak Importance Horizon (days)',
#         color = 'Group',
#         title = 'Predictor Timescale vs Peak Forecast Horizon') +
#    theme_rf() +
#    theme(legend.position = 'right')
# 
# ggsave(file.path(base_dir, 'PeakHorizon_vs_WindowWidth.png'),
#        plot = p_peak, width = 11, height = 7, dpi = 600)
# ggsave(file.path(base_dir, 'PeakHorizon_vs_WindowWidth.svg'),
#        plot = p_peak, width = 11, height = 7, dpi = 600)

# =============================================================================
# PLOT 6: STABILITY — MEAN RANK ± SD RANK
# One dot per predictor, x = mean rank within group across seeds,
# error bars = ± 1 SD. Faceted by group. Lower rank = more important.
# Wide error bars flag collinearity-driven instability.
# =============================================================================

stab_plot_df <- stability_summary %>%
   filter(Group %in% names(group_colors)) %>%
   filter(Group != "LagSalinity") %>% # Dropping LagSalinity from this specific plot
   group_by(Group) %>%
   mutate(Variable = fct_reorder(Variable, MeanRank, .desc = TRUE)) %>%
   ungroup()

p_stability <- ggplot(stab_plot_df,
                      aes(x = MeanRank, y = Variable,
                          color = Group, xmin = MeanRank - SDRank,
                          xmax = MeanRank + SDRank)) +
   geom_errorbarh(height = 0.35, linewidth = 0.7, alpha = 0.6) +
   geom_point(aes(size = MeanImportance), alpha = 0.9) +
   facet_wrap(~ Group, scales = 'free_y', ncol = 2) +
   scale_color_manual(values = group_colors, guide = 'none') +
   scale_size_continuous(range = c(1.5, 5), name = 'Mean Importance') +
   scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
   labs(x = 'Mean Rank Within Group (lower = more important)',
        y = NULL,
        title = 'Predictor Stability Across Seeds',
        subtitle = 'Error bars show ± 1 SD of rank across 10 seeds') +
   theme_rf() +
   theme(legend.position = 'bottom')

ggsave(file.path(base_dir, 'StabilityRank.png'),
       plot = p_stability, width = 13, height = 10, dpi = 600)
ggsave(file.path(base_dir, 'StabilityRank.svg'),
       plot = p_stability, width = 13, height = 10, dpi = 600)

# =============================================================================
# PLOT 7: CV ERROR METRICS — LINE + POINT
# =============================================================================

metrics_long <- rf_stacked$metrics %>%
   select(Fold, Test_Years, RMSE, MAE) %>%
   pivot_longer(cols = c(RMSE, MAE),
                names_to  = 'Metric',
                values_to = 'Value')

p_error <- ggplot(metrics_long,
                  aes(x = Fold, y = Value, color = Metric, group = Metric)) +
   geom_line(linewidth = 1.2) +
   geom_point(size = 3.5) +
   scale_color_manual(values = c(RMSE = "#E07B3F", MAE = "#4A90D9")) +
   scale_x_continuous(breaks = rf_stacked$metrics$Fold,
                      labels = rf_stacked$metrics$Test_Years) +
   labs(x = "Fold (Test Year)", y = "Error",
        color = "Metric",
        title = "Model Performance Across Expanding Window Folds",
        subtitle = "RMSE and MAE on test sets") +
   theme_rf() +
   theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = 'bottom'
   )

ggsave(file.path(base_dir, 'CVErrorMetrics.png'),
       plot = p_error, width = 10, height = 6, dpi = 600)

# =============================================================================
# WRITE OUTPUTS
# =============================================================================

write_qs_files(
   list(group_by_h),
   'Outputs/Models/StackedRF',
   c('GroupImportanceByH')
)

cat("\nScript 03 complete. Plots saved to:", base_dir, "\n")

rm(list = ls())