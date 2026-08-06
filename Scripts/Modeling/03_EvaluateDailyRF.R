# =============================================================================
# Script Name:    03_EvaluateStackedRF.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Evaluates RF screening results from the unified stacked
#                 dataset. Produces per-group heatmaps and a stability-rank
#                 plot from the h-stratified, OOB-based, seed-averaged
#                 permutation importance stream.
# =============================================================================

library(here)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')

# =============================================================================
# PARAMETERS
# =============================================================================

H_MAX <- 20

base_dir <- "Outputs/Plots/StackedRF"
if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

within_group_dir <- file.path(base_dir, 'GroupHeatmaps')
if (!dir.exists(within_group_dir)) dir.create(within_group_dir, recursive = TRUE)

group_colors <- c(
   FlushingDischarge  = "#2E8B57",
   SustainedDischarge = "#4A90D9",
   LagSalinity        = "#E07B3F",
   Tide               = "#D4AC0D",
   Wind               = "#8B4789"
)

GROUP_ORDER <- c("LagSalinity", "SustainedDischarge", "Wind",
                 "FlushingDischarge", "Tide")

group_display_names <- c(
   LagSalinity        = "Lag Salinity",
   SustainedDischarge = "Sustained Discharge",
   Wind               = "Wind",
   FlushingDischarge  = "Flushing Discharge",
   Tide               = "Tide"
)

theme_rf <- function() {
   theme_bw() +
      theme(
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

rf_stacked        <- read_qs_files('Outputs/Models/StackedRF/RFStacked.qs2')
stability_summary <- read_qs_files('Outputs/Models/StackedRF/RFStabilitySummary.qs2')
h_importance      <- read_qs_files('Outputs/Models/StackedRF/RFImportanceByHorizonSeeded.qs2')

# Floor negatives, filter to physical groups
h_imp <- h_importance %>%
   filter(Group %in% names(group_colors)) %>%
   mutate(MeanImportance = pmax(MeanImportance, 0))

h_imp <- h_imp %>%
   mutate(WindowWidth = as.integer(str_extract(Variable, "[0-9]+$")))

# =============================================================================
# PLOT: WITHIN-GROUP HEATMAPS (all windows per group)
# =============================================================================

plot_group_heatmap <- function(df, grp, col) {
   
   df_grp <- df %>% filter(Group == grp)
   
   var_order <- df_grp %>%
      group_by(Variable) %>%
      summarise(total_imp = sum(MeanImportance, na.rm = TRUE)) %>%
      arrange(total_imp) %>%
      pull(Variable)
   
   df_grp <- df_grp %>% mutate(Variable = factor(Variable, levels = var_order))
   
   ggplot(df_grp, aes(x = h, y = Variable, fill = MeanImportance)) +
      geom_tile(color = 'grey85', linewidth = 0.2) +
      scale_fill_gradient(low = 'white', high = col,
                          name = "Importance (\u0394 RMSE, ppt)") +
      scale_x_continuous(name = 'Forecast Horizon (days)',
                         breaks = seq(0, H_MAX, 2), expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(y = NULL) +
      theme_rf() +
      theme(
         panel.grid      = element_blank(),
         axis.text.y     = element_text(size = 9),
         legend.position = "top",
         legend.justification = "right",
         legend.direction = "horizontal",
         legend.key.width  = unit(1.3, "cm"),
         legend.key.height = unit(0.35, "cm")
      )
}

for (grp in names(group_colors)) {
   p_heat <- plot_group_heatmap(h_imp, grp, group_colors[[grp]])
   ggsave(file.path(within_group_dir, paste0('Heatmap_', grp, '.png')),
          plot = p_heat, width = 9, height = 6, dpi = 600)
   ggsave(file.path(within_group_dir, paste0('Heatmap_', grp, '.svg')),
          plot = p_heat, width = 9, height = 6, dpi = 600)
}

# =============================================================================
# PLOT: TOP-N-PER-GROUP HEATMAP (patchwork panels, lettered, multicolor)
# =============================================================================

TOP_N_PER_GROUP <- 6

top_vars <- h_imp %>%
   group_by(Variable, Group) %>%
   summarise(MeanImp = mean(MeanImportance), .groups = 'drop') %>%
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

present_groups <- GROUP_ORDER[GROUP_ORDER %in% unique(h_imp_top$Group)]
letters_used   <- LETTERS[seq_along(present_groups)]

group_counts <- h_imp_top %>%
   group_by(Group) %>%
   summarise(n_vars = n_distinct(Variable), .groups = 'drop') %>%
   deframe()

layout_heights <- sapply(present_groups, function(g) max(group_counts[[g]], 3))

build_heatmap_panels <- function(df, groups, letters) {
   plot_list <- list()
   
   for (i in seq_along(groups)) {
      grp     <- groups[i]
      is_last <- (i == length(groups))
      df_grp  <- df %>% filter(Group == grp)
      
      # Determine a common power-of-10 multiplier for this panel's scale
      max_val   <- max(df_grp$MeanImportance, na.rm = TRUE)
      exponent  <- floor(log10(max_val))
      divisor   <- 10^exponent
      
      p <- ggplot(df_grp, aes(x = h, y = Variable, fill = MeanImportance)) +
         geom_tile(color = 'grey85', linewidth = 0.2) +
         scale_fill_gradient(
            low = 'white', high = group_colors[[grp]],
            name = bquote(atop("Importance (" * Delta * " RMSE, ppt)",
                               "\u00d7 " * 10^.(exponent))),
            labels = function(v) sprintf("%.1f", v / divisor)
         ) +
         scale_x_continuous(breaks = seq(0, H_MAX, 2), expand = c(0, 0)) +
         scale_y_discrete(expand = c(0, 0)) +
         labs(title = paste0(letters[i], ") ", group_display_names[[grp]])) +
         theme_rf() +
         theme(
            plot.title      = element_text(face = "bold", hjust = 0, size = 14,
                                           margin = margin(b = 1)),
            panel.grid      = element_blank(),
            axis.text.y     = element_text(size = 8),
            legend.position = "top",
            legend.justification = "right",
            legend.direction = "horizontal",
            legend.key.width  = unit(1.3, "cm"),
            legend.key.height = unit(0.35, "cm")
         )
      
      if (!is_last) {
         p <- p + theme(axis.title.x = element_blank(),
                        axis.text.x  = element_blank())
         # axis.ticks.x intentionally left at default (visible)
      } else {
         p <- p + labs(x = 'Forecast Horizon (days)')
      }
      
      plot_list[[grp]] <- p
   }
   return(plot_list)
}

plots_multi  <- build_heatmap_panels(h_imp_top, present_groups, letters_used)
p_heat_multi <- wrap_plots(plots_multi, ncol = 1, heights = layout_heights) +
   plot_layout(guides = 'keep')

ggsave(file.path(base_dir, 'Heatmap_TopPredictors_MultiColor.png'),
       plot = p_heat_multi, width = 11, height = 14, dpi = 600)
ggsave(file.path(base_dir, 'Heatmap_TopPredictors_MultiColor.svg'),
       plot = p_heat_multi, width = 11, height = 14, dpi = 600)

# =============================================================================
# PLOT: STABILITY — MEAN RANK ± SD RANK
# =============================================================================

stab_plot_df <- stability_summary %>%
   filter(Group %in% names(group_colors)) %>%
   filter(Group != "LagSalinity") %>%
   group_by(Group) %>%
   mutate(Variable = fct_reorder(Variable, MeanRank, .desc = TRUE)) %>%
   ungroup()

stab_groups  <- GROUP_ORDER[GROUP_ORDER %in% unique(stab_plot_df$Group)]
stab_letters <- LETTERS[seq_along(stab_groups)]

build_stability_panels <- function(df, groups, letters, hide_x_axis = FALSE, axis_right = FALSE) {
   plot_list <- list()
   
   for (i in seq_along(groups)) {
      grp    <- groups[i]
      df_grp <- df %>% filter(Group == grp)
      
      max_val  <- max(df_grp$MeanImportance, na.rm = TRUE)
      exponent <- floor(log10(max_val))
      divisor  <- 10^exponent
      
      p <- ggplot(df_grp,
                  aes(x = MeanRank, y = Variable,
                      xmin = MeanRank - SDRank, xmax = MeanRank + SDRank)) +
         geom_errorbarh(height = 0.35, linewidth = 0.7, alpha = 0.6,
                        color = group_colors[[grp]]) +
         geom_point(aes(size = MeanImportance), color = group_colors[[grp]], alpha = 0.9) +
         scale_size_continuous(
            range  = c(1, 3.5),
            breaks = scales::pretty_breaks(n = 3)(df_grp$MeanImportance),
            name   = bquote("Mean Imp. (\u0394 RMSE, ppt) \u00d7 " * 10^.(exponent)),
            labels = function(v) sprintf("%.1f", v / divisor),
            guide = guide_legend(
               direction = "horizontal",
               title.position = "top",
               title.hjust = 0.5,
               nrow = 1
            )
         ) + 
         scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
         scale_y_discrete(position = if (axis_right) "right" else "left") +
         labs(title = paste0(letters[i], ") ", group_display_names[[grp]]),
              x = "Mean Rank Within Group",
              y = NULL) +
         theme_rf() +
         theme(plot.title = element_text(face = "bold", hjust = 0, size = 14,
                                         margin = margin(b = 1)),
               legend.title = element_text(size = 9),
               legend.text  = element_text(size = 9),
               legend.key.size = unit(0.80, "cm"),
               legend.spacing.x = unit(0.05, "cm"),
               legend.position = c(0.98, 0.98),
               legend.justification = c("right", "top"),
               legend.background = element_rect(fill = alpha('white', 0.3), color = NA),
               legend.margin = margin(2, 2, 2, 2),
               legend.box.background = element_rect(color = 'black'),
               legend.box = 'vertical')
      
      if (hide_x_axis) {
         p <- p + theme(axis.title.x = element_blank(),
                        axis.text.x  = element_blank())
         # ticks left visible intentionally
      }
      
      plot_list[[grp]] <- p
   }
   return(plot_list)
}

all_plot_list <- list(
   build_stability_panels(stab_plot_df, stab_groups[1], stab_letters[1], hide_x_axis = TRUE,  axis_right = FALSE)[[1]],
   build_stability_panels(stab_plot_df, stab_groups[2], stab_letters[2], hide_x_axis = TRUE,  axis_right = TRUE )[[1]],
   build_stability_panels(stab_plot_df, stab_groups[3], stab_letters[3], hide_x_axis = FALSE, axis_right = FALSE)[[1]],
   build_stability_panels(stab_plot_df, stab_groups[4], stab_letters[4], hide_x_axis = FALSE, axis_right = TRUE )[[1]]
)

p_stability <- wrap_plots(all_plot_list, ncol = 2, nrow = 2)

ggsave(file.path(base_dir, 'StabilityRank.png'),
       plot = p_stability, width = 13, height = 10, dpi = 600)
ggsave(file.path(base_dir, 'StabilityRank.svg'),
       plot = p_stability, width = 13, height = 10, dpi = 600)

# =============================================================================
# PLOT: CV ERROR METRICS
# =============================================================================

metrics_long <- rf_stacked$metrics %>%
   select(Fold, Test_Years, RMSE, MAE) %>%
   pivot_longer(cols = c(RMSE, MAE), names_to = 'Metric', values_to = 'Value')

p_error <- ggplot(metrics_long,
                  aes(x = Fold, y = Value, color = Metric, group = Metric)) +
   geom_line(linewidth = 1.2) +
   geom_point(size = 3.5) +
   scale_color_manual(values = c(RMSE = "#E07B3F", MAE = "#4A90D9")) +
   scale_x_continuous(breaks = rf_stacked$metrics$Fold,
                      labels = rf_stacked$metrics$Test_Years) +
   labs(x = "Fold (Test Year)", y = "Error", color = "Metric") +
   theme_rf() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'bottom')

ggsave(file.path(base_dir, 'CVErrorMetrics.png'),
       plot = p_error, width = 10, height = 6, dpi = 600)

cat("\nScript 03 complete. Plots saved to:", base_dir, "\n")

rm(list = ls())