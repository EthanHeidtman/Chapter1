# =============================================================================
# Script Name:    05_GAMCandidateSelection.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    Standalone candidate-selection plotting for the unified
#                 multi-horizon GAM. Reads the metadata object saved by
#                 Script 04 (CandidateGAMs_Metadata.qs2) and reproduces the
#                 original 8 diagnostic plots (moved here unchanged from
#                 fit_gam), plus a new 2-panel accuracy-vs-complexity figure:
#                 Panel 1 shows the full candidate cloud (all k-combos passing
#                 the convergence/sanity gate), Panel 2 zooms to the top ~10
#                 candidates by mean high-salinity RMSE. Intended for
#                 supplemental figure iteration without re-running the ~15 min
#                 CV fit in fit_gam.
# =============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)

source('Scripts/Utilities/ReadQS.R')
source('Scripts/Utilities/WriteQS.R')
source('Scripts/Utilities/FitGAM_PARALLEL.R')

# =============================================================================
# PARAMETERS
# =============================================================================

plot_output_dir <- 'Outputs/Plots/UnifiedGAM/GAMSelection'
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# Placeholder -- inspect the total_edf distribution (see diagnostic print
# below) before trusting this. Given k_interaction_range/k_wind_range etc.,
# legitimate total_edf should sit in the low hundreds at most.
EDF_CEILING <- 1000

gam_colors <- list(
   primary   = "#f58220",
   secondary = "#009bba",
   tertiary  = "#fdb515",
   dark      = "#002030"
)

gam_theme <- theme_bw() +
   theme(
      plot.title    = element_text(size = 16, face = "bold", color = gam_colors$dark),
      plot.subtitle = element_text(size = 13,                color = gam_colors$dark),
      axis.title    = element_text(size = 14, face = "bold", color = gam_colors$dark),
      axis.text     = element_text(size = 12,                color = gam_colors$dark),
      panel.border  = element_rect(colour = gam_colors$dark, fill = NA, linewidth = 1),
      legend.title  = element_text(size = 12, face = "bold", color = gam_colors$dark)
   )

# =============================================================================
# LOAD DATA
# =============================================================================

gam_candidates <- read_qs_files('Outputs/Models/UnifiedGAM/CandidateGAMs_Metadata.qs2')

candidate_summary_raw <- gam_candidates$candidate_summary
edf_tables             <- gam_candidates$edf_tables
fold_cv_all            <- gam_candidates$fold_cv_all
top_candidates_meta    <- gam_candidates$top_candidates
n_folds                <- gam_candidates$top_candidates$n_folds_total[1]

# =============================================================================
# GATING
# =============================================================================

cat("=== PRE-GATE DIAGNOSTIC ===\n")
cat("Candidates before gating:", nrow(candidate_summary_raw), "\n")
cat("total_edf range:", paste(round(range(candidate_summary_raw$total_edf), 1), collapse = " - "), "\n")
cat("total_edf distribution (quantiles):\n")
print(quantile(candidate_summary_raw$total_edf, probs = c(0, .25, .5, .75, .9, .95, .99, 1)))
cat("\n")

candidate_summary <- candidate_summary_raw %>%
   filter(
      n_folds_converged > n_folds_total / 2,
      total_edf < EDF_CEILING,
      mean_high_rmse < 2
   ) %>%
   arrange(mean_high_rmse) %>%
   mutate(candidate_rank = row_number(), label = paste0("C", candidate_rank))

cat("=== POST-GATE ===\n")
cat("Candidates after gating:", nrow(candidate_summary),
    "(dropped", nrow(candidate_summary_raw) - nrow(candidate_summary), ")\n")
cat("total_edf range (gated):", paste(round(range(candidate_summary$total_edf), 1), collapse = " - "), "\n\n")

# Re-key edf_tables and fold_cv_all to the re-ranked candidate_rank via the
rank_lookup <- candidate_summary_raw %>%
   select(candidate_rank_orig = candidate_rank, k_index) %>%
   inner_join(
      candidate_summary %>% select(candidate_rank_gated = candidate_rank, k_index),
      by = "k_index"
   )

edf_all <- bind_rows(edf_tables) %>%
   filter(!is.na(edf)) %>%
   inner_join(rank_lookup, by = c("candidate_rank" = "candidate_rank_orig")) %>%
   select(-candidate_rank) %>%
   rename(candidate_rank = candidate_rank_gated) %>%
   mutate(term_short = vapply(term, function(term) {
      if (grepl("^ti\\(h,RollingWindCross", term)) {
         days <- sub(".*RollingWindCross([0-9]+).*", "\\1", term)
         if (grepl("WindDirLeftBank", term))  return(paste0("h x ", days, " Day Easterly Wind"))
         if (grepl("WindDirRightBank", term)) return(paste0("h x ", days, " Day Westerly Wind"))
      }
      if (grepl("^ti\\(h,", term)) return(paste0("h x ", sub("^ti\\(h,([^,)]+).*$", "\\1", term)))
      if (grepl("^s\\(RollingWindCross", term)) {
         days <- sub(".*RollingWindCross([0-9]+).*", "\\1", term)
         if (grepl("WindDirLeftBank", term))  return(paste0(days, " Day Easterly Wind"))
         if (grepl("WindDirRightBank", term)) return(paste0(days, " Day Westerly Wind"))
      }
      sub("^s\\(([^)]+)\\)$", "\\1", term)
   }, character(1)))

fold_profiles <- fold_cv_all %>%
   inner_join(top_candidates_meta %>% select(k_index, candidate_rank), by = "k_index") %>%
   inner_join(rank_lookup, by = c("candidate_rank" = "candidate_rank_orig")) %>%
   select(-candidate_rank) %>%
   rename(candidate_rank = candidate_rank_gated) %>%
   filter(!is.na(high_rmse))

candidate_summary_top10 <- candidate_summary %>% slice_head(n = 10)
edf_all_top10           <- edf_all %>% filter(candidate_rank %in% candidate_summary_top10$candidate_rank)
fold_profiles_top10     <- fold_profiles %>% filter(candidate_rank %in% candidate_summary_top10$candidate_rank)

# =============================================================================
# ORIGINAL 8 PLOTS (moved unchanged from fit_gam, now built from gated data)
# =============================================================================

pA <- candidate_summary %>%
   mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
   ggplot(aes(x = total_edf, y = mean_high_rmse, label = label)) +
   geom_errorbar(aes(ymin = mean_high_rmse - se_high_rmse, ymax = mean_high_rmse + se_high_rmse), width = 1.5, color = "grey60") +
   geom_point(size = 3.5, color = gam_colors$primary) +
   ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
   labs(title = "Accuracy vs Complexity", x = "Total EDF", y = "Mean High-Salinity RMSE (ppt)") +
   gam_theme

pB <- candidate_summary %>%
   mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
   ggplot(aes(x = mean_high_rmse, y = se_high_rmse, color = total_edf, label = label)) +
   geom_point(size = 3.5) +
   ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
   scale_color_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "Total EDF") +
   labs(title = "Accuracy vs Consistency", x = "Mean High-Salinity RMSE (ppt)", y = "SE of High-Salinity RMSE") +
   gam_theme

pC <- ggplot(edf_all, aes(x = factor(candidate_rank, labels = paste0("C", sort(unique(candidate_rank)))),
                          y = reorder(term_short, edf, FUN = mean), fill = edf)) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(edf, 1)), size = 3, color = "white", fontface = "bold") +
   scale_fill_gradient(low = gam_colors$secondary, high = gam_colors$primary) +
   labs(title = "Per-Term EDF", x = "Candidate", y = "Smooth Term") +
   gam_theme

pD <- ggplot(fold_profiles, aes(x = fold, y = high_rmse, color = factor(candidate_rank), group = factor(candidate_rank))) +
   geom_line(linewidth = 1.1) +
   geom_point(size = 2.8) +
   labs(title = "High-Salinity RMSE by Fold", x = "CV Fold", y = "High-Salinity RMSE") +
   scale_x_continuous(breaks = seq_len(n_folds)) +
   gam_theme

pA_top10 <- candidate_summary_top10 %>%
   mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
   ggplot(aes(x = total_edf, y = mean_high_rmse, color = total_edf, label = label)) +
   geom_errorbar(aes(ymin = mean_high_rmse - se_high_rmse, ymax = mean_high_rmse + se_high_rmse), width = 1.5, color = "grey60") +
   geom_point(size = 3.5) +
   ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
   scale_color_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "Total EDF") +
   labs(title = "Accuracy vs Complexity (Top 10)", x = "Total EDF", y = "Mean High-Salinity RMSE (ppt)") +
   gam_theme

pB_top10 <- candidate_summary_top10 %>%
   mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
   ggplot(aes(x = mean_high_rmse, y = se_high_rmse, color = total_edf, label = label)) +
   geom_point(size = 3.5) +
   ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
   scale_color_gradient(low = gam_colors$secondary, high = gam_colors$primary, name = "Total EDF") +
   labs(title = "Accuracy vs Consistency (Top 10)", x = "Mean High-Salinity RMSE (ppt)", y = "SE of High-Salinity RMSE") +
   gam_theme

pC_top10 <- ggplot(edf_all_top10, aes(x = factor(candidate_rank, labels = paste0("C", sort(unique(candidate_rank)))),
                                      y = reorder(term_short, edf, FUN = mean), fill = edf)) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(edf, 1)), size = 3, color = "white", fontface = "bold") +
   scale_fill_gradient(low = gam_colors$secondary, high = gam_colors$primary) +
   labs(title = "Per-Term EDF (Top 10)", x = "Candidate", y = "Smooth Term") +
   gam_theme

pD_top10 <- ggplot(fold_profiles_top10, aes(x = fold, y = high_rmse, color = factor(candidate_rank), group = factor(candidate_rank))) +
   geom_line(linewidth = 1.1) +
   geom_point(size = 2.8) +
   labs(title = "High-Salinity RMSE by Fold (Top 10)", x = "CV Fold", y = "High-Salinity RMSE") +
   scale_x_continuous(breaks = seq_len(n_folds)) +
   gam_theme

# =============================================================================
# 2-PANEL ACCURACY-VS-COMPLEXITY (full cloud + zoomed top 10)
# =============================================================================

pFull <- candidate_summary %>%
   mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
   ggplot(aes(x = total_edf, y = mean_high_rmse)) +
   geom_errorbar(aes(ymin = mean_high_rmse - se_high_rmse, ymax = mean_high_rmse + se_high_rmse),
                 width = 0.3, color = "grey75", alpha = 0.5) +
   geom_point(size = 1.6, color = gam_colors$primary, alpha = 0.6) +
   labs(title = "A)", x = "Total EDF", y = "Mean High-Salinity RMSE (ppt)") +
   gam_theme

pZoom <- candidate_summary_top10 %>%
   mutate(se_high_rmse = sd_high_rmse / sqrt(n_folds)) %>%
   ggplot(aes(x = total_edf, y = mean_high_rmse, label = label)) +
   geom_errorbar(aes(ymin = mean_high_rmse - se_high_rmse, ymax = mean_high_rmse + se_high_rmse),
                 width = 0.3, color = "grey60") +
   geom_point(size = 3.5, color = gam_colors$primary) +
   ggrepel::geom_text_repel(size = 4, color = gam_colors$dark, fontface = "bold") +
   labs(title = "B)", x = "Total EDF", y = "Mean High-Salinity RMSE (ppt)") +
   gam_theme

pParetoPanel <- patchwork::wrap_plots(pFull, pZoom, ncol = 2)

# =============================================================================
# SAVE PLOTS
# =============================================================================

for (p_info in list(
   list(p = pA,           name = "AccuracyVsComplexity",        w = 8,  h = 6),
   list(p = pB,           name = "AccuracyVsConsistency",       w = 8,  h = 6),
   list(p = pC,           name = "EDFHeatmap",                  w = 10, h = max(6, n_distinct(edf_all$term_short) * 0.35 + 2)),
   list(p = pD,           name = "FoldProfiles",                w = 10, h = 6),
   list(p = pA_top10,     name = "AccuracyVsComplexity_Top10",  w = 8,  h = 6),
   list(p = pB_top10,     name = "AccuracyVsConsistency_Top10", w = 8,  h = 6),
   list(p = pC_top10,     name = "EDFHeatmap_Top10",            w = 10, h = max(6, n_distinct(edf_all_top10$term_short) * 0.35 + 2)),
   list(p = pD_top10,     name = "FoldProfiles_Top10",          w = 10, h = 6),
   list(p = pParetoPanel, name = "AccuracyVsComplexity_Pareto2Panel", w = 14, h = 6)
)) {
   ggsave(file.path(plot_output_dir, paste0(p_info$name, ".png")),
          p_info$p, width = p_info$w, height = p_info$h, dpi = 600)
   ggsave(file.path(plot_output_dir, paste0(p_info$name, ".svg")),
          p_info$p, width = p_info$w, height = p_info$h)
}

cat("\nPlots saved to", plot_output_dir, "\n")

# =============================================================================
# INTERACTIVE CANDIDATE SELECTION & FINAL REFIT
# =============================================================================

if (interactive()) {
   selected_rank <- NA
   while (is.na(selected_rank) || !(selected_rank %in% 1:10)) {
      user_input <- readline(prompt = "Evaluate plots, then enter selected candidate rank (1-10): ")
      selected_rank <- as.integer(trimws(user_input))
      if (is.na(selected_rank) || !(selected_rank %in% 1:10)) {
         cat("Invalid input. Please enter an integer from 1 to 10.\n")
      }
   }
   
   # Translate selected post-gate rank (1-10) to pre-gate candidate rank
   orig_rank <- rank_lookup %>%
      filter(candidate_rank_gated == selected_rank) %>%
      pull(candidate_rank_orig)
   
   cat(sprintf("\n=== Refitting Candidate Rank %d (Original Rank %d) ===\n", selected_rank, orig_rank))
   
   gam_unified <- select_gam_candidate(candidates_output = gam_candidates, rank = orig_rank)
   
   write_qs_files(list(gam_unified), 'Outputs/Models/UnifiedGAM', list('GamUnified'))
   
   cat("\nScript 05 complete. Final model saved to Outputs/Models/UnifiedGAM/GamUnified.qs2\n")
} else {
   cat("\nNon-interactive execution detected. Plots generated; run interactively to select and refit the final model.\n")
}

rm(list = ls())
