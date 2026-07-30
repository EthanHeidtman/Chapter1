# =============================================================================
# DIAGNOSTIC: Check k-sensitivity of mean_high_rmse before committing to a
# reduced/staged search strategy. Uses existing fit_gam() unmodified, coarse
# gam_levels=2. Purely diagnostic -- not the candidate pool used for final
# model selection.
# =============================================================================

t_diag <- system.time({
   k_sensitivity_check <- fit_gam(
      data                    = stacked_data,
      response                = 'Salinity_h',
      predictors              = SELECTED_PREDICTORS,
      folds                   = folds,
      family_type             = 'gaussian',
      link                    = NULL,
      high_salinity_threshold = HIGH_SALINITY_THRESHOLD,
      gam_levels              = 2,          # <-- coarse, diagnostic only
      nthreads                = 4,
      n_top_candidates        = 10,
      plot_output_dir         = 'Outputs/Plots/UnifiedGAM/KSensitivityCheck',
      n_workers               = N_WORKERS,
      show_progress           = TRUE,
      wind_ti_by              = TRUE
   )
})

cat(sprintf("\nDiagnostic wall time: %.1f min\n\n", t_diag["elapsed"] / 60))

# -----------------------------------------------------------------------------
# Sensitivity summary: CV of mean_high_rmse across all k-combos in the grid.
# Low CV supports the "k doesn't matter much" impression; high CV means the
# grid does need to stay closer to full resolution.
# -----------------------------------------------------------------------------

tr <- k_sensitivity_check$tune_grid %>%
   filter(n_failed == 0)   # drop combos where any fold failed to fit

sensitivity_summary <- tr %>%
   summarize(
      n_combos       = n(),
      mean_of_means  = mean(mean_high_rmse, na.rm = TRUE),
      sd_of_means    = sd(mean_high_rmse,   na.rm = TRUE),
      cv_pct         = 100 * sd_of_means / mean_of_means,
      range_min      = min(mean_high_rmse, na.rm = TRUE),
      range_max      = max(mean_high_rmse, na.rm = TRUE),
      range_pct_of_mean = 100 * (range_max - range_min) / mean_of_means
   )

print(sensitivity_summary)

# Also worth checking: is any single dimension driving what variation exists?
# (correlation of each k-column with mean_high_rmse, coarse but informative)
active_k_types <- k_sensitivity_check$fit_params$active_k_types
for (k_col in active_k_types) {
   cat(sprintf("cor(%s, mean_high_rmse) = %.3f\n",
               k_col, cor(tr[[k_col]], tr$mean_high_rmse, use = "complete.obs")))
}



t_diag2 <- system.time({
   k_interaction_check <- fit_gam(
      data                    = stacked_data,
      response                = 'Salinity_h',
      predictors              = SELECTED_PREDICTORS,
      folds                   = folds,
      family_type             = 'gaussian',
      link                    = NULL,
      high_salinity_threshold = HIGH_SALINITY_THRESHOLD,
      gam_levels              = 5,                 # applies to k_interaction (only active range)
      k_sustained_flow_range  = c(4, 4),            # fixed -- weak effect (r=0.15) in prior check
      k_flushing_flow_range   = c(12, 12),          # fixed -- weak effect (r=0.15), midpoint-ish
      k_wind_range            = c(8, 8),            # fixed -- ~no effect (r=-0.03), midpoint
      k_interaction_range     = c(6, 22),           # widened beyond original 6-14, since r=-0.72
      # was still trending negative at upper bound
      nthreads                = 4,
      n_top_candidates        = 5,
      plot_output_dir         = 'Outputs/Plots/UnifiedGAM/KInteractionCheck',
      n_workers               = N_WORKERS,
      show_progress           = TRUE,
      wind_ti_by              = TRUE
   )
})

 cat(sprintf("\nk_interaction diagnostic wall time: %.1f min\n\n", t_diag2["elapsed"] / 60))

tr2 <- k_interaction_check$tune_grid %>% filter(n_failed == 0)
print(tr2 %>% select(k_interaction, mean_rmse, mean_high_rmse, sd_high_rmse, n_folds_converged))

cat(sprintf("cor(k_interaction, mean_high_rmse) = %.3f\n",
            cor(tr2$k_interaction, tr2$mean_high_rmse, use = "complete.obs")))


t_diag3 <- system.time({
   k_interaction_check_v2 <- fit_gam(
      data                    = stacked_data,
      response                = 'Salinity_h',
      predictors              = SELECTED_PREDICTORS,
      folds                   = folds,
      family_type             = 'gaussian',
      link                    = NULL,
      high_salinity_threshold = HIGH_SALINITY_THRESHOLD,
      gam_levels              = 5,                 # k_interaction: ~6,8,10,12,14
      k_sustained_flow_range  = c(4, 4),            # fixed -- weak effect (r=0.15)
      k_flushing_flow_range   = c(12, 12),          # fixed -- weak effect (r=0.15)
      k_wind_range            = c(8, 8),            # fixed -- ~no effect (r=-0.03)
      k_interaction_range     = c(6, 14),           # narrowed from 6-22: minimum was at
      # k=10, both neighbors above 14 were
      # worse and 22 threw instability warnings
      nthreads                = 4,
      n_top_candidates        = 5,
      plot_output_dir         = 'Outputs/Plots/UnifiedGAM/KInteractionCheck_v2',
      n_workers               = N_WORKERS,
      show_progress           = TRUE,
      wind_ti_by              = TRUE
   )
})

cat(sprintf("\nk_interaction diagnostic v2 wall time: %.1f min\n\n", t_diag3["elapsed"] / 60))

tr3 <- k_interaction_check_v2$tune_grid %>% filter(n_failed == 0)
print(tr3 %>% select(k_interaction, mean_rmse, mean_high_rmse, sd_high_rmse, n_folds_converged))

