# ============================================================================
# 7. SAVE PLOTS FUNCTION
# ============================================================================

save_diagnostic_plots <- function(plots, stats, model_name, output_dir) {
   
   # Create model-specific directory
   model_dir <- file.path(output_dir, gsub("[^A-Za-z0-9]", "_", model_name))
   if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
   }
   
   # Save each plot category
   for (category in names(plots)) {
      if (length(plots[[category]]) > 0) {
         for (plot_name in names(plots[[category]])) {
            if (!is.null(plots[[category]][[plot_name]])) {
               filename <- file.path(model_dir, paste0(category, "_", plot_name, ".png"))
               ggsave(filename, plots[[category]][[plot_name]], 
                      width = 12, height = 8, dpi = 300)
            }
         }
      }
   }
   
   # Save statistics
   write.csv(stats, file.path(model_dir, "model_statistics.csv"), row.names = FALSE)
   
   cat("Plots saved to:", model_dir, "\n")
}
