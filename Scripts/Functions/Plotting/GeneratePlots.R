# Main diagnostic function - generates all plots for a model
generate_model_diagnostics <- function(model, data, model_name = "Linear Model", 
                                       model_type = "linear", save_plots = FALSE, 
                                       output_dir = "~/Outputs/ModelingPlots") {
   
   # Get predictions using your existing function
   pred_df <- get_predictions(model, data, model_type)
   
   # Create output directory if saving
   if (save_plots && !dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
   }
   
   cat("Generating comprehensive diagnostics for:", model_name, "\n")
   
   # Generate all diagnostic plots
   plots <- list()
   
   # 1. Overall Performance Plots
   plots$performance <- generate_performance_plots(pred_df, model_name)
   
   # 2. High Salinity Event Analysis
   plots$high_salinity <- generate_high_salinity_plots(pred_df, model_name)
   
   # 3. Correlation and Relationship Analysis
   plots$correlations <- generate_correlation_plots(pred_df, model_name)
   
   # 4. Temporal Analysis
   plots$temporal <- generate_temporal_plots(pred_df, model_name)
   
   # 5. Residual Analysis
   plots$residuals <- generate_residual_plots(pred_df, model_name)
   
   # 6. Model Summary Statistics
   stats <- calculate_model_statistics(pred_df, model_name)
   
   # Save plots if requested
   if (save_plots) {
      save_diagnostic_plots(plots, stats, model_name, output_dir)
   }
   
   # Print summary statistics
   #print_model_summary(stats)
   
   return(list(plots = plots, statistics = stats))
}
