# ============================================================================
# 6. STATISTICS AND SUMMARY FUNCTIONS
# ============================================================================

calculate_model_statistics <- function(pred_df, model_name) {
   
   # Overall statistics
   overall_stats <- pred_df %>%
      summarise(
         n = n(),
         mae = mean(abs(Observed - Predicted), na.rm = TRUE),
         rmse = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
         r2 = cor(Observed, Predicted, use = "complete.obs")^2,
         bias = mean(Predicted - Observed, na.rm = TRUE),
         .groups = 'drop'
      ) %>%
      mutate(model = model_name, category = "Overall")
   
   # High salinity statistics
   high_stats <- pred_df %>%
      filter(is_high == TRUE) %>%
      summarise(
         n = n(),
         mae = mean(abs(Observed - Predicted), na.rm = TRUE),
         rmse = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
         r2 = cor(Observed, Predicted, use = "complete.obs")^2,
         bias = mean(Predicted - Observed, na.rm = TRUE),
         .groups = 'drop'
      ) %>%
      mutate(model = model_name, category = "High Salinity")
   
   # Combine statistics
   combined_stats <- bind_rows(overall_stats, high_stats)
   
   return(combined_stats)
}