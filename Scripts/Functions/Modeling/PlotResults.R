# Function to create summary plots for each stage
plot_stage_results <- function(stage_results, stage_name) {
   
   library(ggplot2)
   
   # Performance comparison plot
   p1 <- ggplot(stage_results$summary_table, aes(x = reorder(Predictor, Score), y = Score)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("Performance Scores:", stage_name),
           x = "Predictor", y = "Composite Score") +
      theme_minimal()
   
   # High salinity RMSE comparison
   p2 <- ggplot(stage_results$summary_table, aes(x = reorder(Predictor, -High_Sal_RMSE), y = High_Sal_RMSE)) +
      geom_col(fill = "coral", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("High Salinity RMSE:", stage_name),
           x = "Predictor", y = "High Salinity RMSE") +
      theme_minimal()
   
   return(list(score_plot = p1, rmse_plot = p2))
}