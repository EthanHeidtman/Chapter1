# =============================================================================
# Script Name:    LogisticRegression.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)
library(zoo)
source('Scripts/Utilities/ExperimentHelpers.R')
source('Scripts/Utilities/SavePlots.R')
source('Scripts/Plots/ModelScreeningPlots.R')
source('Scripts/Plots/MultiPanelModelPlot.R')
source('Scripts/Plots/EvalLogisticPerformance.R')
source('Scripts/Plots/FacetLogisticMatrixPlot.r')
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

DATA_PATH = 'Data/Tidied/Final/FinalModelData.qs'

# DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
# FERC_PATH = 'Data/Tidied/Processed/FERCFlowRequirement.csv'
OUTPUT_PATH = 'Outputs/Experiments/Phase2_LogisticRegression'
PLOT_PATH = 'Outputs/Plots/Phase2_LogisticRegression'

data <- read_qs_files(DATA_PATH)
data <- data %>%
   dplyr::select(DateTime, Year, Month, Day, Salinity, DayOfYear, DayOfYear_sin, DayOfYear_cos, InflowDeficit, PowDischarge, Discharge, Inflows, PowInflows) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   group_by(Date) %>%
   summarise(across(c(2 : 13),  ~ mean(.x, na.rm = TRUE))) %>%
   mutate(RollingInflows = zoo::rollmean(Inflows, k = 90, align = 'right', fill = NA, na.rm = TRUE),
          RollingPowInflows = zoo::rollmean(PowInflows, k = 90, align = 'right', fill = NA, na.rm = TRUE)) %>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))


# # Extract legend from *one* plot so it's consistent
# legend_plot <- create_legend_plot(data_median, grid_df, "Norm_InflowDeficit", "Norm_PowDischarge", prob_breaks) + 
#    theme(legend.position = 'bottom')
# legend_plot <- legend_plot +
#    guides(
#       fill = guide_legend(),
#       color = guide_legend()
#    )
# legend_grob <- cowplot::get_legend(legend_plot)
# legend_panel <- cowplot::ggdraw(legend_grob)

run_multiple_lr_analyses <- function(data, 
                                     output_path = OUTPUT_PATH, 
                                     plot_path = PLOT_PATH,
                                     threshold_quantiles = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.96, 0.97, 0.98, 0.99, 1.0),
                                     predictor_combinations = list(
                                        c('Inflows', 'Discharge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                        c('PowInflows', 'PowDischarge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                        c('RollingInflows', 'Discharge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                        c('RollingInflows', 'PowDischarge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                        c('RollingPowInflows', 'Discharge', 'DayOfYear_sin', 'DayOfYear_cos'),
                                        c('RollingPowInflows', 'PowDischarge', 'DayOfYear_sin', 'DayOfYear_cos')
                                     )) {
   
   results_summary <- data.frame()
   all_results <- list()
   
   counter <- 1
   for (thresh in threshold_quantiles) {
      for (preds in predictor_combinations) {
         
         cat("Running analysis", counter, "- Threshold:", thresh, "Predictors:", paste(preds, collapse = ", "), "\n")
         
         # Run the analysis
         result <- run_logistic_regression_analysis(
            data = data,
            threshold_quantile = thresh,
            other_preds = preds,
            output_path = output_path,
            plot_path = plot_path,
            predictor_vars = c(preds[1 : 2])
         )
         
         # Store results
         all_results[[counter]] <- result
         
         # Add to summary
         results_summary <- rbind(results_summary, data.frame(
            run_id = counter,
            threshold_quantile = thresh,
            threshold_value = result$threshold_value,
            predictors = paste(preds, collapse = ", "),
            folder_name = result$folder_name,
            stringsAsFactors = FALSE
         ))
         
         counter <- counter + 1
      }
   }
   
   # Save overall summary
   write.csv(results_summary, file.path(output_path, "analysis_summary.csv"), row.names = FALSE)
   saveRDS(all_results, file.path(output_path, "all_results.rds"))
   
   return(list(
      summary = results_summary,
      all_results = all_results
   ))
}


all_results <- run_multiple_lr_analyses(data)


grid_plot <- create_threshold_grid_from_dirs(
   base_path = "Outputs/Experiments/Phase2_LogisticRegression",
   folder_pattern = "threshq", 
   predictor_combo = "RollingPowInflows_PowDischarge",
   pred1_col = "RollingPowInflows",
   pred2_col = "PowDischarge",
   prob_col = "exceedance_probability",
   actual_col = "actual_exceedance"
)
ggsave(paste0(PLOT_PATH, '/MatrixGridPlot_RollingPowInflows_PowDischarge.png'), grid_plot, width = 15, height = 11, dpi = 600)

grid_plot <- create_threshold_grid_from_dirs(
   base_path = "Outputs/Experiments/Phase2_LogisticRegression",
   folder_pattern = "threshq",  # Changed from "threshq" to catch both formats
   predictor_combo = "PowInflows_PowDischarge",
   pred1_col = "PowInflows",
   pred2_col = "PowDischarge",
   prob_col = "exceedance_probability",
   actual_col = "actual_exceedance"
)
ggsave(paste0(PLOT_PATH, '/MatrixGridPlot_PowInflows_PowDischarge.png'), grid_plot, width = 15, height = 11, dpi = 600)

