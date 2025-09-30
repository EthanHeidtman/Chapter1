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
source('Scripts/Modeling/SalinityModel.R')
source('Scripts/Plots/LRPlottingSuite.R')
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# DATA_PATH = 'Data/Tidied/Final/FinalModelData.qs'
# OUTPUT_PATH = 'Outputs/Experiments/Phase2_LogisticRegression'
# PLOT_PATH = 'Outputs/Plots/Phase2_LogisticRegression'

# DATA_PATH = 'Data/Tidied/Final/FinalModelData.qs'
# OUTPUT_PATH = 'Outputs/Experiments/Phase2_WeightedLR'
# PLOT_PATH = 'Outputs/Plots/Phase2_WeightedLR'

DATA_PATH = 'Data/Tidied/Final/FinalModelData.qs'
OUTPUT_PATH = 'Outputs/Experiments/Phase2_GAM'
PLOT_PATH = 'Outputs/Plots/Phase2_GAM'

data <- read_qs_files(DATA_PATH)
data <- data %>%
   dplyr::select(DateTime, Year, Month, Day, Salinity, DayOfYear, DayOfYear_sin, DayOfYear_cos, InflowDeficit, PowDischarge, Discharge, Inflows, PowInflows) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   group_by(Date) %>%
   summarise(across(c(2 : 13),  ~ mean(.x, na.rm = TRUE))) %>% # Compute daily mean
   mutate(LogInflows = log(Inflows),
          LogDischarge = log(Discharge),
          RollingInflows = zoo::rollmean(Inflows, k = 90, align = 'right', fill = NA, na.rm = TRUE),
          RollingPowInflows = zoo::rollmean(PowInflows, k = 90, align = 'right', fill = NA, na.rm = TRUE),
          RollingLogInflows = zoo::rollmean(LogInflows, k = 90, align = 'right', fill = NA, na.rm = TRUE)) %>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

# lm <- lm(data = data, Salinity ~ LogDischarge + RollingLogInflows + DayOfYear_sin + DayOfYear_cos, na.action = na.exclude)
# test <- data
# test$lm_fit <- predict(lm)
# 
# lm_predict <- predict(lm, data = data)
# lm_data <- cbind(data, as.data.frame(lm_predict))
# 
# gam <- gam(data = data, Salinity ~ s(LogDischarge) + s(RollingLogInflows) + DayOfYear_sin + DayOfYear_cos + ti(LogDischarge, RollingLogInflows), na.action = na.exclude)
# test$gam_fit <- predict(gam)
# 
# month <- data %>%
#    group_by(Year, Month) %>%
#    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
#    # make a Date column for the first day of each month
#    mutate(Date = as.Date(sprintf("%04d-%02d-01", Year, Month))) %>%
#    relocate(Date) %>%
#    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x))) %>%
#    mutate_if(is.numeric, round, digits = 3)
# 
# sal_range  <- range(data$Salinity,   na.rm = TRUE)
# flow_range <- range(data$LogInflows, na.rm = TRUE)
# scale_fac  <- diff(sal_range) / diff(flow_range)
# 
# ggplot(month, aes(x = Date)) +
#    # --- Salinity line (left axis) ---
#    geom_line(aes(y = Salinity), colour = "red") +
#    
#    # --- Raw LogInflows (rescaled to left axis) ---
#    geom_line(
#       aes(y = LogInflows * scale_fac + sal_range[1] -
#              flow_range[1] * scale_fac),
#       colour = "blue", alpha = 0.4
#    ) +
#    
#    # --- LOESS smooth for LogInflows (also rescaled) ---
#    # geom_smooth(
#    #    aes(y = LogDischarge * scale_fac + sal_range[1] -
#    #           flow_range[1] * scale_fac),
#    #    method = "loess", span = 0.5,
#    #    se = TRUE, colour = "blue", size = 0.8
#    # ) +
#    
#    # Axes
#    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
#    scale_y_continuous(
#       name = "Salinity (psu)",
#       sec.axis = sec_axis(
#          trans = ~ (. - sal_range[1] + flow_range[1] * scale_fac) / scale_fac,
#          name  = "Log(Inflows)"
#       )
#    ) +
#    facet_wrap(~Year, scales = "free_x") +
#    theme_bw() +
#    labs(title = "Havre de Grace Salinity and Conowingo Reservoir Inflows",
#         x = "Date") + 
#    theme(plot.title = element_text(size = 16, face = 'bold'),
#          axis.title = element_text(size = 14, face = 'bold'),
#          axis.text = element_text(size = 12))
# 
# 
# ggplot(test, aes(x = Date)) +
#    # --- Salinity line (left axis) ---
#    geom_line(aes(y = Salinity), colour = "red") +
#    
#    # Modeled Salinity
#    geom_line(aes(y = gam_fit), color = 'black') +
#    
#    # --- Raw LogInflows (rescaled to left axis) ---
#    geom_line(
#       aes(y = LogInflows * scale_fac + sal_range[1] -
#              flow_range[1] * scale_fac),
#       colour = "blue", alpha = 0.4
#    ) +
#    
#    # --- LOESS smooth for LogInflows (also rescaled) ---
#    geom_smooth(
#       aes(y = LogDischarge * scale_fac + sal_range[1] -
#              flow_range[1] * scale_fac),
#       method = "loess", span = 0.5,
#       se = TRUE, colour = "blue", size = 0.8
#    ) +
#    
#    # Axes
#    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
#    scale_y_continuous(
#       name = "Salinity (psu)",
#       sec.axis = sec_axis(
#          trans = ~ (. - sal_range[1] + flow_range[1] * scale_fac) / scale_fac,
#          name  = "Log(Inflows)"
#       )
#    ) +
#    facet_wrap(~Year, scales = "free_x") +
#    theme_bw() +
#    labs(title = "Havre de Grace Salinity and Conowingo Reservoir Inflows",
#         x = "Date") + 
#    theme(plot.title = element_text(size = 16, face = 'bold'),
#          axis.title = element_text(size = 14, face = 'bold'),
#          axis.text = element_text(size = 12))
# 
# ggplot(test %>% filter(Year == 2016), aes(x = Date)) + 
#    # --- Salinity line (left axis) ---
#    geom_line(aes(y = Salinity), colour = "red") +
#    
#    # Modeled Salinity
#    geom_line(aes(y = gam_fit), color = 'black', size = 1) +
#    
#    # --- Raw LogInflows (rescaled to left axis) ---
#    geom_line(aes(y = LogInflows * scale_fac + sal_range[1] -
#              flow_range[1] * scale_fac),
#       colour = "blue", alpha = 0.4) +
#    # --- LOESS smooth for LogInflows (also rescaled) ---
#    geom_smooth(aes(y = LogInflows * scale_fac + sal_range[1] -
#              flow_range[1] * scale_fac),
#       method = "loess", span = 0.5,
#       se = FALSE, colour = "blue", size = 1) +
#    # Axes
#    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#    scale_y_continuous(name = "Salinity (psu)", sec.axis = sec_axis(
#          trans = ~ (. - sal_range[1] + flow_range[1] * scale_fac) / scale_fac,
#          name  = "Log(Inflows)")) +
#    theme_bw() +
#    labs(title = "Havre de Grace Salinity and Conowingo Reservoir Inflows: 2016", x = "Date") + 
#    theme(plot.title = element_text(size = 16, face = 'bold'),
#          axis.title = element_text(size = 14, face = 'bold'),
#          axis.text = element_text(size = 12))


# Perform LR Model Runs
#lr_results <- run_multiple_lr_analyses(data, LR = TRUE, GAM = FALSE, weight = 1)

# Weighted LR
#weighted_lr <- run_multiple_lr_analyses(data, LR = TRUE, GAM = FALSE, weight = 5)

# Perform GAM Model Runs
gam_results <- run_multiple_lr_analyses(data, LR = FALSE, GAM = TRUE, weight = 1.5)



grid_plot <- create_threshold_grid_from_dirs(
   data_path = "Outputs/Experiments/Phase2_GAM/all_results.rds",
   predictor_combo = "RollingLogInflows_LogDischarge",
   pred1_col = "RollingLogInflows",
   pred2_col = "LogDischarge",
   prob_col = "exceedance_probability",
   actual_col = "actual_exceedance"
)
ggsave(paste0(PLOT_PATH, '/MatrixGridPlot_RollingLogInflows_LogDischarge.png'), grid_plot, width = 15, height = 11, dpi = 600)

grid_plot <- create_threshold_grid_from_dirs(
   data_path = "Outputs/Experiments/Phase2_GAM/all_results.rds",
   predictor_combo = "RollingPowInflows_PowDischarge",
   pred1_col = "RollingInflows",
   pred2_col = "PowDischarge",
   prob_col = "exceedance_probability",
   actual_col = "actual_exceedance"
)
ggsave(paste0(PLOT_PATH, '/MatrixGridPlot_RollingPowInflows_PowDischarge.png'), grid_plot, width = 15, height = 11, dpi = 600)

