# =============================================================================
# Script Name:    FitTidalData.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Description:    This script combines two nearby tidal height datasets, one at
#                 Havre de Grace, MD, and the other at Chesapeake City, MD. A 
#                 sine curve is fit to each dataset, and then the Chesapeake City
#                 tides are transformed to match the HdG sine curve. This produces
#                 a complete timeseries of tidal range at HdG.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
# Load packages
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(Metrics)

# Source functions
source('Scripts/Utilities/FitTidalData.R')

# Read in Datasets
HdG <- read.csv('Data/Tidied/Processed/HavreDeGraceTides.csv')
HdG <- HdG %>%
   dplyr::select(2, 3) %>%
   mutate(DateTime = as_datetime(DateTime)) %>%
   rename(HdG = Tide_HdG)
CCity <- read.csv('Data/Tidied/Processed/ChesapeakeCityTides.csv')
CCity <- CCity %>%
   dplyr::select(2, 3) %>%
   mutate(DateTime = as_datetime(DateTime)) %>%
   rename(CCity = Tide_CCity)

# Create a dataframe with all tidal data
tides <- merge(CCity, HdG, by = 'DateTime', all.x = TRUE, all.y = TRUE)
tides <- tides %>%
   mutate(time_hours = as.numeric(difftime(DateTime, DateTime[1], units = 'hour'))) %>% # count hours from first hour for model fitting
   mutate(Year = year(DateTime),
          Month = month(DateTime), 
          Day = day(DateTime)) %>%
   relocate(time_hours, .after = DateTime) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   mutate(time_years = as.numeric(Year - Year[1]))                                      # Count years from first year 

########################### Fit and Transform Tidal Data #######################
# see /Scripts/Functions/TideFittingFunction.R for details

# Weighting Method: Transforming CCity tides to HdG tides

# Initial Parameter Estimates for Sine Fit
A <- (max(tides$HdG, na.rm = TRUE) - min(tides$HdG, na.rm = TRUE)) / 2               # Amplitude
B <- 2 * pi / 12.42 # Tidal frequency
C <- 0 # Phase shift
D <- mean(tides$HdG, na.rm = TRUE) # mean height

# Assign weights to higher tides (we care about salt events)
# assign weight of 4 to highest and lowest 25%, 1 to middle 50%
tides$HdG_weights <- ifelse(tides$HdG > quantile(tides$HdG, 0.75, na.rm = TRUE) | 
                               tides$HdG < quantile(tides$HdG, 0.25, na.rm = TRUE), 4, 1) 
tides$CCity_weights <- ifelse(tides$CCity > quantile(tides$CCity, 0.75, na.rm = TRUE) | 
                                 tides$CCity < quantile(tides$CCity, 0.25, na.rm = TRUE), 4, 1) 

# Perform the model fit
tides <- tidal_fitting(tides, A, B, C, D)

#Assessing the Model Fit (only for the overlapping ~9 months of data)
tides <- na.omit(tides) # if we want to omit consider only the times when the tides overlap between HdG and CCity
errors <- tides %>% group_by(Month) %>%
  summarize(RMSE = rmse(HdG, new_HdG),
            r2 = cor(HdG, new_HdG)^2) %>%
  summarise(RMSE = mean(RMSE),
            r2 = mean(r2))


# Write a csv file containing the tide data
write.csv(tides, 'Data/Tidied/Processed/FittedTides.csv')

############################### Plotting #######################################

monthly_tides <- tides %>%
   group_by(Month, Year) %>%
   summarise(Tide_CCity = mean(CCity, na.rm = TRUE),
             Tide_HdG = mean(HdG, na.rm = TRUE),
             new_HdG = mean(new_HdG, na.rm = TRUE))



p1 <- ggplot(tides, aes(x = DateTime)) +
   geom_line(aes(y = HdG, color = 'Measured'), na.rm = TRUE, linewidth = 1.1) +
   geom_line(aes(y = new_HdG, color = 'Fitted'), na.rm = TRUE, linetype = 1, linewidth = 1) +
   geom_point(aes(y = new_HdG), color = '#97BEE5', na.rm = TRUE, size = 2) +
   theme_bw() +
   scale_x_datetime(limits = c(as_datetime('2005-08-01 00:00:00'), as_datetime('2005-08-10 23:59:00'))) +
   labs(x = 'DateTime (UTC)', y = 'Tidal Height (m)', title = 'Tidal Height at Havre de Grace, MD') +
   scale_color_manual(name = 'Havre de Grace Tides', limits = c('Measured', 'Fitted'), values = c('Measured' = 'black', 'Fitted' = '#97BEE5')) +
   ylim(-0.25, 1.1) + 
   theme(plot.title = element_text(size = 16),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.justification = c(0,0.5),
         legend.text = element_text(size = 12), 
         legend.position = 'bottom',
         axis.title = element_text(size = 14),
         #text = element_text(family = 'Nunito'),
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'))

p2 <- ggplot(tides, aes(x = HdG, y = new_HdG)) +
   geom_point(na.rm = TRUE, alpha = 0.5, color = '#97BEE5') +
   theme_bw() +
   geom_abline(color = 'black') +
   stat_poly_line(se = FALSE, linetype = 2, na.rm = TRUE, color = 'red') + stat_poly_eq(use_label('eq', 'r2', 'P'), na.rm = TRUE, color = 'red', size = 7) +
   xlim(-0.25,  1.3) + ylim(-0.1, 1.1) +
   labs(x = 'Measured Tide (m)', y = 'Fitted Tide (m)', title = 'Measured vs Fitted Tide at Havre de Grace, MD') +
   theme(plot.title = element_blank(),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.title = element_text(size = 16),
         legend.justification = c(0,0.5),
         legend.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         #text = element_text(family = 'Nunito'),
         plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), 'cm'),
         legend.position = 'none')

plot <- plot_grid(p1, p2, ncol = 1, nrow = 2, align = 'hv')
ggsave('Fitting.svg', plot, path = '~/Documents/Penn State/Projects/HdG Salinity/Figures', dpi = 700, height = 10, width = 8)
ggsave('Fitting.png', plot, path = '~/Documents/Penn State/Projects/HdG Salinity/Figures', dpi = 600, height = 10, width = 8)

# ggplot(tides, aes(x = DateTime)) + 
#   geom_line(aes(y = Tide_HdG, color = 'Havre de Grace'), na.rm = TRUE) +
#   geom_line(aes(y = Tide_CCity, color = 'Chesapeake City'), na.rm = TRUE) +
#   geom_point(aes(y = diff, color = 'Difference'), na.rm = TRUE, linetype = 2) + 
#   #geom_smooth(method = 'nls', se = TRUE) + 
#   theme_bw() + 
#   theme(plot.title = element_text(size = 14),
#         axis.text.x = element_text(size = 12, angle = 10),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 14),
#         legend.justification = c(0,0.5),
#         legend.text = element_text(size = 12)) + 
#   scale_x_datetime(date_labels = '%b %d %Y %H:%M', 
#                    limits = c(as_datetime('2005-09-01 00:00:00'), as_datetime('2005-09-04 23:59:00')),
#                    date_breaks = '4 hours') + 
#   labs(x = 'Datetime (UTC)', y = 'Tide Height (m)', title = 'Tidal Patterns: Upper Chesapeake Bay') +
#   scale_color_manual(name = 'Tidal Monitor', limits = c('Havre de Grace', 'Chesapeake City', 'Difference'), values = c('Havre de Grace' = 'red',
#                                                                                                                        'Chesapeake City' = 'blue',
#                                                                                                                        'Difference' = 'black'))
