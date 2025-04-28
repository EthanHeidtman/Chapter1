################################################################################
# Written by Ethan Heidtman, March 2025

# This script performs a tidal data fitting for Havre de Grace, Maryland. 
# There is limited tidal data available for Havre de Grace, but quality sub-hourly
# tidal data ~15 miles away at Chesapeake City, Maryland. This script fits sine curves
# to each of the available tidal datasets (weighting toward the high tides because
# we care saltwater intrusion during high tide) and transforms the Chesapeake City
# tidal data to match the fitted period of Havre de Grace tidal data. 


############################ Load Data and Packages ############################
      
      # Load packages
      library(here)
      library('easypackages')
      libraries('tidyverse', 'sf', 'ggplot2', 'dplyr', 'maps', 'mapdata', 'cowplot',
                'rnaturalearth', 'rnaturalearthdata', 'readxl', 'raster', 'viridis', 
                'zoo', 'Kendall', 'ggpmisc', 'rtrend', 'RColorBrewer', 'EnvStats', 
                'trend', 'htmlTable', 'lubridate', 'Metrics')
      
      # Source functions
      source('Scripts/Functions/TideFittingFunction.R')
      
      # Read in Datasets
      HdG <- read.csv('Data/Tidied/HavreDeGraceTides.csv')
      HdG <- HdG %>%
         dplyr::select(2, 3) %>%
         mutate(DateTime = as_datetime(DateTime)) %>%
         rename(HdG = Tide_HdG)
      CCity <- read.csv('Data/Tidied/ChesapeakeCityTides.csv')
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
         mutate(time_years = as.numeric(Year - Year[1])) # Count years from first year 

########################### Fit and Transform Tidal Data #######################
      # see /Scripts/Functions/TideFittingFunction.R for details
      
      # Weighting Method: Transforming CCity tides to HdG tides
      
         # Initial Parameter Estimates for Sine Fit
         A <- (max(tides$HdG, na.rm = TRUE) - min(tides$HdG, na.rm = TRUE)) / 2 # Amplitude
         B <- 2 * pi / 12.42 # Tidal frequency
         C <- 0 # Phase shift
         D <- mean(tides$HdG, na.rm = TRUE) # mean height
         
         # Assign weights to higher tides (we care about salt events)
         tides$HdG_weights <- ifelse(tides$HdG > quantile(tides$HdG, 0.75, na.rm = TRUE) | 
                                        tides$HdG < quantile(tides$HdG, 0.25, na.rm = TRUE), 4, 1) # assign weight of 4 to highest and lowest 25%, 1 to middle 50$
         tides$CCity_weights <- ifelse(tides$CCity > quantile(tides$CCity, 0.75, na.rm = TRUE) | 
                                          tides$CCity < quantile(tides$CCity, 0.25, na.rm = TRUE), 4, 1) # assign weight of 4 to highest and lowest 25%, 1 to middle 50%
      
         # Perform the model fit
         tides <- tidal_fitting(tides, A, B, C, D)
         
         # Assessing the Model Fit (only for the overlapping ~9 months of data)
         #tides <- na.omit(tides) # if we want to omit consider only the times when the tides overlap between HdG and CCity
         # errors <- tides %>% group_by(Month) %>%
         #   summarize(RMSE = rmse(Tide_HdG, new_HdG),
         #             r2 = cor(Tide_HdG, new_HdG)^2) %>%
         #   summarise(RMSE = mean(RMSE), 
         #             r2 = mean(r2))
         
         
         # Write a csv file containing the tide data
         write.csv(tides, 'Data/Tidied/FittedTides.csv')

############################### Plotting #######################################

monthly_tides <- tides %>%
   group_by(Month, Year) %>%
   summarise(Tide_CCity = mean(Tide_CCity, na.rm = TRUE),
             Tide_HdG = mean(Tide_HdG, na.rm = TRUE),
             new_HdG = mean(new_HdG, na.rm = TRUE))



p1 <- ggplot(tides, aes(x = DateTime)) +
   geom_line(aes(y = Tide_HdG, color = 'Measured'), na.rm = TRUE, linewidth = 1.1) +
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
         text = element_text(family = 'Nunito'),
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'))

p2 <- ggplot(tides, aes(x = Tide_HdG, y = new_HdG)) +
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
         text = element_text(family = 'Nunito'),
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


############################### Organize Data ##################################
# Create Dataframe with all Data
all <- merge(DateTime, CB2.2_Salinity, by = c('DateTime'), all = TRUE)
all <- merge(all, DNR_Salinity, by = c('DateTime'), all = TRUE)
all <- merge(all, CON_Obs, by = c('DateTime'), all = TRUE)
all <- merge(all, MAR_Obs, by = c('DateTime'), all = TRUE)
all <- merge(all, tides[, c(1, 11)], by = c('DateTime'), all = TRUE)
colnames(all) <- c('DateTime', 'CB2.2', 'DNR', 'Temp_DNR', 'Conowingo', 'Marietta', 'Fitted_Tide')
rm(CB2.2_Salinity, DNR_Salinity)
all <- all[!duplicated(all[c('DateTime')], fromLast = TRUE), ] # Remove duplicate datetimes

all <- all %>%
   mutate(Date = as.Date(DateTime),
          Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Date, Year, Month, Day, .after = DateTime)





# Summarize data by day
daily <- all %>%
   group_by(Date) %>%
   summarise(CB2.2 = max(CB2.2, na.rm = TRUE),
             DNR = max(DNR, na.rm = TRUE),
             Conowingo = mean(Conowingo, na.rm = TRUE),
             Marietta = mean(Marietta, na.rm = TRUE),
             Tide = max(Fitted_Tide, na.rm = TRUE)) %>% 
   mutate_if(is.numeric, round, digits = 2) %>%
   mutate(Year = year(Date),
          Month = month(Date),
          Day = day(Date)) %>%
   relocate(Year, Month, Day, .after = Date)
daily[daily == as.numeric('-Inf')] <- NA
daily$Conowingo[is.nan(daily$Conowingo)] <- NA
daily$Marietta[is.nan(daily$Marietta)] <- NA



   



test <- all %>%
   filter(Month > 3 & Month < 12) %>%
   filter(DateTime > '2007-01-01')

# 3 Panel Plot: Salt, Tide, Discharge
p1 <- ggplot(all, aes(x = DateTime)) + 
   #geom_line(aes(y = CB2.2 , color = 'CB2.2'), na.rm = TRUE) + 
   geom_point(aes(y = DNR), na.rm = TRUE, color = 'red', linewidth = 0.5, size = 0.2) + 
   theme_bw() + 
   labs(x = 'Datetime', y = 'Salinity (psu)', title = 'Havre de Grace Salinity') + 
   # scale_color_manual(name = 'Location', values = c('CB2.2' = 'green', 'Havre de Grace' = 'red')) + 
   scale_x_datetime(breaks = '3 years', date_labels = '%m/%Y', limits = c(as_datetime('2007-03-01'), as_datetime('2024-09-30'))) + 
   theme(plot.title = element_text(size = 16),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(), 
         axis.text.y = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.justification = c(0,0.5),
         legend.text = element_text(size = 12), 
         legend.position = 'right', 
         plot.margin = unit(c(0, 0, 0, 0), 'cm'))
p2 <- ggplot(all %>% filter(!is.na(Fitted_Tide)), aes(x = DateTime)) + # Because there are 3 NAs btwn each datapoint, subset the tide data to get rid of NAs and show on plot
   geom_line(aes(y = Fitted_Tide), na.rm = TRUE, color = '#97BEE5', linewidth = 0.3, size = 0.3) + 
   theme_bw() + 
   labs(x = 'Datetime', y = 'Tide Height (m)', title = 'Reconstructed Havre de Grace Tide Height') + 
   scale_x_datetime(breaks = '3 years', date_labels = '%m/%Y', limits = c(as_datetime('2007-03-01'), as_datetime('2024-09-30'))) + 
   theme(plot.title = element_text(size = 14),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(), 
         axis.text.y = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.justification = c(0,0.5),
         legend.text = element_text(size = 12), 
         plot.margin = unit(c(0, 0, 0, 0), 'cm'))
p3 <- ggplot(all, aes(x = DateTime, y = Conowingo)) + 
   geom_line(na.rm = TRUE, color = '#FFC000', linewidth = 0.5) + 
   theme_bw() + 
   labs(x = 'Datetime', y = 'Discharge (cubic m / s)', title = 'Susquehanna Discharge at Conowingo Dam') + 
   scale_x_datetime(breaks = '3 years', date_labels = '%m/%Y', limits = c(as_datetime('2007-03-01'), as_datetime('2024-09-30'))) + 
   theme(plot.title = element_text(size = 14),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.justification = c(0,0.5),
         legend.text = element_text(size = 12), 
         plot.margin = unit(c(0, 0, 0, 0), 'cm'))
plot <- plot_grid(p1, p2, p3, ncol = 1, nrow = 3, align = 'hv')
ggsave('Panel_Plot.png', plot, path = '~/Documents/Penn State/Projects/HdG Salinity/Figures', width = 10, height = 8, dpi = 700)












flow_quantiles <- daily %>%
   dplyr::select(1, 2, 3, 4, 7, 8) %>%
   mutate(MD = yday(Date)) %>% # convert date to day of year
   mutate(Period = cut(Year, breaks = c(1996, 2003, 2010, 2017, 2024), labels = c('1997-2003', '2004-2010', '2011-2017', '2018-2024')))

flow_stats <- flow_quantiles %>%
   group_by(MD) %>% # group by each day of the year
   summarise(Mean = mean(Marietta, na.rm = TRUE)) %>% # compute mean for each day of year
   pivot_longer(cols = -c(MD), names_to = 'statistic', values_to = 'value') %>% # pivot to long format for plotting
   mutate(value_log = log10(value)) %>% # compute log10 of data
   mutate_if(is.numeric, round, digits = 2)

flow_quantiles <- flow_quantiles %>%
   group_by(MD) %>%
   summarise(as_tibble_row(quantile(Marietta, probs = c(0, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 1), na.rm = TRUE))) %>% # compute quantiles at each %
   pivot_longer(cols = -c(MD), names_to = 'quantile', values_to = 'value') %>% # pivot to long format for plotting
   mutate(value_log = log10(value)) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   mutate(quantile = factor(quantile, levels = c('0%', '2.5%', '10%', '25%', '50%', '75%', '90%', '97.5%', '100%'))) # order the factor variable

ggplot(flow_quantiles, aes(x = MD, y = value, group = quantile, fill = quantile)) +
   geom_area(na.rm = TRUE, position = position_stack(reverse = TRUE)) + theme_bw() + 
   # geom_line(data = flow_stats, aes(x = MD, y = value, group = statistic), inherit.aes = FALSE, color = 'black', linewidth = 1.5) + 
   #geom_line(data = ferc, aes(x = Day, y = Discharge_log), color = 'red', linewidth = 1.5, inherit.aes = FALSE)
   labs(x = 'Day of Year', y = 'Discharge (cubic meter/sec)', title = 'Marietta Discharge by Day') + 
   scale_fill_viridis(option = 'turbo', discrete = TRUE, name = 'Flow Quantiles') + 
   scale_x_continuous(breaks = c(1, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365)) + 
   #facet_wrap(~Period) + 
   theme(axis.title.x = element_text(size = 13),
         axis.title.y = element_text(size = 13),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         legend.title = element_text(size = 13),
         legend.justification = c(0,0.5),
         legend.text = element_text(size = 12),
         plot.title = element_text(size = 16),
         strip.text.x = element_text(size = 12))

# ggsave(filename = 'WebsitePlot.png', width = 10, height = 6, plot, dpi = 600,
#        path = '/Users/ethanheidtman/Documents/Penn State/Projects/HdG Salinity/')
# 

