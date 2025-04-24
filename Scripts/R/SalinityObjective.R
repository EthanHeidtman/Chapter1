################################################################################
# Written by Ethan Heidtman, March 2025

# This script uses modeled tidal data, observed discharge data for the Conowingo 
# Dam, and observed/modeled salinity data at Havre de Grace to first develop a 
# predictive relationship for salinity near the Havre de Grace Drinking water
# intake. The predictive relationship is then formulated into an objective function
# that represents a shortage index, the amount of time/probability that the Dam's
# releases are not enough to dilute salt below the safe threshold.


############################ Load Data and Packages ############################

      library('easypackages')
      libraries('here', 'rstan', 'dplyr', 'tidyverse', 'lubridate', 'zoo', 'ggplot2',
                'ggthemes')
      
      # 2006 FERC report: 
      # HdG drinking water intake is exposed to tidal influence when dam discharge 
      # falls below 4000 cfs (113.2 m3/sec)  # 4000 or 4500 cfs?
     
      # Create DateTime Object 
      datetime <- as.data.frame(seq(ymd_hms('1985-10-01 00:00:00'), ymd_hms('2024-08-31 00:00:00'), by = "15 min"))
      datetime <- with_tz(datetime, tzone = 'UTC') # Set proper timezone
      colnames(datetime) <- 'DateTime'
      datetime <- datetime %>%
         mutate(Year = year(DateTime),
                Month = month(DateTime),
                Day = day(DateTime))
      
      # Marietta Discharge Data: CUBIC METERS PER SECOND (Inflows to the reservoir)
      inflows <- read.csv('Data/Tidied/MariettaDischarge.csv')
      inflows <- inflows %>%
         dplyr::select(2, 3) %>%
         mutate(DateTime = as_datetime(DateTime)) %>% # Convert to datetime
         rename(Inflows = MAR_m3s)
      
      # Conowingo Discharge Data: CUBIC METERS PER SECOND (outflows from the turbines)
      discharge <- read.csv('Data/Tidied/ConowingoDischarge.csv')
      discharge <- discharge %>%
         dplyr::select(2, 3) %>%
         mutate(DateTime = as_datetime(DateTime)) %>% # Convert to datetime
         rename(Discharge = CON_m3s)
      
      # Salinity Data: MEASURED IN PRACTICAL SALINITY UNITS (PSU), AKA PARTS PER THOUSAND (PPT)
      salinity <- read.csv('Data/Tidied/DNRSalinityData.csv')
      salinity <- salinity %>%
         dplyr::select(2, 3) %>%
         mutate(DateTime = as_datetime(DateTime)) # Convert to datetime
      
      # Tidal Data (and Fitted): MEASURED IN METERS ABOVE THE MEAN LOWER LOW WATER (MLLW) DATUM
      tides <- read.csv('Data/Tidied/FittedTides.csv')
      tides <- tides %>%
         dplyr::select(2, 3, 4, 5, 7, 8, 12) %>%
         mutate(DateTime = as_datetime(DateTime)) %>% # Convert to datetime
         rename(Fitted_HdG = new_HdG)
      
      # Existing FERC Minimum Flow Requirement
      ferc <- data.frame(read.table('Data/Raw/Text/min_flow_req.txt'))
      ferc$DayOfYear <- seq(1, 365, 1)
      ferc$Date <- format(seq.Date(as.Date("2001-03-01"), as.Date("2002-02-28"), by = 1),
                          format = "%m-%d")
      ferc <- ferc %>%
         rename(flow = V1) %>%
         relocate(DayOfYear, Date) %>%
         mutate(FERC = flow * 0.0283) %>% # Convert from cfs to cubic feet per second
         slice(307:365, 1:306) %>% # order days so day 1 is Jan 1
         mutate(DayOfYear = seq(1, 365, 1)) %>%
         mutate_if(is.numeric, round, digits = 4)   
      
      # Create Dataframe of all data (15 MINUTE TEMPORAL RESOLUTION)
      data_15min <- merge(datetime, discharge, by = c('DateTime'), all = TRUE)
      data_15min <- merge(data_15min, inflows, by = c('DateTime'), all = TRUE)
      data_15min <- merge(data_15min, salinity, by = c('DateTime'), all = TRUE)
      data_15min <- merge(data_15min, tides, by = c('DateTime', 'Year', 'Month', 'Day'), all = TRUE)
      
      # Create Aggregated Dataframe of all data (HOURLY RESOLUTION)
      data <- data_15min %>%
         mutate(Hour = floor_date(DateTime, unit = 'hour')) %>% # Group dates by Hour
         group_by(Hour) %>%
         summarise(Inflows = mean(Inflows, na.rm = TRUE),
                   Discharge = mean(Discharge, na.rm = TRUE),
                   Salinity = mean(Salinity, na.rm = TRUE),
                   CCity = mean(CCity, na.rm = TRUE),
                   HdG = mean(HdG, na.rm = TRUE),
                   Fitted_HdG = mean(Fitted_HdG, na.rm = TRUE)) %>% # Compute hourly means
         ungroup() %>%
         rename(DateTime = Hour) %>%
         mutate(Year = year(DateTime),
                Month = month(DateTime),
                Day = day(DateTime),
                DayOfYear = pmin(yday(DateTime), 365)) %>%
         relocate(Year, Month, Day, DayOfYear, .after = DateTime) %>%
         mutate_at(vars(Inflows, Discharge, Salinity, CCity, HdG, Fitted_HdG), ~replace(., is.nan(.), NA)) %>% # Replace NaN with NA
         left_join(dplyr::select(ferc, DayOfYear, FERC), by = c('DayOfYear')) # Join the FERC requirement to the rest of the data
   
        
#### Developing a Predictive Relationship for Salt with Bayesian Inference #####
      
      # Susquehanna Morphological Characteristics
      d = 9.9 * 1.609 * 1000 # dam's distance from the mouth in meters (~9.9 miles)
      depth = 6 # average depth of the river from the dam to the mouth in meters
      width = 1600 # average width of the river from the dam to the mouth in meters
      area = depth * width # average cross-sectional area of the river below the dam  (m^2)
      
     
   
   
     
      violations <- data %>%
         mutate(Inflow_Violation = ifelse(!is.na(Inflows), Inflows < FERC, NA), 
                Discharge_Violation = ifelse(!is.na(Discharge), Discharge < FERC, NA)) %>%
         summarise(Inflow_Total = sum(!is.na(Inflows)),
                   Inflow_Violations = sum(Inflow_Violation, na.rm = TRUE),
                   Inflow_ViolationPerc = mean(Inflow_Violation, na.rm = TRUE) * 100, 
                   
                   Discharge_Total = sum(!is.na(Discharge)),
                   Discharge_Violations = sum(Discharge_Violation, na.rm = TRUE),
                   Discharge_ViolationPerc = mean(Discharge_Violation, na.rm = TRUE) * 100)
         
         
    
    
      
      
      
      
     
     
      
      ### Hourly Plots
      fdc <- data %>%
         pivot_longer(cols = c(Inflows, Discharge, FERC), names_to = 'Location', values_to = 'Flow') %>%
         filter(!is.na(Flow)) %>%
         group_by(Location) %>%
         arrange(desc(Flow)) %>%
         mutate(rank = row_number(),
                exceedance_prob = 100 * rank / n()) %>%
         ungroup()
      
      ggplot(fdc, aes(x = exceedance_prob, y = Flow, color = Location)) +
         geom_line() +
         scale_x_continuous(name = "Exceedance Probability (%)") +
         scale_y_log10(name = "Discharge (log scale)") +
         theme_bw() +
         ggtitle("Flow Duration Curves") +
         theme(legend.title = element_blank()) + 
         scale_color_manual(values = c('Inflows' = 'red', 'Discharge' = 'forestgreen', 'FERC' = 'black'))
      
    
      ggplot(data, aes(x = DateTime, y = Discharge)) + 
         geom_line(na.rm = TRUE) + 
         geom_line(aes(x = DateTime, y = FERC), color = 'red', na.rm = TRUE) + 
         geom_line(aes(x = DateTime, y = Inflows), color = 'forestgreen', na.rm = TRUE) + 
         scale_x_datetime(limits = c(as_datetime('2023-01-01'), as_datetime('2024-12-31'))) + 
         theme_bw() + 
         ylim(0, 10000) + 
         labs(x = 'DateTime', y = 'Discharge (cubic meters per second)')
      
      
      ggplot(data) + 
         geom_histogram(aes(x = Inflows, fill = 'Conowingo Inflows'), na.rm = TRUE, alpha = 1, bins = 50) + 
         geom_histogram(aes(x = Discharge, fill = 'Conowingo Discharge'), na.rm = TRUE, alpha = 0.7, bins = 50) + 
         scale_x_log10() +
         theme_bw() + 
         labs(x = 'Discharge (cubic m/s)', y = 'Observation Count', title = 'Histograms of Conowingo and Marietta Flows') + 
         scale_fill_manual(name = 'Location', values = c('Conowingo Inflows' = 'red', 'Conowingo Discharge' = 'forestgreen'))
      
      
      
      
      ### Monthly Plots
      monthly <- data %>%
         group_by(Year, Month) %>%
         summarise(across(Inflows : FERC, ~ mean(.x, na.rm = TRUE))) %>%
         ungroup() %>%
         mutate_at(vars(Inflows, Discharge, Salinity, CCity, HdG, Fitted_HdG, FERC), ~replace(., is.nan(.), NA)) %>% # Replace NaN with NA
         mutate(Date = as.Date(paste(Year, Month, 15, sep = "-")))
      
      ggplot(monthly) + 
         geom_histogram(aes(x = Inflows, fill = 'Marietta'), na.rm = TRUE, alpha = 1) + 
         geom_histogram(aes(x = Discharge, fill = 'Conowingo'), na.rm = TRUE, alpha = 0.7) + 
         theme_bw() + 
         scale_x_log10() + 
         scale_fill_manual(name = 'Location', values = c('Marietta' = 'red', 'Conowingo' = 'forestgreen')) + 
         labs(x = 'Flow (cubic m/s)', y = 'Count', title = 'Monthly Mean Flows')
      
      monthly_fdc <- monthly %>%
         pivot_longer(cols = c(Inflows, Discharge, FERC), names_to = 'Location', values_to = 'Flow') %>%
         filter(!is.na(Flow)) %>%
         group_by(Location) %>%
         arrange(desc(Flow)) %>%
         mutate(rank = row_number(),
                exceedance_prob = 100 * rank / n()) %>%
         ungroup()
      
      ggplot(monthly_fdc, aes(x = exceedance_prob, y = Flow, color = Location)) +
         geom_line() +
         scale_x_continuous(name = "Exceedance Probability (%)") +
         scale_y_log10(name = "Discharge (log scale)") +
         theme_bw() +
         ggtitle("Monthly Flow Duration Curves") +
         theme(legend.title = element_blank()) + 
         scale_color_manual(values = c('Inflows' = 'red', 'Discharge' = 'forestgreen', 'FERC' = 'black'))
      



#### Formulate Predictive Relationship as Shortage Objective Function to Minimize ####



##################### Comparison to Old FERC Requirement #######################

      