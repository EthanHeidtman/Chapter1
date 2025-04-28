################################################################################
# Written by Ethan Heidtman, March 2025

# This script gathers all of the tidied data and creates an hourly aggregated
# dataframe that contains all of the necessary data for further analysis.

############################ Load Data and Packages ############################

      library('easypackages')
      libraries('here', 'rstan', 'dplyr', 'tidyverse', 
                'lubridate', 'zoo', 'ggplot2', 'ggthemes')
     
      # Create DateTime Object 
      datetime <- as.data.frame(seq(ymd_hms('1985-10-01 00:00:00'), 
                                    ymd_hms('2024-08-31 00:00:00'), by = "15 min"))
      datetime <- with_tz(datetime, tzone = 'UTC') # Set proper timezone
      colnames(datetime) <- 'DateTime'
      datetime <- datetime %>%
         mutate(Year = year(DateTime),
                Month = month(DateTime),
                Day = day(DateTime)) # Create year, month, and day variables
      
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
      
      # Existing FERC Minimum Flow Requirement: REPORTED IN CUBIC METERS PER SECOND
      ferc <- data.frame(read.table('Data/Raw/Text/min_flow_req.txt'))
      ferc$DayOfYear <- seq(1, 365, 1)
      ferc$Date <- format(seq.Date(as.Date("2001-03-01"), as.Date("2002-02-28"), by = 1),
                          format = "%m-%d")
      ferc <- ferc %>%
         rename(flow = V1) %>%
         relocate(DayOfYear, Date) %>%
         mutate(FERC = flow * 0.0283) %>% # Convert from cfs to cubic meters per second
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
         mutate(Hour = floor_date(DateTime, unit = 'hour')) %>% # Group dates by hour
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
         mutate_at(vars(Inflows, Discharge, Salinity, CCity, HdG, Fitted_HdG), 
                   ~replace(., is.nan(.), NA)) %>% # Replace NaN with NA
         left_join(dplyr::select(ferc, DayOfYear, FERC), 
                   by = c('DayOfYear')) # Join the FERC requirement to the rest of the data
      
      
      # Write the Aggregated Hourly Data to csv format
      write.csv(data, 'Data/Tidied/HourlyDataFinal.csv')
   
        
