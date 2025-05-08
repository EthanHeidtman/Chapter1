########################### Read in packages ###################################
      library(here)
      library('easypackages')
      libraries('tidyverse', 'ggplot2', 'dplyr', 'cowplot',
                'readxl','viridis', 
                'zoo', 'RColorBrewer', 'lubridate')
    
    
################################### Read in and Tidy Data ###############################
      # Create a DateTime column
          DateTime <- as.data.frame(seq(ymd_hms('1985-10-01 00:00:00'), ymd_hms('2024-08-31 00:00:00'), by = "15 min"))
          DateTime <- with_tz(DateTime, tzone = 'UTC')
          colnames(DateTime) <- 'DateTime'
      
      # Read Susquehanna River HdG_DNR_Salinity Data (measured at Havre de Grave, MD)
      # Measurements taken every 15 minutes from march through October, starting in 2007
      # HdG_DNR_Salinity in ppt (thousand), and temp in C
          DNR_Salinity <- read.csv('Data/Raw/CSV/EOTBData_Susquehanna_30Mar07_TO_06Nov24.csv')
          DNR_Salinity$DateTime <- as.POSIXct(DNR_Salinity$DateTime, format = '%m/%d/%y %H:%M', tz = 'UTC')
          DNR_Salinity <- DNR_Salinity %>%
            dplyr::select(3,6,7)
          colnames(DNR_Salinity) <- c('DateTime', 'Salinity', 'Temp')
          DNR_Salinity$DateTime <- round_date(DNR_Salinity$DateTime, unit = '15 mins') # Round dates to nearest 15 minutes
          
      # Read in Conowingo Dam discharge data taken from USGS 01578310
      # Data are every 15 minutes from 02/02/1988 through 10/31/2024, measured discharge in cubic feet per second
          CON_Obs <- read.csv('Data/Raw/CSV/Conowingo_Discharge.csv')
          other <- read.csv('Data/Raw/CSV/Conowingo_Discharge2.csv')
          CON_Obs <- rbind(CON_Obs, other)
          rm(other)
          CON_Obs$DateTime <- paste(CON_Obs$Date, CON_Obs$Time, sep = ' ')              # Join date and time column
          CON_Obs$DateTime <- as.POSIXct(CON_Obs$DateTime, 
                                         format = '%m/%d/%y %H:%M', tz = 'UTC')         # Parse to class DateTime
          CON_Obs <- CON_Obs %>%
            relocate(DateTime, .after = 'Time') %>%
            mutate(CON_m3s = Discharge * 0.0283) %>%                                    # convert to cubic meters per second
            mutate_if(is.numeric, round, digits = 2) %>%
            dplyr::select(4,6) %>%
            mutate(DateTime = round_date(DateTime, unit = '15 mins'))                   # Round dates to nearest 15 minutes
      
      # Read in FERC Minimum Flow Requirement
          ferc <- data.frame(read.table('Data/Raw/Text/min_flow_req.txt'))
          ferc$Day <- seq(1, 365, 1)
          ferc$Date <- format(seq.Date(as.Date("2001-03-01"), as.Date("2002-02-28"), by = 1),
                              format = "%m-%d")
          ferc <- ferc %>%
            rename(flow = V1) %>%
            relocate(Day, Date) %>%
            mutate(Discharge = flow * 0.0283,
                   Discharge_log = log10(Discharge)) %>%                                # Convert from cfs to cubic feet per second
            slice(307:365, 1:306) %>%                                                   # order days so day 1 is Jan 1
            mutate(Day = seq(1, 365, 1)) %>%
            mutate_if(is.numeric, round, digits = 4)
      
      # Read in Marietta Discharge Data from USGS 01576000
      # Data are every 30 minutes from 1985 through 
          MAR_Obs <- read.csv('Data/Raw/CSV/Marietta_Discharge.csv')
          MAR_Obs$DateTime <- paste(MAR_Obs$Date, MAR_Obs$Time, sep = ' ')              # Join date and time column
          MAR_Obs$DateTime <- as.POSIXct(MAR_Obs$DateTime, 
                                         format = '%m/%d/%y %H:%M', tz = 'UTC')         # Parse to class DateTime
          MAR_Obs <- MAR_Obs %>%
            relocate(DateTime, .after = 'Time') %>%
            mutate(MAR_m3s = Discharge * 0.0283) %>%                                    # convert to cubic meters per second
            mutate_if(is.numeric, round, digits = 2) %>%
            dplyr::select(4, 6) %>%
            filter(DateTime < '2024-11-01 00:00:00') %>%
            mutate(DateTime = round_date(DateTime, '30 mins'))
          
      # Read in Modeled Hourly Salinity Data at Location CB2.2. Units are ppt
      # This model output is from Ming Li's group 
          CB2.2_Salinity <- read.csv('Data/Raw/CSV/CB2.2.csv')
          CB2.2_Salinity$DateTime <- paste(CB2.2_Salinity$date, CB2.2_Salinity$time, sep = ' ')
          CB2.2_Salinity$DateTime <- as_datetime(CB2.2_Salinity$DateTime, 
                                                 format = '%m/%d/%y %H:%M', tz = 'UTC')
          CB2.2_Salinity <- CB2.2_Salinity %>%
            dplyr::select(3,4) %>%
            relocate(DateTime)
          colnames(CB2.2_Salinity) <- c('DateTime', 'Salinity')
          
      # Read More Modeled Hourly Salinity Data (units ppt): THIS LOCATION IS CB1.1, HAVRE DE GRACE
          HdG_Salinity <- read.csv('Data/Raw/CSV/Havre_de_Grace.csv')
          HdG_Salinity$DateTime <- as_datetime(HdG_Salinity$time, format = '%m/%d/%y %H:%M', tz = 'UTC')
          HdG_Salinity <- HdG_Salinity %>%
            dplyr::select(2, 3) %>%
            relocate(DateTime) %>%
            rename(Salinity = Drinking.water.intake.at.Havre.de.Grace)
          CB2.2_Salinity <- merge(DateTime, CB2.2_Salinity, by = c('DateTime'), all.x = TRUE)
          CB2.2_Salinity <- merge(CB2.2_Salinity, HdG_Salinity, by = c('DateTime', 'Salinity'), all.x = TRUE, all.y = TRUE)
          rm(HdG_Salinity)
          
      
      # Read in tidal data for Havre de Grace and Chesapeake City (datum = mean lower low water, units = m, timezone = Greenwich mean time)
          HdG_Tide <- read.csv('Data/Raw/CSV/HdG_Tides.csv')
          HdG_Tide <- HdG_Tide %>%
            mutate(Time..GMT. = paste(Time..GMT., ':00', sep = '')) %>%
            mutate(DateTime = paste(Date, Time..GMT., sep = ' ')) %>%
            mutate(DateTime = as_datetime(DateTime, tz = 'GMT')) %>%                    # Convert to datetime
            mutate(DateTime = with_tz(DateTime, 'UTC')) %>%                             # Set proper timzeone
            dplyr::select(5, 6) %>%
            relocate(DateTime) %>%
            rename(Tide_HdG = Verified..m.) %>%
            mutate_if(is.character, as.numeric)
          CCity_Tide <- read.csv('Data/Raw/CSV/ChesapeakeCity_Tides.csv')
          CCity_Tide <- CCity_Tide %>%
            mutate(Time..GMT. = paste(Time..GMT., ':00', sep = '')) %>%
            mutate(DateTime = paste(Date, Time..GMT., sep = ' ')) %>%
            mutate(DateTime = as_datetime(DateTime, format = '%m/%d/%y %H:%M:%S')) %>%  # Convert to Datetime
            mutate(DateTime = with_tz(DateTime, 'UTC')) %>% # Set proper timezone
            dplyr::select(5, 6) %>%
            relocate(DateTime) %>%
            rename(Tide_CCity = Verified..m.) %>%
            mutate(Tide_CCity = as.numeric(Tide_CCity, na.rm = TRUE))
        
############################ Write Tidied Data #################################
      write.csv(CB2.2_Salinity, 'Data/Tidied/CB2.2_SalinityData.csv')
      write.csv(CCity_Tide, 'Data/Tidied/ChesapeakeCityTides.csv')
      write.csv(HdG_Tide, 'Data/Tidied/HavreDeGraceTides.csv')
      write.csv(CON_Obs, 'Data/Tidied/ConowingoDischarge.csv')
      write.csv(MAR_Obs, 'Data/Tidied/MariettaDischarge.csv')
      write.csv(DateTime, 'Data/Tidied/DateTime_1985-10-01_to_2024-08-31.csv')
      write.csv(DNR_Salinity, 'Data/Tidied/DNRSalinityData.csv')
      write.csv(ferc, 'Data/Tidied/FERCFlowRequirement.csv')
        
