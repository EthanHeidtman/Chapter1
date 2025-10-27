

source('Scripts/Utilities/ReadYearlyTextFiles.R')

library(dplyr)
library(lubridate)
library(ggplot2)

dir1 <- "Data/Raw/Text/SusquehannaBuoy/Meteo"
dir2 <- "Data/Raw/Text/SusquehannaBuoy/Ocean"

meteo <- combine_txt_files(dir1)
ocean <- combine_txt_files(dir2)

meteo <- meteo %>%
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>%
   dplyr::select(-c(YY, MM, DD, hh, mm)) %>%
   relocate(DateTime) %>%
   mutate(across(
      where(is.numeric),
      ~ if_else(grepl("^9+\\.?9*$", as.character(.x)), NA_real_, .x)
   )) %>%
   mutate(
      # Wind direction: convert FROM (meteorological) → TO (mathematical)
      theta = (270 - WDIR) * pi / 180,
      dx = WSPD * cos(theta),
      dy = WSPD * sin(theta)
   ) %>%
   mutate(Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime)

ocean <- ocean %>%
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>%
   dplyr::select(-c(YY, MM, DD, hh, mm)) %>%
   relocate(DateTime) %>%
   mutate(across(
      where(is.numeric),
      ~ if_else(grepl("^9+\\.?9*$", as.character(.x)), NA_real_, .x)
   ))

daily <- meteo %>%
   mutate(Date = as_date(DateTime)) %>%
   group_by(Year, Month, Day, Date) %>%
   summarise(
      dx_mean = mean(dx, na.rm = TRUE),
      dy_mean = mean(dy, na.rm = TRUE),
      WSPD_mean = sqrt(dx_mean^2 + dy_mean^2),        # resultant vector magnitude
      WDIR_mean = (270 - atan2(dy_mean, dx_mean) * 180 / pi) %% 360,  # back to met convention
      theta = (270 - WDIR_mean) * pi / 180,
      .groups = "drop"
   )



ggplot(daily, aes(x = Date)) +
   geom_hline(yintercept = 0, color = "gray60", linewidth = 0.3) +
   geom_segment(
      aes(
         xend = Date + 0.5,  # horizontal spacing tweak
         y = 0,
         yend = WSPD_mean * sin((270 - WDIR_mean) * pi / 180),
         color = WSPD_mean
      ),
      arrow = arrow(length = unit(0.08, "cm")),
      linewidth = 1.0
   ) +
   scale_color_viridis_c(option = "plasma") +
   labs(
      x = "Date",
      y = NULL,
      color = "Mean wind speed (m/s)",
      title = "Daily Mean Wind Speed and Direction at Havre de Grace, MD"
   ) +
   theme_bw() + 
   ylim(-8, 5) + 
   scale_x_date(
      limits = c(as_date('2016-09-20'), as_date('2016-10-31')),
      date_breaks = "1 week",           # tick mark every week
      date_labels = "%b %d %Y",            # label format (e.g., "Sep 10")
      expand = expansion(add = 0.5)     # small padding on ends
   )


