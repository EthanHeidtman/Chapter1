# Source necessary functions
source('Scripts/Utilities/LoadTextFiles.R')
source('Scripts/Utilities/WriteQS.R')

# Load necessary packages
library(here)        # For directory referencing
library(tidyverse)   # For data manipulation
library(dplyr)       # For data manipulation
library(zoo)         # For rolling computation
library(lubridate)   # For datetime related functions
library(svglite)

# Directories where data are located
dir1 <- 'Data/Tidied/Processed/HourlyDataFinal.csv'
dir2 <- "Data/Raw/Text/SusquehannaBuoy/Meteo"

# Read in hourly Discharge and salinity data
q_sal_data <- read.csv(dir1, 
                       colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
q_sal_data <- q_sal_data %>%
   dplyr::select(-c(9, 10)) %>%                              # Remove extra columns
   mutate(DateTime = as_datetime(DateTime)) %>%              # Make dates class datetime
   rename(Tide = Fitted_HdG) %>%
   filter(DateTime < as_datetime('2024-11-01 00:00:00')) %>% # Keep only dates before 
   mutate_if(is.character, as.factor)

# Read in meteorology data, including wind
meteo <- combine_txt_files(dir2)
meteo <- meteo %>%
   mutate(DateTime = make_datetime(YY, MM, DD, hh, mm)) %>% # Make a datetime column
   dplyr::select(-c(YY, MM, DD, hh, mm)) %>%
   relocate(DateTime) %>%
   mutate(across(
      where(is.numeric),
      ~ if_else(grepl("^9+\\.?9*$", as.character(.x)), NA_real_, .x)
   )) %>%
   dplyr::select(1 : 4) %>%
   mutate(Year = year(DateTime),
          Month = month(DateTime),
          Day = day(DateTime)) %>%
   relocate(Year, Month, Day, .after = DateTime) %>%
   arrange(DateTime)

# Merge all data into 1 dataset
data <- merge(q_sal_data, meteo, by = c('DateTime', 'Year', 'Month', 'Day'), all.x = TRUE)
data <- data %>%
   filter(Year > 2006 & Year < 2025) %>%
   mutate_if(is.numeric, round, digits = 2) %>%
   rename(Gust = GST)

rm(meteo, q_sal_data, dir1, dir2)



library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(grid)  # for arrow()



# Shared X range
x_min <- as_datetime("2016-09-27")
x_max <- as_datetime("2016-11-05")
highlight_start <- as_datetime("2016-10-09")
highlight_end   <- as_datetime("2016-10-25")

# Raw hourly data for wind (before aggregation) 
data_raw <- data  # assumes data is still hourly when this runs

wind_df <- data_raw %>%
   filter(
      DateTime >= x_min,
      DateTime <= x_max,
      !is.na(WDIR), !is.na(WSPD)
   ) %>%
   # Thin to every 6 hours
   filter(hour(DateTime) %in% c(0, 12, 24)) %>%
   mutate(
      rad  = (270 - WDIR) * pi / 180,
      # Oceanographic: direction wind is blowing TOWARD
      uend = DateTime + lubridate::dhours(WSPD * cos(rad) * 4),  # 8hr scaling
      vend = WSPD * sin(rad)
   )

# Aggregate to daily resolution
plot_data <- data_raw %>%
   mutate(DateTime = as.Date(DateTime)) %>%
   group_by(DateTime, Year, Month, Day) %>%
   summarise(
      Salinity = max(Salinity, na.rm = TRUE),
      Tide     = max(Tide, na.rm = TRUE) - min(Tide, na.rm = TRUE),
      across(
         where(is.numeric) & !all_of(c("Salinity", "Tide")),
         ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
   ) %>%
   mutate(across(where(is.numeric), ~ round(.x, 2)))

plot_data[] <- lapply(plot_data, function(x) {
   x[is.nan(x) | is.infinite(x)] <- NA
   x
})

# Single definition — daily, POSIXct DateTime
plot_data_2016 <- plot_data %>%
   filter(Year == 2016) %>%
   mutate(DateTime = as_datetime(DateTime))

# Scaling for secondary axis
sal_range  <- range(plot_data_2016$Salinity,  na.rm = TRUE)
flow_range <- range(plot_data_2016$Discharge, na.rm = TRUE)
scale_fac  <- diff(sal_range) / diff(flow_range)

# Hourly background — from data_raw, scaled using daily ranges 
hourly_2016 <- data_raw %>%
   filter(DateTime >= x_min, DateTime <= x_max) %>%
   mutate(
      sal_scaled  = Salinity,
      flow_scaled = Discharge * scale_fac + sal_range[1] - flow_range[1] * scale_fac
   )

# WIND PANEL
wind_color <- "#8B4789"

wind_panel <- ggplot(wind_df, aes(x = DateTime)) +
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   geom_hline(yintercept = 0, color = "#a0b4be", linewidth = 0.3) +
   geom_segment(
      aes(
         xend = uend,
         y    = 0,
         yend = vend
      ),
      colour    = wind_color,
      arrow     = arrow(length = unit(0.1, "cm"), type = "closed"),
      linewidth = 0.6,
      lineend   = "round"
   ) +
   
   # ── Reference arrows ──────────────────────────────────────────────────────
   # Positioned in top-right, pointing straight up (due north = toward)
   annotate("segment",
            x = x_max - lubridate::ddays(8), xend = x_max - lubridate::ddays(8),
            y = 6, yend = 7, colour = wind_color, linewidth = 0.6,
            arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
   annotate("segment",
            x = x_max - lubridate::ddays(6), xend = x_max - lubridate::ddays(6),
            y = 6, yend = 9, colour = wind_color, linewidth = 0.6,
            arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
   annotate("segment",
            x = x_max - lubridate::ddays(4), xend = x_max - lubridate::ddays(4),
            y = 6, yend = 11, colour = wind_color, linewidth = 0.6,
            arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
   annotate("rect",
            xmin = x_max - lubridate::ddays(9),
            xmax = x_max - lubridate::ddays(3),
            ymin = 3.5, ymax = 12,
            fill = "white", colour = "#002030", linewidth = 0.3) +
   
   # Labels well below the base line
   annotate("text", x = x_max - lubridate::ddays(8), y = 4.5,
            label = "1 m/s", size = 2.8, colour = wind_color, hjust = 0.5) +
   annotate("text", x = x_max - lubridate::ddays(6), y = 4.5,
            label = "3 m/s", size = 2.8, colour = wind_color, hjust = 0.5) +
   annotate("text", x = x_max - lubridate::ddays(4), y = 4.5,
            label = "5 m/s", size = 2.8, colour = wind_color, hjust = 0.5) +
   # Redraw arrows and labels on top of the rect
   annotate("segment",
            x = x_max - lubridate::ddays(8), xend = x_max - lubridate::ddays(8),
            y = 6, yend = 7, colour = wind_color, linewidth = 0.6,
            arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
   annotate("segment",
            x = x_max - lubridate::ddays(6), xend = x_max - lubridate::ddays(6),
            y = 6, yend = 9, colour = wind_color, linewidth = 0.6,
            arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
   annotate("segment",
            x = x_max - lubridate::ddays(4), xend = x_max - lubridate::ddays(4),
            y = 6, yend = 11, colour = wind_color, linewidth = 0.6,
            arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
   coord_cartesian(ylim = c(-10, 14)) +
   scale_x_datetime(
      limits  = c(x_min, x_max),
      breaks  = seq(x_min, x_max, by = "6 days"),
      expand  = c(0, 0)
   ) + 
   scale_y_continuous(breaks = c(-5, 0, 5)) +
   labs(title = "October 2016 High Salinity Event", x = NULL, y = "Wind (m/s)") +
   theme_bw() +
   theme(
      plot.title       = element_text(size = 30, face = "bold", color = "#002030"),
      axis.title.y     = element_text(size = 20, face = "bold", color = wind_color),
      axis.text.y      = element_text(size = 20, colour = wind_color),
      axis.ticks.y     = element_line(colour = wind_color),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      panel.border     = element_rect(colour = "#002030", fill = NA, linewidth = 1),
      panel.grid.major = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(5.5, 5.5, 6, 5.5)
   )

# SALINTY/Discharge PANEL 
p1 <- ggplot(plot_data_2016, aes(x = DateTime)) +
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   geom_hline(yintercept = 0.5, color = "#002030", linetype = 2) +
   annotate("text",
            x = as_datetime(x_min + 0.30 * 3600 * 24), y = 0.52,
            label = "EPA Secondary Drinking Water Standard",
            hjust = 0, vjust = 0, size = 4.5, colour = "#002030") +
   
   # ── Hourly background lines ──
   geom_line(data = hourly_2016,
             aes(y = flow_scaled),
             colour = "#009bba", alpha = 0.40, linewidth = 0.3, na.rm = TRUE) +
   geom_line(data = hourly_2016,
             aes(y = sal_scaled),
             colour = "#f58220", alpha = 0.40, linewidth = 0.3, na.rm = TRUE) +
   
   # ── Daily bold lines ──
   geom_line(aes(y = Salinity),
             color = "#f58220", linewidth = 0.8) +
   geom_line(aes(y = Discharge * scale_fac + sal_range[1] -
                    flow_range[1] * scale_fac),
             colour = "#009bba", linewidth = 0.8, na.rm = TRUE) +
   
   scale_x_datetime(
      limits = c(x_min, x_max),
      expand = c(0, 0),
      labels = NULL,
      breaks = seq(x_min, x_max, by = "6 days")
   ) +
   scale_y_continuous(
      name = "Salinity (PSU)",
      sec.axis = sec_axis(
         trans = ~ (. - sal_range[1] + flow_range[1] * scale_fac) / scale_fac,
         name  = expression(paste("Discharge (", m^3, "/s)"))
      )
   ) +
   theme_bw() +
   labs(x = NULL) +
   theme(
      axis.title.x       = element_blank(),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.title.y.left  = element_text(size = 20, face = "bold", colour = "#f58220"),
      axis.title.y.right = element_text(size = 20, face = "bold", colour = "#009bba"),
      axis.text.y.left   = element_text(size = 20, colour = "#f58220"),
      axis.text.y.right  = element_text(size = 20, colour = "#009bba"),
      panel.border       = element_rect(colour = "#002030", fill = NA, linewidth = 1),
      panel.grid.major   = element_line(colour = "grey92"),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(0, 5.5, 0, 5.5)
   )
# TIDE PANEL
tide_panel <- ggplot(hourly_2016 %>% filter(!is.na(Tide)),
                     aes(x = DateTime, y = Tide)) +
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   # geom_ribbon(aes(ymin = min(Tide, na.rm = TRUE), ymax = Tide),
   #             fill = "#002030", alpha = 0.15) +
   geom_line(colour = "#002030", linewidth = 0.6) +
   scale_x_datetime(
      breaks      = seq(x_min, x_max, by = "6 days"),
      date_labels = "%b %d",
      limits      = c(x_min, x_max),
      expand      = c(0, 0)
   ) + 
   scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
   labs(x = "Date", y = "Tide Height (m)") +
   theme_bw() +
   theme(
      axis.title.x   = element_text(size = 20, face = "bold", color = "#002030"),
      axis.title.y   = element_text(size = 20, face = "bold", color = "#002030"),
      axis.text.x    = element_text(size = 15, color = "#002030"),
      axis.ticks.x   = element_line(color = "#002030"),
      axis.text.y    = element_text(size = 20, colour = "#002030"),
      panel.border   = element_rect(colour = "#002030", fill = NA, linewidth = 1),
      panel.grid     = element_blank(),
      plot.margin    = margin(6, 5.5, 5.5, 5.5),
      panel.grid.major = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
   )

# Assemble 
combined <- wind_panel / p1 / tide_panel +
   plot_layout(
      heights = c(1, 3, 1),
      guides  = "keep"
   ) &
   theme(plot.margin = margin(0.1, 5.5, 0.1, 5.5))

combined[[1]] <- combined[[1]] + theme(plot.margin = margin(5.5, 5.5, 0, 5.5))
combined[[3]] <- combined[[3]] + theme(plot.margin = margin(0, 5.5, 5.5, 5.5))

ggsave("Outputs/Plots/WindPanel.svg", wind_panel, height = 5, width = 13, device = svglite)
ggsave("Outputs/Plots/SaltPanel.svg", p1, height = 8, width = 13, device = svglite)
ggsave("Outputs/Plots/TidePanel.svg", tide_panel, height = 5, width = 13, device = svglite)

# Save the plot
ggsave("Outputs/Plots/SalinityWindTidePlot.png",
       combined, dpi = 600, height = 10, width = 13)
ggsave("Outputs/Plots/SalinityWindTidePlot.svg",
       combined, height = 10, width = 13, device = svglite)
