# Source necessary functions 
dirs <- c("Scripts/Utilities")
invisible(
   lapply(dirs, function(dir) {
      files <- list.files(dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
      lapply(files, function(f) {
         sys.source(f, envir = globalenv())
      })
   })
)

# Read in model data
model_data <- as.data.frame(read_qs_files('Data/Tidied/Final/FinalModelData.qs'))
model_data <- model_data %>%
   dplyr::select(-contains('Norm')) %>%
   arrange(DateTime) %>%
   mutate(Date = as_date(DateTime)) %>%
   relocate(Date, .after = DateTime) %>%
   filter(DateTime > '2007-03-29') 

plot_data <- model_data %>%
   mutate(LogInflows = log(Inflows)) %>%
   relocate(LogInflows, .after = Salinity) 

highlight_start <- as_datetime("2016-10-09")
highlight_end   <- as_datetime("2016-10-25")

sal_range  <- range(plot_data$Salinity,   na.rm = TRUE)
flow_range <- range(plot_data$LogInflows, na.rm = TRUE)
scale_fac  <- diff(sal_range) / diff(flow_range)

p1 <- ggplot(plot_data %>% filter(Year == 2016), aes(x = DateTime)) + 
   #--- Yellow highlight background ---
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   
   # EPA Total Dissolved Solids
   geom_hline(yintercept = 0.5, color = '#002030', linetype = 2) + 
   
   annotate("text",
            x = as.POSIXct("2016-09-13"),  # Left side of your plot
            y = 0.52,                       # Just above the 0.5 line
            label = "EPA Secondary Drinking Water Standard for TDS",
            hjust = 0,                      # Left-align the text
            vjust = 0,                      # Bottom-align (sits above the line)
            size = 5,
            colour = "#002030") +      
   
   # --- Salinity line (left axis) ---
   geom_line(aes(y = Salinity), color = "#f58220", size = 0.8) +
   
   # --- Raw LogInflows (rescaled to left axis) ---
   geom_line(aes(y = LogInflows * scale_fac + sal_range[1] -
                    flow_range[1] * scale_fac),
             colour = "#009bba", alpha = 0.4, na.rm = TRUE) +
   
   # --- LOESS smooth for LogInflows (also rescaled) ---
   geom_smooth(aes(y = LogInflows * scale_fac + sal_range[1] -
                      flow_range[1] * scale_fac),
               method = "loess", span = 0.5,
               se = FALSE, colour = "#009bba", size = 1, na.rm = TRUE) +
   
   # --- Axes ---
   scale_x_datetime(date_breaks = "6 days", date_labels = "%b %d",
                limits = c(as_datetime('2016-09-13'), as_datetime('2016-10-31'))) +
   scale_y_continuous(
      name = "Salinity (psu)",
      sec.axis = sec_axis(
         trans = ~ exp((. - sal_range[1] + flow_range[1] * scale_fac) / scale_fac),
         name  = expression(paste("Inflows (", m^3, "/s)"))
      )
   ) +
   
   # --- Theming ---
   theme_bw() +
   labs(
      title = "October 2016 High Salinity Event",
      x = "Date"
   ) +
   theme(
      text               = element_text(family = "Franklin Gothic ATF"),
      plot.title         = element_text(size = 30, face = 'bold', color = '#002030'),
      axis.title.x       = element_text(size = 28, face = 'bold', color = '#002030'),
      axis.title.y.left  = element_text(size = 28, face = 'bold', colour = "#f58220"),
      axis.title.y.right = element_text(size = 28, face = 'bold', colour = "#009bba"),
      axis.text.y.left   = element_text(colour = "#f58220"),
      axis.text.y.right  = element_text(colour = "#009bba"),
      axis.text.x        = element_text(size = 20),
      axis.text.y        = element_text(size = 24),
      panel.border       = element_rect(colour = '#002030', fill = NA, linewidth = 1),
   )



ggsave('Outputs/Plots/SalinityInflows2016Plot.png', p1, dpi = 600, height = 8, width = 13)
ggsave('Outputs/Plots/SalinityInflows2016Plot.svg', p1, dpi = 600, height = 8, width = 13)



library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(grid)  # for arrow()

# ── Shared x limits ──────────────────────────────────────────────────────────
x_min <- as_datetime("2016-09-25")
x_max <- as_datetime("2016-11-05")
highlight_start <- as_datetime("2016-10-09")
highlight_end   <- as_datetime("2016-10-25")

plot_data_2016 <- data %>% filter(Year == 2016)

# ── Scaling for main panel ────────────────────────────────────────────────────
sal_range  <- range(plot_data_2016$Salinity,   na.rm = TRUE)
flow_range <- range(plot_data_2016$Discharge, na.rm = TRUE)
scale_fac  <- diff(sal_range) / diff(flow_range)

# ── Wind color scale ──────────────────────────────────────────────────────────
# Tertiary yellow: light tint → full saturated yellow → dark anchor
wind_low  <- "#fef0c0"   # very light yellow tint
wind_high <- "#fdb515"   # full tertiary yellow

# ── WIND PANEL ────────────────────────────────────────────────────────────────
wind_panel <- ggplot(wind_df, aes(x = DateTime)) +
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   geom_hline(yintercept = 0, color = "#a0b4be", linewidth = 0.3) +
   geom_segment(
      aes(
         xend   = xend,
         y      = 0,
         yend   = yend,
         colour = WSPD
      ),
      arrow     = arrow(length = unit(0.1, "cm"), type = "closed"),
      linewidth = 0.9,
      lineend   = "round"
   ) +
   scale_colour_gradient(
      low   = wind_low,
      high  = wind_high,
      name  = "Wind\nspeed\n(m/s)"
   ) +
   scale_x_datetime(limits = c(x_min, x_max), expand = c(0, 0)) +
   scale_y_continuous(breaks = NULL, expand = expansion(mult = 0.15)) +
   labs(title = "October 2016 High Salinity Event", x = NULL, y = "Wind (m/s)") +
   theme_bw() +
   theme(
      plot.title       = element_text(size = 30, face = "bold", color = "#002030"),
      axis.title.y     = element_text(size = 20, face = "bold", color = "#fdb515"),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks.y     = element_blank(),
      panel.border     = element_rect(colour = "#002030", fill = NA, linewidth = 1),
      panel.grid       = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = 10, face = "bold", color = "#002030"),
      legend.text      = element_text(size = 9),
      legend.margin         = margin(0, 0, 0, 2),      # pulls legend toward panel
      legend.box.margin     = margin(0, 0, 0, -8),     # closes gap further
      plot.margin      = margin(5.5, 5.5, 6, 5.5)      # small bottom gap
   )
# ── MAIN PANEL — no title, no x-axis labels ───────────────────────────────────
p1 <- ggplot(plot_data_2016, aes(x = DateTime)) +
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   geom_hline(yintercept = 0.5, color = "#002030", linetype = 2) +
   annotate("text",
            x = as_datetime(x_min + 5 * 3600 * 24), y = 0.52,
            label = "EPA Secondary Drinking Water Standard",
            hjust = 0, vjust = 0, size = 4.5, colour = "#002030") +
   geom_line(aes(y = Salinity), color = "#f58220", linewidth = 0.8) +
   geom_line(aes(y = Discharge * scale_fac + sal_range[1] -
                    flow_range[1] * scale_fac),
             colour = "#009bba", linewidth = 0.8, na.rm = TRUE) +
   scale_x_datetime(
      limits  = c(x_min, x_max),
      expand  = c(0, 0),
      labels  = NULL,   # suppress labels but keep limits consistent
      breaks  = seq(x_min, x_max, by = "6 days")
   ) +
   scale_y_continuous(
      name = "Salinity (psu)",
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
      plot.margin        = margin(0, 5.5, 0, 5.5)
   )

# ── TIDE PANEL ────────────────────────────────────────────────────────────────
tide_panel <- ggplot(plot_data_2016 %>% filter(!is.na(Tide)),
                     aes(x = DateTime, y = Tide)) +
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "#fdb515", alpha = 0.2) +
   geom_ribbon(aes(ymin = min(Tide, na.rm = TRUE), ymax = Tide),
               fill = "#002030", alpha = 0.15) +
   geom_line(colour = "#002030", linewidth = 0.6) +
   scale_x_datetime(
      date_breaks = "6 days",
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
      plot.margin    = margin(6, 5.5, 5.5, 5.5)        # small top gap
   )

# ── Assemble ──────────────────────────────────────────────────────────────────
combined <- wind_panel / p1 / tide_panel +
   plot_layout(
      heights = c(0.18, 1, 0.15),
      guides  = "keep"
   ) &
   theme(plot.margin = margin(0.1, 5.5, 0.1, 5.5))

# Override margins on first and last to preserve title/x padding
combined[[1]] <- combined[[1]] + theme(plot.margin = margin(5.5, 5.5, 0, 5.5))
combined[[3]] <- combined[[3]] + theme(plot.margin = margin(0, 5.5, 5.5, 5.5))

ggsave("Outputs/Plots/SalinityInflows2016Plot.png",
       combined, dpi = 600, height = 10, width = 13)
ggsave("Outputs/Plots/SalinityInflows2016Plot.svg",
       combined, dpi = 600, height = 10, width = 13)
