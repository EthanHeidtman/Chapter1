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


