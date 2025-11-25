

plot_data <- model_data %>%
   mutate(LogInflows = log(Inflows)) %>%
   # mutate(RollingInflows = zoo::rollmean(LogInflows, 24 * 30, fill = NA, align = "right", na.rm = TRUE)) %>%
   relocate(LogInflows, .after = Inflows) 

highlight_start <- as.Date("2016-10-05")
highlight_end   <- as.Date("2016-10-25")

sal_range  <- range(plot_data$Salinity,   na.rm = TRUE)
flow_range <- range(plot_data$LogInflows, na.rm = TRUE)
scale_fac  <- diff(sal_range) / diff(flow_range)

ggplot(plot_data %>% filter(Year == 2016), aes(x = DateTime)) + 
   #--- Yellow highlight background ---
   annotate("rect",
            xmin = highlight_start, xmax = highlight_end,
            ymin = -Inf, ymax = Inf,
            fill = "yellow", alpha = 0.2) +
   
   # EPA Total Dissolved Solids
   geom_hline(yintercept = 0.5, color = 'black', linetype = 2) + 
   
   # --- Salinity line (left axis) ---
   geom_line(aes(y = Salinity), colour = "red", size = 1) +
   
   # --- Raw LogInflows (rescaled to left axis) ---
   geom_line(aes(y = LogInflows * scale_fac + sal_range[1] -
                    flow_range[1] * scale_fac),
             colour = "blue", alpha = 0.4, na.rm = TRUE) +
   
   # --- LOESS smooth for LogInflows (also rescaled) ---
   geom_smooth(aes(y = LogInflows * scale_fac + sal_range[1] -
                      flow_range[1] * scale_fac),
               method = "loess", span = 0.5,
               se = FALSE, colour = "blue", size = 1, na.rm = TRUE) +
   
   # --- Axes ---
   scale_x_date(date_breaks = "1 month", date_labels = "%b",
                limits = c(as_date('2016-09-15'), as_date('2016-10-31'))) +
   scale_y_continuous(
      name = "Salinity (psu)",
      sec.axis = sec_axis(
         trans = ~ (. - sal_range[1] + flow_range[1] * scale_fac) / scale_fac,
         name  = "Log(Inflows) (m3/sec)"
      )
   ) +
   
   # --- Theming ---
   theme_bw() +
   labs(
      title = "Havre de Grace Salinity and Conowingo Reservoir Inflows: 2016",
      x = "Date"
   ) +
   theme(
      plot.title = element_text(size = 16, face = 'bold'),
      axis.title.x = element_text(size = 14, face = 'bold'),
      axis.title.y.left  = element_text(size = 14, face = 'bold', colour = "red"),
      axis.title.y.right = element_text(size = 14, face = 'bold', colour = "blue"),
      axis.text.y.left   = element_text(colour = "red"),
      axis.text.y.right  = element_text(colour = "blue"),
      axis.text.x        = element_text(size = 12)
   )
