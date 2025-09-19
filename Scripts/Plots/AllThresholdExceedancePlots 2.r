all_threshold_exceedance_probs <- function(data) {
   plot <- ggplot(final_data) + 
      geom_line(aes(x = Date, y = exceedance_probability,
                    color = factor(threshold)), na.rm = TRUE, size = 0.7) +
      geom_line(aes(x = Date, y = Salinity / max(Salinity, na.rm = TRUE),
                    color = "Observed Salinity"),
                linetype = 1, size = 1.5) +
      scale_y_continuous(
         name = "Exceedance Probability",
         limits = c(0, 1),
         sec.axis = sec_axis(~ . * max(final_data$Salinity, na.rm = TRUE),
                             name = "Salinity (psu)")
      ) +
      scale_color_manual(
         name = "Threshold (psu)",
         values = c(
            "Observed Salinity" = "red",
            setNames(
               viridis(length(unique(final_data$threshold))),
               as.character(sort(unique(final_data$threshold)))
            )
         )
      ) +
      labs(
         x = "Date",
         y = "Value",
         title = "Predicted Exceedance Probability and Observed Salinity by Threshold"
      ) +
      theme_bw() +
      theme(legend.title = element_text(face = "bold", size = 14),
            legend.position = 'bottom',
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = 'bold'),
            axis.text = element_text(size = 12), 
            plot.title = element_text(size = 16, face = 'bold'),
            strip.text = element_text(size = 14)) + 
      guides(color = guide_legend(
         override.aes = list(
            size = c(rep(1.5, length(unique(final_data$threshold))), 2),
            linetype = c(rep(1, length(unique(final_data$threshold))), 1)
         )
      )) + 
      facet_wrap(~Year, scales = 'free_x') + 
      scale_x_date(date_labels = '%b',
                   date_breaks = '2 months')
   
   return(plot) 
   
}

all_threshold_window_plot <- function(data) {
   plot <- ggplot(final_data) + 
         # exceedance probabilities for each threshold
         geom_line(aes(x = Date, y = exceedance_probability,
                       color = factor(threshold)), na.rm = TRUE, size = 0.7) +
         # observed salinity
         geom_line(aes(x = Date, y = Salinity / max(Salinity, na.rm = TRUE),
                       color = "Observed Salinity"),
                   linetype = 1, size = 1.5) +
         scale_y_continuous(
            name = "Exceedance Probability",
            limits = c(0, 1),
            sec.axis = sec_axis(~ . * max(final_data$Salinity, na.rm = TRUE),
                                name = "Salinity (psu)")
         ) +
         scale_x_date(date_labels = '%b %Y',
                      date_breaks = '2 months',
                      limits = as.Date(c("2016-04-06", "2016-11-23"))) +
         scale_color_manual(
            name = "Threshold (psu)",
            values = c(
               "Observed Salinity" = "red",
               setNames(
                  viridis(length(unique(final_data$threshold))),
                  as.character(sort(unique(final_data$threshold)))
               )
            )
         ) +
         labs(
            x = "Date",
            y = "Value",
            title = "Predicted Exceedance Probability and Observed Salinity by Threshold"
         ) +
         theme_bw() +
         theme(legend.title = element_text(face = "bold", size = 14),
               legend.position = 'bottom',
               legend.text = element_text(size = 12),
               axis.title = element_text(size = 14, face = 'bold'),
               axis.text = element_text(size = 12), 
               plot.title = element_text(size = 16, face = 'bold'),
               strip.text = element_text(size = 14)) + 
         guides(color = guide_legend(
            override.aes = list(
               size = c(rep(1.5, length(unique(final_data$threshold))), 2),
               linetype = c(rep(1, length(unique(final_data$threshold))), 1)
            )
         )) 
   
   return(plot) 
}
   
   
   