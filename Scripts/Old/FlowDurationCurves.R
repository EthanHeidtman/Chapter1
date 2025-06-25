# Making a Flow Duration Curve
CON_fdc <- daily %>%
   dplyr::select(1, 2, 3, 4, 7) %>%
   filter(Date > '1996-12-31') %>%
   mutate(Rank = rank(-Conowingo),
          ExceedProb = 100 * (Rank / (length(Conowingo) + 1)))

MAR_fdc <- daily %>%
   dplyr::select(1, 2, 3, 4, 8) %>%
   #filter(Date > '1996-12-31') %>%
   mutate(Rank = rank(-Marietta),
          ExceedProb = 100 * (Rank / (length(Marietta) + 1)))

fdc <- MAR_fdc[, 1 : 4]
fdc <- merge(fdc, MAR_fdc, by = c('Date', 'Year', 'Month', 'Day'), all = TRUE)
#colnames(fdc) <- c('Date', 'Year', 'Month', 'Day', 'Marietta_Q', 'Marietta_Rank', 'Marietta_Prob')
fdc <- merge(fdc, CON_fdc,  by = c('Date', 'Year', 'Month', 'Day'), all = TRUE)
colnames(fdc) <- c('Date', 'Year', 'Month', 'Day', 'Marietta_Q', 'Marietta_Rank', 'Marietta_Prob', 'Conowingo_Q', 'Conowingo_Rank', 'Conowingo_Prob')

ggplot(fdc) + 
   geom_line(aes(x = Marietta_Prob, y = Marietta_Q, color = 'Marietta'), na.rm = TRUE) + 
   geom_line(aes(x = Conowingo_Prob, y = Conowingo_Q, color = 'Conowingo'), na.rm = TRUE) + 
   scale_y_log10() + 
   labs(x = 'Exceedance Probability', y = 'Discharge (cubic m/sec)', title = 'Susquehanna Discharge: Flow Duration Curves') + 
   theme_bw() + 
   scale_color_manual(name = 'USGS Station', values = c('Marietta' = 'red',
                                                        'Conowingo' = 'blue'))