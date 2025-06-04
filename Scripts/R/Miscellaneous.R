
### TESTING DIFFERENT DISCHARGE LAGS ###
### Which lag transformation performs the best?
### using the power law transformation (Q ^ -0.4)

## Model 1a: 1-hr lag
model1a <- lm(Salinity ~ Norm_PowLagDischarge1 + Norm_Tide, data = model_data)

## Model 1b: 3-hr lag
model1b <- lm(Salinity ~ Norm_PowLagDischarge3 + Norm_Tide, data = model_data)

## Model 1c: 6-hr lag
model1c <- lm(Salinity ~ Norm_PowLagDischarge6 + Norm_Tide, data = model_data)

## Model 1d: 10-hr lag
model1d <- lm(Salinity ~ Norm_PowLagDischarge10 + Norm_Tide, data = model_data)

## Model 1e: 12-hr lag BEST PERFORMER
model1e <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_Tide, data = model_data)

## Model 1f: 36-hr lag
model1f <- lm(Salinity ~ Norm_PowLagDischarge36 + Norm_Tide, data = model_data)

## Model 1g: 48-hr lag
model1g <- lm(Salinity ~ Norm_PowLagDischarge48 + Norm_Tide, data = model_data)

## Model 1h: 72-hr lag
model1h <- lm(Salinity ~ Norm_PowLagDischarge72 + Norm_Tide, data = model_data)

models <- list(model1a, model1b, model1c, model1d, model1e, model1f, model1g, model1h)
model_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('1hr', '3hr', '6hr', '10hr', '12hr', '36hr', '48hr', '72hr')
Models1EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names, Group = 1)

### TESTING DIFFERENT DISCHARGE ROLLING AVERAGES ###
### Which rolling average performs best? Again, using the -0.4 transformation

## Model 2a: 0.5-day rolling average 
model2a <- lm(Salinity ~ Norm_RollingPowDischarge0.5 + Norm_Tide, data = model_data)

## Model 2b: 1-day rolling average 
model2b <- lm(Salinity ~ Norm_RollingPowDischarge1 + Norm_Tide, data = model_data)

## Model 2c: 2-day rolling average 
model2c <- lm(Salinity ~ Norm_RollingPowDischarge2 + Norm_Tide, data = model_data)

## Model 2d: 4-day rolling average 
model2d <- lm(Salinity ~ Norm_RollingPowDischarge4 + Norm_Tide, data = model_data)

## Model 2e: 7-day rolling average 
model2e <- lm(Salinity ~ Norm_RollingPowDischarge7 + Norm_Tide, data = model_data)

## Model 2f: 10-day rolling average BEST PERFORMER
model2f <- lm(Salinity ~ Norm_RollingPowDischarge10 + Norm_Tide, data = model_data)

## Model 2g: 14-day rolling average 
model2g <- lm(Salinity ~ Norm_RollingPowDischarge14 + Norm_Tide, data = model_data)

models <- list(model2a, model2b, model2c, model2d, model2e, model2f, model2g)
models_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('0.5day', '1day', '2day', '4day', '7day', '10day', '14day')
Models2EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names, Group = 2)


### COMBINED DISCHARGE MODELS ###
### What are the best combinations of discharge predictors?

## Model 3a: Best Raw Discharge Transformation + Best Lag
model3a <- lm(Salinity ~ Norm_PowDischarge + Norm_PowLagDischarge12 + Norm_Tide,
              data = model_data)

## Model 3b: Best raw discharge transformation + best rolling average
model3b <- lm(Salinity ~ Norm_PowDischarge + Norm_RollingPowDischarge10 + 
                 Norm_Tide, data = model_data)

## Model 3c: Best lag + best rolling average BEST PERFORMER
model3c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + Norm_Tide,
              data = model_data)                                                            

## Model 3d: Best raw discharge + best lag + best rolling average
model3d <- lm(Salinity ~ Norm_PowDischarge + Norm_PowLagDischarge12 + 
                 Norm_RollingPowDischarge10 + Norm_Tide, data = model_data)

models <- list(model3a, model3b, model3c, model3d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('Raw+Lag', 'Raw+Rolling', 'Lag+Rolling', 'Raw+Lag+Rolling')
Models3EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names, Group = 3)


### TESTING VALUE OF LAGGED MARIETTA INFLOWS ###
### What lagged inflow time series is the best?
### Building from the best combined discharge model (model3c)

## Model 4a: 12-hr lag of inflows
model4a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_LagInflows12, data = model_data)

## Model 4b: 24-hr lag of inflows
model4b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_LagInflows24, data = model_data)

## Model 4c: 48-hr lag of inflows BEST PERFORMER
model4c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_LagInflows48, data = model_data)

## Model 4d: 72-hr lag of inflows
model4d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_LagInflows72, data = model_data)

models <- list(model4a, model4b, model4c, model4d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('12hr', '25hr', '48hr', '72hr')
Models4EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names, Group = 4)


### TESTING VALUE OF ROLLING AVERAGE INFLOWS ###
### What is the best performing rolling average inflows?
### Building from the best combined discharge modell (model3c)

## Model 5a: 1-day rolling average of inflows
model5a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_RollingPowInflows1, data = model_data)

## Model 5b: 2-day rolling average of inflows BEST PERFORMER
model5b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_RollingPowInflows2, data = model_data)

## Model 5c: 7-day rolling average of inflows
model5c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_RollingPowInflows7, data = model_data)

## Model 5d: 10-day rolling average of inflows
model5d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 +
                 Norm_Tide + Norm_RollingPowInflows10, data = model_data)

models <- list(model5a, model5b, model5c, model5d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('1day', '2day', '7day', '10day')
Models5EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names, Group = 5)

### TESTING BASIC STRESS (AND SEASON) CLASSIFICATION ###
### What combinations of stress improve performance? and then what does adding seasons do?
### Adding to our best model so far, model5b

## Model 6a: Add moderate stress
model6a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsModerateStress, data = model_data) 

## Model6b: Add high stress BEST PERFORMER FOR STRESSES
model6b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress, data = model_data)    

## Model6c: Add both stresses
model6c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsModerateStress + IsHighStress, data = model_data)  

## Model 6d: Adding seasons to the best model (6b) BEST OVERALL PERFORMER FROM MODELS 6
model6d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason, data = model_data)    

models <- list(model6a, model6b, model6c, model6d)
models_types <- c('linear', 'linear', 'linear', 'linear')
model_names <- c('Moderate', 'High', 'Moderate+High', 'Best+Season')
Models6EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names, Group = 6)

### TESTING INTERACTIONS ###
### 

## Model 7a: Discharge and Tide
model7a <- lm(Salinity ~ Norm_PowLagDischarge12 * Norm_Tide + Norm_RollingPowDischarge10 * Norm_Tide + 
                 Norm_RollingPowInflows2 + IsHighStress + SalinitySeason, data = model_data)    

## Model 7b: Inflows and Tide
model7b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_RollingPowInflows2 * Norm_Tide + IsHighStress + SalinitySeason, data = model_data)   

## Model 7c: Discharge and High Stress BEST PERFORMER
model7c <- lm(Salinity ~ Norm_PowLagDischarge12 * IsHighStress + Norm_RollingPowDischarge10 * IsHighStress + 
                 Norm_Tide + Norm_RollingPowInflows2 + SalinitySeason, data = model_data) 

## Model 7d: Inflows and High Stress
model7d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 * IsHighStress + SalinitySeason, data = model_data)   

## Model 7e: Discharge and Salinity Season
model7e <- lm(Salinity ~ Norm_PowLagDischarge12 * SalinitySeason + Norm_RollingPowDischarge10 * SalinitySeason + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress, data = model_data)    

## Model 7f: Inflows and Salinity Season
model7f <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 * SalinitySeason + IsHighStress, data = model_data)   

## Model7g: Tide and Salinity Season
model7g <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide * SalinitySeason + Norm_RollingPowInflows2 + IsHighStress, data = model_data)   

models <- list(model7a, model7b, model7c, model7d, model7e, model7f, model7g)
model_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('DischargeTide', 'InflowsTide', 'DischargeStress', 'InflowsStress', 'DischargeSeason', 'InflowsSeason', 'TideSeason')
Models7EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names, Group = 7)


### What additional stress metrics improve the fit?
### Adding to best model from previous, model6d

## Model 7a: Consecutive Stress Hours @ Marietta (Inflows)
model7a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_ConsecutiveStressHours_Marietta, data = model_data)   

## Model 7b: Consecutive Stress Hours @ Conowingo (Discharge)
model7b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_ConsecutiveStressHours_Conowingo, data = model_data)   

## Model 7c: # of Stress Hours in the last 7 days @ Marietta (Inflows)
model7c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_StressHours_7day_Marietta, data = model_data)   

## Model 7d: # of Stress Hours in the last 14 days @ Marietta (Inflows)
model7d <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_StressHours_14day_Marietta, data = model_data)   

## Model 7e: # of Stress Hours in the last 30 days @ Marietta (Inflows) BEST PERFORMER
model7e <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_StressHours_30day_Marietta, data = model_data)   

## Model 7f: Cumulative Stress in the last 7 days @ Marietta (Inflows)
model7f <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_CumulativeStress_7day_Marietta, data = model_data)   

## Model 7g: Cumulative Stress in the last 14 days @ Marietta (Inflows)
model7g <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_CumulativeStress_14day_Marietta, data = model_data)   

## Model 7h: Cumulative Stress in the last 30 days @ Marietta (Inflows)
model7h <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + 
                 IsHighStress + SalinitySeason + Norm_CumulativeStress_30day_Marietta, data = model_data)  

models <- list(model7a, model7b, model7c, model7d, model7e, model7f, model7g, model7h)
model_types <- c('linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear', 'linear')
model_names <- c('ConsecMarietta', 'ConsecConowingo', 'MarStress7', 'MarStress14', 
                 'MarStress30', 'MarCumStress7', 'MarCumStress14', 'MarCumStress30')
Models7EVAL <- compare_models(models, model_types, data = model_data, threshold = salinity_threshold, model_names)

### LATENT FLOW INTEGRATION ###
### How do the latent flow classifications improve the model?

## Model 8a: Adding simple latent flow formulation
model8a <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason + 
                 Norm_StressHours_30day_Marietta + Norm_SimpleLatent, data = model_data)   

## Model 8b: Adding stress-dependent latent flow formulation
model8b <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason + 
                 Norm_StressHours_30day_Marietta + Norm_StressLatent, data = model_data) 

## Model 8c: Adding best latent flow formulation
model8c <- lm(Salinity ~ Norm_PowLagDischarge12 + Norm_RollingPowDischarge10 + 
                 Norm_Tide + Norm_RollingPowInflows2 + IsHighStress + SalinitySeason + 
                 Norm_StressHours_30day_Marietta + Norm_BestLatent, data = model_data) 

models <- list(model8a, model8b, model8c)
models_types <- c('linear', 'linear', 'linear')
model_names <- c('SimpleLatent', 'StressLatent', 'BestLatent')
Models8EVAL <- compare_models(models, models_types, data = model_data, salinity_threshold, model_names)




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




# daily: 1.85% more
# monthly: 0.8% less




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

# Susquehanna Morphological Characteristics
d = 9.9 * 1.609 * 1000                   # dam's distance from the mouth in meters (~9.9 miles)
depth = 6                                # average depth of the river from the dam to the mouth in meters
width = 1600                             # average width of the river from the dam to the mouth in meters
area = depth * width                     # average cross-sectional area of the river below the dam  (m^2)


#### Formulate Predictive Relationship as Shortage Objective Function to Minimize ####



##################### Comparison to Old FERC Requirement #######################

