importance <- rf_hourly$importance_test

avg_importance <- importance %>%
   group_by(Variable) %>%
   summarise(
      Mean_IncMSE = mean(IncMSE_Test),
      SD_IncMSE = sd(IncMSE_Test),
      SE_IncMSE = SD_IncMSE / sqrt(n()),
      CI_lower = Mean_IncMSE - 1.96 * SE_IncMSE,
      CI_upper = Mean_IncMSE + 1.96 * SE_IncMSE,
      N_folds = n()
   ) %>%
   arrange(desc(Mean_IncMSE))

print(head(avg_importance, 20))

inflow_pattern <- "Inflows"  
discharge_pattern <- "Discharge"
wind_pattern <- "U|V|Gust"
tide_pattern <- "Tide"

n_candidates <- 5 

inflow_candidates <- avg_importance %>% 
   filter(grepl(inflow_pattern, Variable, ignore.case = TRUE)) %>%
   slice_max(Mean_IncMSE, n = n_candidates)

discharge_candidates <- avg_importance %>% 
   filter(grepl(discharge_pattern, Variable, ignore.case = TRUE)) %>%
   slice_max(Mean_IncMSE, n = n_candidates)

wind_candidates <- avg_importance %>% 
   filter(grepl(wind_pattern, Variable, ignore.case = TRUE)) %>%
   slice_max(Mean_IncMSE, n = n_candidates)

tide_candidates <- avg_importance %>%
   filter(grepl(tide_pattern, Variable, ignore.case = TRUE)) %>%
   slice_max(Mean_IncMSE, n = n_candidates)

all_candidates <- c(
   inflow_candidates$Variable, 
   discharge_candidates$Variable, 
   wind_candidates$Variable,
   tide_candidates$Variable
)

response_col <- 'Salinity'

data_subset <- model_data[, c(response_col, all_candidates)]

# Remove rows with NA in response
data_subset <- data_subset[!is.na(data_subset[[response_col]]), ]

for (col in all_candidates) {
   if (any(is.na(data_subset[[col]]))) {
      data_subset[[col]] <- ifelse(
         is.na(data_subset[[col]]),
         median(data_subset[[col]], na.rm = TRUE),
         data_subset[[col]]
      )
   }
}

cf_formula <- as.formula(paste(response_col, "~ ."))

cf <- cforest(
   formula = cf_formula, 
   data = data_subset,
   controls = cforest_unbiased(
      ntree = 500,  
      mtry = floor(sqrt(length(all_candidates)))
   )
)

cond_imp <- varimp(cf, conditional = TRUE)
std_imp <- varimp(cf, conditional = FALSE)

cond_imp_df <- data.frame(
   Variable = names(cond_imp),
   Cond_Importance = cond_imp,
   Std_Importance = std_imp[names(cond_imp)]
) %>% 
   arrange(desc(Cond_Importance))

final_inflow <- cond_imp_df %>% 
   filter(Variable %in% inflow_candidates$Variable) %>%
   slice_max(Cond_Importance, n = 1)

final_discharge <- cond_imp_df %>% 
   filter(Variable %in% discharge_candidates$Variable) %>%
   slice_max(Cond_Importance, n = 1)

final_wind <- cond_imp_df %>% 
   filter(Variable %in% wind_candidates$Variable) %>%
   slice_max(Cond_Importance, n = 1)

final_tide <- cond_imp_df %>%
   filter(Variable %in% tide_candidates$Variable) %>%
   slice_max(Cond_Importance, n = 1)

# Combine final selections
final_vars <- c(
   final_inflow$Variable, 
   final_discharge$Variable, 
   final_wind$Variable,
   final_tide$Variable
)



