# Define shift function
shift_predictors_by_k <- function(data, k) {
   
   # Columns to exclude from shifting
   exclude_cols <- c("DateTime", "Salinity", 'FERC', 'Year', 'Month', 'Day', 'DayOfYear', 'Date')
   
   # Columns to remove entirely (Inflows-related)
   remove_cols <- grep("^Inflows", names(data), value = TRUE)
   
   # Get predictor columns (everything except excluded and removed)
   predictor_cols <- setdiff(
      names(data), 
      c(exclude_cols, remove_cols)
   )
   
   # Shift predictors and create new dataset
   shifted_data <- data %>%
      select(-all_of(remove_cols)) %>%  # Remove Inflows columns
      mutate(
         across(
            all_of(predictor_cols),
            ~lag(.x, k),
            .names = "{.col}_{k}"  # Dynamic naming based on k
         )
      ) %>%
      select(-all_of(predictor_cols))  # Remove original unshifted columns
   
   return(shifted_data)
}
