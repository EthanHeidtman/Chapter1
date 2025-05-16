# Define a function to normalize a vector and return the parameters so we can transform back after


normalize_with_parameters <- function(x) {
   mean <- mean(x, na.rm = TRUE)
   sd <- sd(x, na.rm = TRUE)
   norm <- (x - mean) / sd
   return(list(normalized = norm, mean = mean, sd = sd))
}

# Function to normalize multiple predictors and add them to model_data using dplyr
normalize_multiple_predictors <- function(data, predictors) {
   # Create a list to store normalization parameters
   norm_params <- list()
   
   # Start with the original data
   result_data <- data
   
   # Loop through each predictor
   for (predictor in predictors) {
      # Apply your existing normalize_with_parameters function
      norm_result <- normalize_with_parameters(data[[predictor]])
      
      # Store the parameters
      norm_params[[predictor]] <- list(mean = norm_result$mean, sd = norm_result$sd)
      
      # Add the normalized column to the data using dplyr
      result_data <- result_data %>%
         dplyr::mutate(!!paste0("Norm_", predictor) := norm_result$normalized)
   }
   
   # Return both the updated data and parameters
   return(list(
      data = result_data,
      parameters = norm_params
   ))
}