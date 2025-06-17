# Function to create different weighting schemes for salinity

create_extreme_weights <- function(response_var, method = "quantile_progressive") {
   
   if(method == "quantile_progressive") {
      # Progressive weighting based on quantiles
      breaks <- quantile(response_var, probs = c(0, 0.5, 0.7, 0.85, 0.95, 1.0), na.rm = TRUE)
      weights <- cut(response_var, breaks, labels = c(1, 1.5, 3, 8, 20), include.lowest = TRUE)
      return(as.numeric(as.character(weights)))
   }
   
   if(method == "exponential") {
      # Exponential weighting for extreme values
      threshold_95 <- quantile(response_var, 0.95, na.rm = TRUE)
      weights <- ifelse(response_var > threshold_95, 
                        exp((response_var - threshold_95) / threshold_95 * 2), 
                        1)
      return(weights)
   }
   
   if(method == "binary_extreme") {
      # Heavy weighting for extreme events only
      threshold_90 <- quantile(response_var, 0.90, na.rm = TRUE)
      weights <- ifelse(response_var > threshold_90, 10, 1)
      return(weights)
   }
   
   return(rep(1, length(response_vary)))
}
