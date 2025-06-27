# Function to create different weighting schemes for salinity
create_weight_schemes <- function(response_var, method = "quantile_progressive", time_var = NULL, data = NULL) {
   
   if(method == "quantile_progressive") {
      # Progressive weighting based on quantiles
      breaks <- quantile(response_var, probs = c(0, 0.5, 0.7, 0.85, 0.95, 1.0), na.rm = TRUE)
      weights <- cut(response_var, breaks, labels = c(1, 1.5, 3, 8, 20), include.lowest = TRUE) # Give serious weights to the high salinity
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
   
   # AR-SPECIFIC WEIGHTING METHODS
   
   if(method == "ar_event_sequence") {
      # Weight entire sequences of extreme events for AR models
      threshold_90 <- quantile(response_var, 0.90, na.rm = TRUE)
      base_weights <- ifelse(response_var > threshold_90, 8, 1)
      
      # Ensure data is ordered by time if time_var provided
      if(!is.null(time_var) && !is.null(data)) {
         order_idx <- order(data[[time_var]])
         response_ordered <- response_var[order_idx]
         base_weights_ordered <- base_weights[order_idx]
         
         # Extend high weights to neighboring observations
         weights_extended <- base_weights_ordered
         for(i in 2:length(weights_extended)) {
            # If previous observation was extreme, give current observation moderate weight
            if(base_weights_ordered[i-1] > 1) {
               weights_extended[i] <- max(weights_extended[i], 3)
            }
         }
         for(i in 1:(length(weights_extended)-1)) {
            # If next observation is extreme, give current observation moderate weight  
            if(base_weights_ordered[i+1] > 1) {
               weights_extended[i] <- max(weights_extended[i], 3)
            }
         }
         
         # Return weights in original order
         final_weights <- numeric(length(response_var))
         final_weights[order_idx] <- weights_extended
         return(final_weights)
      } else {
         warning("ar_event_sequence requires time_var and data arguments")
         return(base_weights)
      }
   }
   
   if(method == "ar_gradient") {
      # Gradient weighting that smoothly increases toward extremes
      # Better for AR models as it avoids sharp weight discontinuities
      threshold_50 <- quantile(response_var, 0.50, na.rm = TRUE)
      threshold_95 <- quantile(response_var, 0.95, na.rm = TRUE)
      
      # Normalize salinity relative to thresholds
      normalized_sal <- pmax(0, (response_var - threshold_50) / (threshold_95 - threshold_50))
      normalized_sal <- pmin(normalized_sal, 2)  # Cap at 2x threshold
      
      # Quadratic increase in weights
      weights <- 1 + normalized_sal^2 * 7  # Ranges from 1 to 8
      return(weights)
   }
   
   if(method == "ar_persistence") {
      # Weight based on persistence of extreme conditions
      # Higher weights for sustained extreme events
      threshold_85 <- quantile(response_var, 0.85, na.rm = TRUE)
      
      if(!is.null(time_var) && !is.null(data)) {
         order_idx <- order(data[[time_var]])
         response_ordered <- response_var[order_idx]
         
         # Calculate running count of consecutive extreme values
         extreme_flag <- response_ordered > threshold_85
         persistence_count <- numeric(length(extreme_flag))
         
         current_count <- 0
         for(i in 1:length(extreme_flag)) {
            if(extreme_flag[i]) {
               current_count <- current_count + 1
               persistence_count[i] <- current_count
            } else {
               current_count <- 0
               persistence_count[i] <- 0
            }
         }
         
         # Weight based on persistence: 1 + log(persistence + 1) * multiplier
         weights_ordered <- 1 + log(persistence_count + 1) * 2
         weights_ordered[!extreme_flag] <- 1  # Non-extreme always weight 1
         
         # Return weights in original order
         final_weights <- numeric(length(response_var))
         final_weights[order_idx] <- weights_ordered
         return(final_weights)
      } else {
         warning("ar_persistence requires time_var and data arguments")
         return(ifelse(response_var > threshold_85, 3, 1))
      }
   }
   
   if(method == "ar_buildup") {
      # Weight based on "buildup" to extreme events
      # Increases weights leading up to and during extreme events
      threshold_90 <- quantile(response_var, 0.90, na.rm = TRUE)
      
      if(!is.null(time_var) && !is.null(data)) {
         order_idx <- order(data[[time_var]])
         response_ordered <- response_var[order_idx]
         extreme_flag <- response_ordered > threshold_90
         
         weights_ordered <- rep(1, length(response_ordered))
         
         # Find extreme event periods
         for(i in 1:length(extreme_flag)) {
            if(extreme_flag[i]) {
               # High weight for extreme event itself
               weights_ordered[i] <- 10
               
               # Moderate weights leading up to event (3 time steps back)
               for(lag in 1:3) {
                  if(i - lag > 0) {
                     weights_ordered[i - lag] <- max(weights_ordered[i - lag], 4 - lag)
                  }
               }
               
               # Moderate weights following event (2 time steps forward)  
               for(lead in 1:2) {
                  if(i + lead <= length(weights_ordered)) {
                     weights_ordered[i + lead] <- max(weights_ordered[i + lead], 3 - lead)
                  }
               }
            }
         }
         
         # Return weights in original order
         final_weights <- numeric(length(response_var))
         final_weights[order_idx] <- weights_ordered
         return(final_weights)
      } else {
         warning("ar_buildup requires time_var and data arguments")
         return(ifelse(response_var > threshold_90, 10, 1))
      }
   }
   
   # Default fallback
   return(rep(1, length(response_var)))
}

