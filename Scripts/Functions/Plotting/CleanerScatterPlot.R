# Function to create cleaner scatter plots for messy data
create_cleaner_scatter <- function(data, pred1 = "Norm_RollingPowDischarge14", pred2 = "Norm_StressHours_30day_Marietta") {
   
   # Remove incomplete cases
   clean_data <- data[complete.cases(data[, c(pred1, pred2, "Salinity")]), ]
   
   # 1. Hexbin plot to handle overplotting
   p1 <- ggplot(clean_data, aes_string(x = pred1, y = pred2)) +
      geom_hex(bins = 50) +
      scale_fill_viridis_c(name = "Count") +
      labs(title = "Hexbin Plot: Density of Data Points",
           subtitle = "Cleaner view of data distribution",
           x = gsub("_", " ", pred1),
           y = gsub("_", " ", pred2)) +
      theme_minimal()
   
   # 2. Sample data for cleaner scatter
   if (nrow(clean_data) > 5000) {
      sample_data <- clean_data[sample(nrow(clean_data), 5000), ]
   } else {
      sample_data <- clean_data
   }
   
   p2 <- ggplot(sample_data, aes_string(x = pred1, y = pred2, color = "Salinity")) +
      geom_point(alpha = 0.6, size = 0.8) +
      scale_color_viridis_c(name = "Salinity", trans = "sqrt") +
      labs(title = "Sampled Data: Predictor Relationship by Salinity",
           subtitle = paste("n =", nrow(sample_data), "points"),
           x = gsub("_", " ", pred1),
           y = gsub("_", " ", pred2)) +
      theme_minimal()
   
   # 3. Focus on extreme events
   high_sal_threshold <- quantile(clean_data$Salinity, 0.95, na.rm = TRUE)
   extreme_data <- clean_data[clean_data$Salinity >= high_sal_threshold, ]
   
   p3 <- ggplot(clean_data, aes_string(x = pred1, y = pred2)) +
      geom_point(alpha = 0.1, color = "gray80", size = 0.5) +  # Background points
      geom_point(data = extreme_data, aes(color = Salinity), size = 2, alpha = 0.8) +
      scale_color_viridis_c(name = "Salinity") +
      labs(title = "Extreme Salinity Events Highlighted",
           subtitle = paste("Showing top 5% salinity events (≥", round(high_sal_threshold, 3), ")"),
           x = gsub("_", " ", pred1),
           y = gsub("_", " ", pred2)) +
      theme_minimal()
   
   # 4. Find and highlight October 2016 specifically
   if ("DateTime" %in% names(data) || "Date" %in% names(data)) {
      time_col <- ifelse("DateTime" %in% names(data), "DateTime", "Date")
      
      # Try to identify October 2016
      oct2016_data <- clean_data[format(clean_data[[time_col]], "%Y-%m") == "2016-10", ]
      
      if (nrow(oct2016_data) > 0) {
         p4 <- ggplot(clean_data, aes_string(x = pred1, y = pred2)) +
            geom_point(alpha = 0.1, color = "gray80", size = 0.5) +  # Background
            geom_point(data = oct2016_data, aes(color = Salinity), size = 3, alpha = 0.9) +
            scale_color_gradient2(low = "blue", mid = "yellow", high = "red", 
                                  midpoint = median(oct2016_data$Salinity, na.rm = TRUE),
                                  name = "Salinity") +
            labs(title = "October 2016 Event Highlighted",
                 subtitle = paste("Max salinity in Oct 2016:", round(max(oct2016_data$Salinity, na.rm = TRUE), 3)),
                 x = gsub("_", " ", pred1),
                 y = gsub("_", " ", pred2)) +
            theme_minimal()
         
         # Print some stats about October 2016
         cat("OCTOBER 2016 EVENT ANALYSIS\n")
         cat("===========================\n")
         cat(sprintf("Number of observations: %d\n", nrow(oct2016_data)))
         cat(sprintf("Salinity range: %.3f to %.3f\n", 
                     min(oct2016_data$Salinity, na.rm = TRUE),
                     max(oct2016_data$Salinity, na.rm = TRUE)))
         cat(sprintf("%s range: %.3f to %.3f\n", pred1,
                     min(oct2016_data[[pred1]], na.rm = TRUE),
                     max(oct2016_data[[pred1]], na.rm = TRUE)))
         cat(sprintf("%s range: %.3f to %.3f\n", pred2,
                     min(oct2016_data[[pred2]], na.rm = TRUE),
                     max(oct2016_data[[pred2]], na.rm = TRUE)))
      } else {
         p4 <- NULL
         cat("Could not find October 2016 data\n")
      }
   } else {
      p4 <- NULL
      cat("No date column found for October 2016 analysis\n")
   }
   
   # 5. Contour plot to show salinity surface
   p5 <- ggplot(clean_data, aes_string(x = pred1, y = pred2, z = "Salinity")) +
      geom_contour_filled(bins = 15, alpha = 0.7) +
      scale_fill_viridis_d(name = "Salinity") +
      labs(title = "Salinity Contour Surface",
           subtitle = "Shows how salinity varies across predictor space",
           x = gsub("_", " ", pred1),
           y = gsub("_", " ", pred2)) +
      theme_minimal()
   
   return(list(
      hexbin = p1,
      sampled_scatter = p2,
      extreme_events = p3,
      october_2016 = p4,
      contour_surface = p5
   ))
}

