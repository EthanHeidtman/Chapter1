make_expanding_folds <- function(data, date_col = "DateTime", start_month_cutoff = 5) {
   library(lubridate)
   
   data[[date_col]] <- as.POSIXct(data[[date_col]])
   data$year <- year(data[[date_col]])
   data$month <- month(data[[date_col]])
   
   years <- sort(unique(data$year))
   folds <- list()
   
   # Check if first year starts after cutoff month
   first_year <- years[1]
   first_year_min_month <- min(data$month[data$year == first_year], na.rm = TRUE)
   
   if (first_year_min_month > start_month_cutoff) {
      # Combine first 3 years into one initial fold
      initial_train_years <- years[1:3]
      test_year <- years[4]
      start_idx <- which(data$year %in% initial_train_years)
      test_idx <- which(data$year == test_year)
      folds[[1]] <- list(
         train = start_idx,
         test = test_idx,
         train_years = initial_train_years,
         test_years = test_year
      )
      start_fold <- 4
   } else {
      # Normal start (first year complete)
      initial_train_years <- years[1:2]
      test_year <- years[3]
      start_idx <- which(data$year %in% initial_train_years)
      test_idx <- which(data$year == test_year)
      folds[[1]] <- list(
         train = start_idx,
         test = test_idx,
         train_years = initial_train_years,
         test_years = test_year
      )
      start_fold <- 3
   }
   
   # Continue expanding folds for remaining years
   for (i in start_fold:(length(years) - 1)) {
      train_years <- years[1:i]
      test_year <- years[i + 1]
      
      folds[[length(folds) + 1]] <- list(
         train = which(data$year %in% train_years),
         test = which(data$year == test_year),
         train_years = train_years,
         test_years = test_year
      )
   }
   
   return(folds)
}
