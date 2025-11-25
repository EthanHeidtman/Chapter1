make_expanding_folds <- function(
      data,
      date_col = "DateTime",
      start_month_cutoff = 5,
      initial_train_length = 6 
) {
   library(lubridate)
   
   data[[date_col]] <- as.POSIXct(data[[date_col]])
   data$year <- year(data[[date_col]])
   data$month <- month(data[[date_col]])
   
   years <- sort(unique(data$year))
   folds <- list()
   
   # Determine whether the first year starts after cutoff
   first_year <- years[1]
   first_year_min_month <- min(data$month[data$year == first_year], na.rm = TRUE)
   
   # If the first year starts after cutoff, allow an extra year in the initial training
   # but do it generically (initial_train_length vs initial_train_length - 1)
   if (first_year_min_month > start_month_cutoff) {
      # e.g., if initial_train_length = 3 → use 3 years instead of 2
      train_n <- initial_train_length
   } else {
      # use one fewer year (first-year complete case)
      train_n <- initial_train_length - 1
      if (train_n < 1) stop("initial_train_length too small for this data and cutoff logic.")
   }
   
   # Define initial fold
   initial_train_years <- years[1:train_n]
   test_year <- years[train_n + 1]
   
   folds[[1]] <- list(
      train = which(data$year %in% initial_train_years),
      test = which(data$year == test_year),
      train_years = initial_train_years,
      test_years = test_year
   )
   
   # Track next fold start index
   start_fold <- train_n + 1
   
   # Build expanding folds
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

