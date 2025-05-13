# Define a function to normalize a vector and return the parameters so we can transform back after


normalize_with_parameters <- function(x) {
   mean <- mean(x, na.rm = TRUE)
   sd <- sd(x, na.rm = TRUE)
   norm <- (x - mean) / sd
   return(list(normalized = norm, mean = mean, sd = sd))
}