# Define a function to normalize a vector


normalize <- function(x) {
   (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}