# Function to read and combine all yearly text files in a directory
combine_txt_files <- function(dir_path) {
   files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)
   files <- sort(files)
   
   first_two <- readLines(files[1], n = 2, warn = FALSE)
   header_line <- first_two[1]
   col_names <- strsplit(trimws(gsub("^#", "", header_line)), "\\s+")[[1]]
   
   data_list <- lapply(seq_along(files), function(i) {
      # Read the actual data, skipping the two header lines
      df <- read.table(files[i], header = FALSE, skip = 2, fill = TRUE, 
                       stringsAsFactors = FALSE, comment.char = "")
      names(df) <- col_names
      df
   })
   
   combined <- dplyr::bind_rows(data_list)
   return(combined)
}