read_qs_files <- function(path) {
   if (!requireNamespace("qs2", quietly = TRUE)) stop("Package 'qs2' is required.")
   
   if (length(path) != 1) stop("Please provide a single file path or directory.")
   
   if (file.exists(path) && !dir.exists(path)) {
      # return the raw object directly (not wrapped in a list)
      return(qs2::qs_read(path))
   } else if (dir.exists(path)) {
      qs_files <- list.files(path, pattern = "\\.qs2?$", full.names = TRUE)
      obj_names <- tools::file_path_sans_ext(basename(qs_files))
      setNames(lapply(qs_files, qs2::qs_read), obj_names)
   } else {
      stop("Provided path does not exist as a file or directory.")
   }
}