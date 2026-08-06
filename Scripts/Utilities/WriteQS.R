write_qs_files <- function(
      obj_list,
      dir_path,
      file_names = NULL,
      format = c("qs2", "json", "csv"),
      compress_level = 3L
) {
   format <- match.arg(format)
   
   # Check required packages
   if (format == "qs2" && !requireNamespace("qs2", quietly = TRUE)) {
      stop("Package 'qs2' is required to write .qs2 files.")
   }
   if (format == "json" && !requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required to write .json files.")
   }
   
   if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
   stopifnot(is.list(obj_list))
   
   # Use names or validate file_names
   if (is.null(file_names)) {
      stopifnot(!is.null(names(obj_list)))
      file_names <- names(obj_list)
   } else {
      stopifnot(length(file_names) == length(obj_list))
      file_names <- sub("\\.(qs2|qs|json|csv)$", "", file_names)
   }
   
   file_names <- paste0(file_names, ".", format)
   paths <- file.path(dir_path, file_names)
   
   mapply(function(obj, path) {
      switch(format,
             qs2 = {
                qs2::qs_save(obj, file = path, compress_level = compress_level)
             },
             json = {
                jsonlite::write_json(obj, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
             },
             csv = {
                if (!is.data.frame(obj)) {
                   stop(sprintf("Object for path '%s' is not a data.frame and cannot be written to CSV.", path))
                }
                utils::write.csv(obj, file = path, row.names = FALSE)
             }
      )
   }, obj_list, paths, SIMPLIFY = FALSE)
   
   invisible(paths)
}