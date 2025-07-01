write_qs_files <- function(obj_list, dir_path, file_names = NULL, preset = "high") {
   if (!requireNamespace("qs", quietly = TRUE)) stop("Package 'qs' is required.")
   if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
   
   stopifnot(is.list(obj_list))
   
   # Use user-supplied file names or fallback to object names
   if (is.null(file_names)) {
      stopifnot(!is.null(names(obj_list)))
      file_names <- paste0(names(obj_list), ".qs")
   } else {
      stopifnot(length(file_names) == length(obj_list))
      file_names <- sub("\\.qs$", "", file_names)  # remove trailing .qs if present
      file_names <- paste0(file_names, ".qs")      # ensure extension
   }
   
   paths <- file.path(dir_path, file_names)
   
   mapply(function(obj, path) {
      qs::qsave(obj, file = path, preset = preset)
   }, obj_list, paths, SIMPLIFY = FALSE)
   
   invisible(paths)
}
