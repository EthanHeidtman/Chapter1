run_reticulate_setup <- function() {
   if (requireNamespace("reticulate", quietly = TRUE)) {
      poetry_env <- tryCatch(
         system("poetry env info --path", intern = TRUE),
         error = function(e) NULL
      )
      if (!is.null(poetry_env) && dir.exists(poetry_env)) {
         reticulate::use_python(file.path(poetry_env, "bin", "python"), required = TRUE)
      } else {
         message("Poetry environment not found or invalid.")
      }
   }
}

run_reticulate_setup()
rm(run_reticulate_setup)

# Activate renv for R package management
source("renv/activate.R")

