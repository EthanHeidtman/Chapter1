setHook("rstudio.sessionInit", function(...) {
   run_reticulate_setup <- function() {
      if (!requireNamespace("reticulate", quietly = TRUE)) return()
      
      if (!file.exists("pyproject.toml")) {
         message("No pyproject.toml found — skipping Poetry setup.")
         return()
      }
      
      poetry_env <- tryCatch({
         poetry_path <- Sys.which("poetry")
         if (!nzchar(poetry_path)) {
            message("Poetry not found in system PATH.")
            return(NULL)
         }
         
         env_path <- system("poetry env info --path", intern = TRUE)
         if (length(env_path) != 1 || is.na(env_path) || !dir.exists(env_path)) {
            message("Poetry environment path is invalid.")
            return(NULL)
         }
         
         env_path
      }, error = function(e) {
         message("Error getting Poetry env path: ", conditionMessage(e))
         NULL
      })
      
      if (!is.null(poetry_env)) {
         python_bin <- file.path(poetry_env, "bin", "python")
         if (file.exists(python_bin)) {
            reticulate::use_python(python_bin, required = TRUE)
            message("Using Python from Poetry env: ", python_bin)
         } else {
            message("Python binary not found in Poetry env path.")
         }
      }
   }
   
   run_reticulate_setup()
}, action = "append")

# Activate renv for R package management
source("renv/activate.R")

rm(poetry_env, run_reticulate_setup)
