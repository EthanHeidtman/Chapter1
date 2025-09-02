source("renv/activate.R")

if (requireNamespace("reticulate", quietly = TRUE)) {
   
   # 1. Detect the Poetry venv for this project
   poetry_env <- tryCatch(
      system("poetry env info --path", intern = TRUE),
      error = function(e) ""
   )
   
   if (nzchar(poetry_env) && dir.exists(poetry_env)) {
      
      # 2. Set RETICULATE_PYTHON
      Sys.setenv(RETICULATE_PYTHON = file.path(poetry_env, "bin", "python"))
      
      # 3. Force reticulate to use this interpreter, ignore any previous cache
      reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
      
      # 4. Optional: check critical modules
      if (!reticulate::py_module_available("numpy")) {
         warning("Warning: numpy not found in Poetry virtualenv. Run `poetry install` first.")
      }
      
      message("[reticulate] Using Python from: ", Sys.getenv("RETICULATE_PYTHON"))
      
   } else {
      warning("Poetry virtualenv not found. Run `poetry install` first.")
   }
   
   rm(poetry_env)
}
