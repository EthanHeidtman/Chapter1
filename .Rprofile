# Activate renv for R dependencies
source("renv/activate.R")

# Point reticulate to the Poetry virtual environment for this project
poetry_env <- system("poetry env info --path", intern = TRUE)
Sys.setenv(RETICULATE_PYTHON = file.path(poetry_env, "bin", "python"))

# Use this Python for reticulate
if (requireNamespace("reticulate", quietly = TRUE)) {
   message("Using Python from: ", Sys.getenv("RETICULATE_PYTHON"))
   reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
}


rm(poetry_env)
