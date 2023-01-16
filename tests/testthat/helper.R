# Function to set up python reticulate environment
setup_env <- function(PYTHON_ENV = "demes-r"){
  if (!dir.exists(reticulate::miniconda_path()))
    reticulate::install_miniconda()

  reticulate::conda_install(envname = PYTHON_ENV, packages = c("ruamel.yaml"), pip = FALSE)
}
