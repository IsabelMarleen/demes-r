# Function to set up python reticulate environment
setup_env <- function(PYTHON_ENV = "demes-r"){
  if (!dir.exists(reticulate::miniconda_path()))
    reticulate::install_miniconda()

  reticulate::conda_install(envname = PYTHON_ENV, packages = c("ruamel.yaml"), pip = FALSE)
}

# Function to parse test files using the reference implementation, given that a condaenv env is active
parse_ref <- function(input_file, path_tmp_dir){
  path_ref_implementation <- fs::path_package("demes", "tools", "demes-spec", "reference_implementation", "resolve_yaml.py")
  path_preparsed_file <- file.path(path_tmp_dir, gsub('yaml', 'json', input_file))

  py_command <- paste("import os; os.system('python", path_ref_implementation, fs::path(get_test_file_path(), input_file), ">", path_preparsed_file, "')")
  reticulate::py_run_string(py_command)

  return(path_preparsed_file)
}

# Function to return test file location
get_test_file_path <- function(){
  return(fs::path_package("demes", "tools", "demes-spec", "test-cases", "valid"))
}
