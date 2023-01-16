test_that("no 'Infinity' strings remain", {
  PYTHON_ENV <- "demes-r"
  setup_env(PYTHON_ENV)
  reticulate::use_condaenv(PYTHON_ENV, required=TRUE)

  test_files <- list.files(testthat::test_path("data", "valid"))
  path_tmp_dir <- withr::local_tempdir()

  for (f in test_files){
    path_ref_implementation <- testthat::test_path("reference_implementation", "resolve_yaml.py")
    path_preparsed_file <- file.path(path_tmp_dir, gsub('yaml', 'json', f))

    py_command <- paste("import os; os.system('python", path_ref_implementation, testthat::test_path("data", "valid", f), ">", path_preparsed_file, "')")
    reticulate::py_run_string(py_command)

    inp <- yaml::read_yaml(test_path(path_preparsed_file))
    d <- validate_deme(inp)
    d <- convert_infinity(d)
    d_vec <- unlist(d)

    expect_false(any("Infinity" %in% d_vec), label=f)
  }
})
