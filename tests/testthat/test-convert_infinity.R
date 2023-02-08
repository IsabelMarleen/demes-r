test_that("no 'Infinity' strings remain", {
  PYTHON_ENV <- "demes-r"
  setup_env(PYTHON_ENV)
  reticulate::use_condaenv(PYTHON_ENV, required=TRUE)

  #test_files <- list.files(get_test_file_path())
  test_files <- c("admixture_27.yaml", "admixture_and_split_01.yaml", "basic_resolution_01.yaml", "bottleneck.yaml")
  path_tmp_dir <- withr::local_tempdir()

  for (f in test_files){
    path_preparsed_file <- parse_ref(input_file = f, path_tmp_dir = path_tmp_dir)

    inp <- yaml::read_yaml(path_preparsed_file)
    d <- validate_demes(inp)
    d <- convert_infinity(d)
    d_vec <- unlist(d)

    expect_false(any("Infinity" %in% d_vec), label=f)
  }
})
