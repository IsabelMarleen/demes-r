test_that("no 'Infinity' strings remain after parsing a YAML input", {
  setup_env()
  setup_demes_spec()

  #test_files <- list.files(get_test_file_path())
  test_files <- c("minimal_01.yaml", "admixture_27.yaml", "admixture_and_split_01.yaml", "basic_resolution_01.yaml", "bottleneck.yaml")

  for (f in test_files){
    yaml_path <- get_test_file(f)
    path_preparsed_file <- parse_ref(yaml_path)

    inp <- yaml::read_yaml(path_preparsed_file)
    d <- validate_demes(inp)
    d <- convert_infinity(d)
    d_vec <- unlist(d)

    expect_false(any("Infinity" %in% d_vec), label = f)
  }
})
