

test_that("minimal_01.yaml is parsed correctly", {
  inp <- yaml::read_yaml(get_test_file_path("minimal_01.yaml"))
  d <- validate_demes(inp)

  # Test that all the expected list entries are there
  expect_setequal(names(d), c("time_units", "generation_time", "description", "demes", "doi", "metadata", "migrations", "pulses"))

  # Test that all the list entries have the expected entries
  expect_identical(d$time_units, "generations")
  expect_equal(d$demes[[1]]$epochs[[1]]$start_size, 100)
  expect_identical(d$demes[[1]]$name, "a")

  # Test that all the empty values have the right type and are empty
  expect_identical(d$doi, list())
  named_list <- list()
  names(named_list) <- character()
  expect_equal(d$metadata, named_list)
  expect_equal(d$demes[[1]]$ancestors, list())
  expect_identical(d$demes[[1]]$description, '')
  expect_equal(d$demes[[1]]$proportions, vector(mode="integer"))
  expect_identical(d$migrations, list())
  expect_identical(d$pulses, list())
})



test_that("parser does the same as python reference implementation", {

  PYTHON_ENV <- "demes-r"
  setup_env(PYTHON_ENV)
  reticulate::use_condaenv(PYTHON_ENV, required=TRUE)

  test_files <- list.files(get_test_file_path())
  test_files <- test_files[test_files %in% c(paste0("minimal_0", 1:2, ".yaml"),
                                             paste0("admixture_0", 1:9, ".yaml"), paste0("admixture_", 10:27, ".yaml"),
                                             paste0("deme_names_0", 1:3, ".yaml"),
                                             paste0("migration_0", 1:9, ".yaml"), "migration_10.yaml",
                                             paste0("structure_0", 1:8, ".yaml"),
                                             "args_from_file_01.yaml",
                                             "admixture_and_split_01.yaml",
                                             "asymmetric_migration_01.yaml",
                                             "bad_pulse_time_01.yaml",
                                             paste0("deme_cloning_rate_0", 1:3, ".yaml"),
                                             "deme_selfing_rate_01.yaml",
                                             "size_function_defaults_01.yaml",
                                             paste0("split_0", 1:9, ".yaml"), "split_10.yaml",
                                             "selfing_cloning_01.yaml",
                                             paste0("size_changes_0", 1:9, ".yaml"), paste0("size_changes_", 10:32, ".yaml")
                                              )]
  path_tmp_dir <- withr::local_tempdir()

  for (f in test_files){
    path_preparsed_file <- parse_ref(input_file = f, path_tmp_dir = path_tmp_dir)

    # validating a fully parsed model should not change anything
    # Use less strict expectation (as opposed to use_identical), because values should be encoded as doubles,
    # but read_yaml reads 0 values as integers
    true_deme <- yaml::read_yaml(path_preparsed_file)
    more_true_deme <- post_process_expected(true_deme)
    test_deme <- convert_infinity(validate_demes(true_deme))

    expect_equal(order_demes(test_deme), more_true_deme, label = paste(f, "preparsed"))

    # validating the not pre-processed file should produce the same as the pre-processed file
    incomp_deme <- yaml::read_yaml(get_test_file_path(f))
    test2_deme <- validate_demes(incomp_deme)

    expect_equal(order_demes(test2_deme), more_true_deme, label = paste(f, "parsed de novo"))
  }

  # This test has a couple of work-arounds that should be improved:
  #   1)  JSON files don't encode infinity value, so converting to JSON files during processing is a problem.
  #       It is also an odd round-about way of testing because the package officially only accepts yaml input, so should be changed.
  #   2)  Converting all values in the validation from integers to doubles automatically converts the "Infinity" string to Inf in.
  #       This is done to avoid type errors for users, when default values like 0, read as an integer by read_yaml() are changed interactively
  #   3) 1) and 2) mean that an extra processing step has to happen to the true comparison object in the testing
})
