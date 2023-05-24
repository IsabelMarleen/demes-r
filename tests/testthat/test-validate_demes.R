test_that("minimal_01.yaml is parsed correctly", {
  setup_demes_spec()
  inp <- yaml::read_yaml(get_test_file("minimal_01.yaml"))
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
  expect_equal(d$demes[[1]]$ancestors, vector(mode="character"))
  expect_identical(d$demes[[1]]$description, '')
  expect_equal(d$demes[[1]]$proportions, vector(mode="double"))
  expect_identical(d$migrations, list())
  expect_identical(d$pulses, list())
})

test_that("R parser results match the reference implementation in Python", {
  setup_env()
  setup_demes_spec()

  # get all valid test YAML files available in the Demes specification repository
  test_files <- file.path(get_spec_dir(), "test-cases", "valid") %>% list.files(pattern = ".yaml")
  # Remove files that cannot be parsed
  # The pre-parsed file that causes issues. It cannot be read by yaml::read_yaml without causing an error unless encoding UTF-16 is specified
  test_files <- test_files[!(test_files %in% c("unicode_deme_name_04.yaml"))]

  for (f in test_files){
    path_preparsed_file <- parse_ref(get_test_file(f))

    # validating a fully parsed model should not change anything
    # Use less strict expectation (as opposed to use_identical), because values should
    # be encoded as doubles, but read_yaml reads 0 values as integers
    true_demes <- yaml::read_yaml(path_preparsed_file)
    more_true_demes <- post_process_expected(true_demes)
    test_demes <- convert_infinity(validate_demes(true_demes))

    expect_equal(order_demes(test_demes),
                 more_true_demes, label = paste(f, "preparsed"))

    # validating the not pre-processed file should produce the same as the pre-processed file
    incomp_demes <- yaml::read_yaml(get_test_file(f))
    test2_demes <- validate_demes(incomp_demes)

    expect_equal(order_demes(test2_demes),
                 more_true_demes, label = paste(f, "parsed de novo"))
  }

  # This test has a couple of work-arounds that should be improved:
  #   1)  JSON files don't encode infinity value, so converting to JSON files during processing is a problem.
  #       It is also an odd round-about way of testing because the package officially only accepts yaml input, so should be changed.
  #   2)  Converting all values in the validation from integers to doubles automatically converts the "Infinity" string to Inf in.
  #       This is done to avoid type errors for users, when default values like 0, read as an integer by read_yaml() are changed interactively
  #   3) 1) and 2) mean that an extra processing step has to happen to the true comparison object in the testing
})

# test_that("R parser rejects all invalid test cases", {
#   setup_demes_spec()
#
#   # get all invalid test YAML files available in the Demes specification repository
#   test_files <- file.path(get_spec_dir(), "test-cases", "invalid") %>% list.files(pattern = ".yaml")
#
#   for (f in test_files){
#     inp <- yaml::read_yaml(get_invalid_test_file(f))
#     try(validate_demes(inp))
#     expect_error(validate_demes(inp), label = f)
#   }
# })
