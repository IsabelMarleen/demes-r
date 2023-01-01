

test_that("minimal_01.yaml is parsed correctly", {
  inp <- yaml::read_yaml(test_path("data", "valid", "minimal_01.yaml"))
  d <- validate_deme(inp)

  # Test that all the expected list entries are there
  expect_setequal(names(d), c("time_units", "demes", "doi", "metadata", "migrations", "pulses"))

  # Test that all the list entries have the expected entries
  expect_identical(d$time_units, "generations")
  expect_equal(d$demes[[1]]$epochs[[1]]$start_size, 100)
  expect_identical(d$demes[[1]]$name, "a")

  # Test that all the empty values have the right type and are empty
  expect_identical(d$doi, list())
  expect_identical(d$metadata, list())
  expect_identical(d$demes[[1]]$ancestors, list())
  expect_identical(d$demes[[1]]$description, '')
  expect_identical(d$demes[[1]]$proportions, list())
  expect_identical(d$migrations, list())
  expect_identical(d$pulses, list())
})


# Second test compares with internal representation from python and checks that they are the same
# Reference parser
# identical() can compare two nested lists

# Could check that parsing the python pre-parsed file does not change anything
# test_that("example 03 is parsed the same way as C-parser does", {
#   preparsed_deme <- demes::load_deme(test_path("data", "ex03_preparsed.yaml"))
#   preparsed_yaml <- yaml::read_yaml(test_path("data", "ex03_preparsed.yaml"))
#   expect_identical(preparsed_deme, preparsed_yaml)
# })

# test_that("parser does the same as python reference implementation", {
#   #expect_true()
#   reticulate::py_install('ruamel.yaml', pip = FALSE)
#   reticulate::py_run_string("import os; os.system('python demes-spec/reference_implementation/resolve_yaml.py demes-spec/test-cases/valid/structure_01.yaml > tests/testthat/py_json_temp/valid/structure_01.json')")
#   reticulate::py_run_file("tests/testthat/parse_ref_test_cases.py")
# })
