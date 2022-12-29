d <- load_deme("../../../ex03.yaml")

test_that("example 03 is parsed correctly", {
  expect_true(d$time_units == "generations")
  expect_true(identical(d$doi, list()))
  #expect_true(identical(d$metadata, list()))
  expect_true(identical(d$demes[[1]]$ancestors, list()))
  expect_true(d$demes[[1]]$description == '')
  expect_true(identical(d$demes[[1]]$proportions, list()))
  expect_true(identical(d$migrations, list()))
  expect_true(identical(d$pulses, list()))
  expect_true(d$demes[[1]]$epochs[[1]]$start_size == 1000)
  expect_true(d$demes[[1]]$name ==  "A")

  # Check that there are no additional list entries
  # Make it a verbose test, make a list of expected entries, then use set operations to report what the problem is
})


# Second test compares with internal representation from python and checks that they are the same
# Reference parser
# Or look at c parser
# identical() can compare two nested lists

preparsed_deme <- load_deme("../../../ex03_preparsed.yaml")
preparsed_yaml <- yaml::read_yaml("../../../ex03_preparsed.yaml")
# Could check that parsing the python pre-parsed file does not change anything
test_that("example 03 is parsed the same way as C-parser does", {
  expect_true(identical(preparsed_deme, preparsed_yaml))
})


reticulate::py_install('ruamel.yaml', pip = FALSE)
reticulate::py_run_string("import os; os.system('python ../demes-spec/reference_implementation/resolve_yaml.py ../demes-spec/test-cases/valid/structure_01.yaml > tests/testthat/py_json_temp/valid/structure_01.json')")
reticulate::py_run_file("tests/testthat/parse_ref_test_cases.py")



# test_that("example ", {
#   expect_true()
# })
