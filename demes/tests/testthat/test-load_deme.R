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
