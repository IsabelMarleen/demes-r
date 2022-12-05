d <- load_deme("../../../ex03.yaml")

test_that("example 03 is parsed correctly", {
  expect_true(d$time_units == "generations")
  expect_true(identical(d$doi, list()))
  expect_true(identical(d$metadata, list()))
  expect_true(identical(d$demes[[1]]$ancestors, list()))
  expect_true(d$demes[[1]]$description == '')
  expect_true(identical(d$demes[[1]]$proportions, list()))
  expect_true(identical(d$demes[[1]]$migrations, list()))
  expect_true(identical(d$demes[[1]]$pulses, list()))
  expect_true(d$demes[[1]]$epochs[[1]]$start_size == 1000)
  expect_true(d$demes[[1]]$name ==  "A")
})


# Second test compares with internal representation from python and checks that they are the same
# Reference parser
# Or look at c parser
# identical() can compare two nested lists

# Could check that parsing the python pre-parsed file does not change anything
# test_that("example 03 is parsed correctly", {
#   d <- load_deme("../../../ex03.yaml")
#
#
#   expect_known_value(d$time_units, "generations")
#   expect_type(d$doi, list)
#   expect_type(d$metadata, list)
#   expect_type(d$demes[[1]]$ancestors, list)
#   expect_known_value(d$demes[[1]]$description, '')
#   expect_type(d$demes[[1]]$proportions, list)
#   expect_type(d$demes[[1]]$migrations, list)
#   expect_type(d$demes[[1]]$pulses, list)
#   expect_known_value(d$demes[[1]]$epochs$start_size, 1000)
#   expect_known_value(d$demes[[1]]$name, "A")
# })
