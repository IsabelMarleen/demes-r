

test_that("minimal_01.yaml is loaded correctly", {
  d <- read_demes(file = test_path("data", "valid", "minimal_01.yaml"))

  # Test that all the expected list entries are there
  expect_setequal(names(d), c("time_units", "generation_time", "doi", "description", "metadata", "demes", "migrations", "pulses"))

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


test_that("both input types work", {
  yaml_string <- "time_units: generations\ndemes:\n  - name: a\n    epochs:\n    - start_size: 100"
  b <- read_demes(text=yaml_string)
  d <- read_demes(file = test_path("data", "valid", "minimal_01.yaml"))
  expect_identical(order_demes(b), order_demes(d))
})
