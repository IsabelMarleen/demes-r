test_that("minimal_01.yaml is loaded correctly", {
  d <- read_demes(file = fs::path_package("demes", "tools", "demes-spec", "test-cases", "valid", "minimal_01.yaml"))

  # Test that all the expected list entries are there
  expect_setequal(names(d), c("time_units", "generation_time", "description", "demes", "doi", "metadata", "migrations", "pulses"))

  # Test that all the list entries have the expected entries
  expect_identical(d$time_units, "generations")
  expect_equal(d$demes[[1]]$epochs[[1]]$start_size, 100)
  expect_identical(d$demes[[1]]$name, "a")

  # Test that all the empty values have the right type and are empty
  named_list <- list()
  names(named_list) <- character()
  expect_identical(d$doi, list())
  expect_equal(d$metadata, named_list)
  expect_equal(d$demes[[1]]$ancestors, list())
  expect_identical(d$demes[[1]]$description, '')
  expect_equal(d$demes[[1]]$proportions, vector(mode="integer"))
  expect_identical(d$migrations, list())
  expect_identical(d$pulses, list())
})


test_that("both input types work", {
  yaml_string <- "time_units: generations\ndemes:\n  - name: a\n    epochs:\n    - start_size: 100"
  b <- read_demes(text=yaml_string)
  d <- read_demes(file = fs::path_package("demes", "tools", "demes-spec", "test-cases", "valid", "minimal_01.yaml"))
  expect_identical(order_demes(b), order_demes(d))
})
