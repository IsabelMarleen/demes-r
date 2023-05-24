# TODO: this should be more extensive (perhaps extract this into a function and run
# against multiple (or even all?) YAML files in the Demes spec repository)
test_that("minimal_01.yaml is loaded correctly", {
  setup_demes_spec()
  d <- read_demes(file = file.path(get_spec_dir(), "test-cases", "valid", "minimal_01.yaml"))

  # Test that all the expected list entries are present in the loaded
  # nested-list R structure
  expect_setequal(names(d), c("time_units", "generation_time", "description", "demes",
                              "doi", "metadata", "migrations", "pulses"))

  # Test that all the list entries have the expected entries
  expect_identical(d$time_units, "generations")
  expect_equal(d$demes[[1]]$epochs[[1]]$start_size, 100)
  expect_identical(d$demes[[1]]$name, "a")

  # Test that all the empty values have the right type and are empty
  named_list <- list()
  names(named_list) <- character()
  expect_identical(d$doi, list())
  expect_equal(d$metadata, named_list)
  expect_equal(d$demes[[1]]$ancestors, vector(mode="character"))
  expect_identical(d$demes[[1]]$description, '')
  expect_equal(d$demes[[1]]$proportions, vector(mode="double"))
  expect_identical(d$migrations, list())
  expect_identical(d$pulses, list())
})

test_that("both input types work", {
  setup_demes_spec()
  yaml_string <- "time_units: generations\ndemes:\n  - name: a\n    epochs:\n    - start_size: 100"
  a <- read_demes(text = yaml_string)
  b <- read_demes(file = file.path(get_spec_dir(), "test-cases", "valid", "minimal_01.yaml"))
  expect_identical(order_demes(a), order_demes(b))
})

# TODO:
# test_that("missing YAML file is correctly handled")
# test_that("ill-formatted YAML string input is correctly handled") # see tryCatch()
