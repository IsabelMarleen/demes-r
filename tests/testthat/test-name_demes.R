test_that("loaded demes are named correctly", {
  # d <- yaml::read_yaml(test_path("data", "valid", "minimal_01.yaml"))
  d <- yaml::read_yaml(system.file("extdata/yaml", "minimal_01.yaml", package = "demes"))
  d <- validate_demes(d)
  d <- name_demes(d)

  expect_identical(names(d$demes), c("a"))
  expect_identical(d$demes[[1]], d$demes$a)
})

# TODO: test on Demes YAML files with multiple populations
# TODO: test on Demes YAML files with different *orders* of such populations