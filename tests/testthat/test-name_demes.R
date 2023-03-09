test_that("one deme is named correctly", {
  d <- yaml::read_yaml(system.file("extdata/yaml", "minimal_01.yaml", package = "demes"))
  d <- validate_demes(d)
  d <- name_demes(d)

  expect_identical(names(d$demes), c("a"))
  expect_identical(d$demes[[1]], d$demes$a)
})

test_that("several demes are named correctly", {
  d <- yaml::read_yaml(system.file("extdata/yaml", "several_demes_01.yaml", package = "demes"))
  d <- validate_demes(d)
  d <- name_demes(d)

  expected_names <- c(d$demes[[1]]$name, d$demes[[2]]$name, d$demes[[3]]$name)
  expect_identical(names(d$demes), expected_names)
  expect_identical(d$demes[[1]], d$demes$a)
  expect_identical(d$demes[[2]], d$demes[[d$demes[[2]]$name]])
  expect_identical(d$demes[[3]], d$demes[[d$demes[[3]]$name]])
})

# TODO: test on Demes YAML files with multiple populations
# TODO: test on Demes YAML files with different *orders* of such populations
