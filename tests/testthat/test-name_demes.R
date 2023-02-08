test_that("demes are named correctly", {
  d <- yaml::read_yaml(test_path("data", "valid", "minimal_01.yaml"))
  d <- validate_demes(d)
  d <- name_demes(d)

  expect_identical(names(d$demes), c("a"))
  expect_identical(d$demes[[1]], d$demes$a)
})
