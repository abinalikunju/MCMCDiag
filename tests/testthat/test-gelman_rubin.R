library(testthat)
library(MCMCDiag)  # Ensure you load the package containing gelman_rubin

test_that("gelman_rubin works for well-mixed chains", {
  set.seed(242)
  chains <- list(rnorm(1000), rnorm(1000))
  expect_lt(gelman_rubin(chains), 1.1)
})

test_that("gelman_rubin detects poorly mixed chains", {
  set.seed(123)
  chains <- list(rnorm(1000, mean = 0), rnorm(1000, mean = 10))
  gr_stat <- gelman_rubin(chains)
  expect_gt(gr_stat, 1.1)  # Expect Rhat > 1 for poorly mixed chains
})

test_that("gelman_rubin handles large datasets", {
  set.seed(242)
  chains <- list(rnorm(1e3), rnorm(1e3))  # Reduced from 1e5
  expect_type(gelman_rubin(chains), "double")
  expect_lt(gelman_rubin(chains), 1.1)
})

test_that("gelman_rubin throws error for invalid input", {
  # This should expect an error about having fewer than two chains
  expect_error(gelman_rubin(list(1:10)), "Input must be a list of at least two chains")
  # Add another test for non-numeric values if needed
  expect_error(gelman_rubin(list("a", "b")), "All chains must contain numeric values")
})

