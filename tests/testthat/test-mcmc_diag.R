library(testthat)
library(MCMCDiag)  # Ensure you load the package containing mcmc_diag

test_that("mcmc_diag creates correct object", {
  set.seed(123)
  chains <- list(rnorm(1000), rnorm(1000))
  diag <- mcmc_diag(chains)

  expect_s3_class(diag, "mcmc_diag")
  expect_named(diag, c("chains", "gelman_rubin", "effective_sample_size"))
})

test_that("print method works for mcmc_diag", {
  set.seed(123)
  chains <- list(rnorm(1000), rnorm(1000))
  diag <- mcmc_diag(chains)

  expect_output(print(diag), "MCMC Diagnostic Results:")
  expect_output(print(diag), "Gelman-Rubin statistic:")
  expect_output(print(diag), "Effective Sample Size:")
})

test_that("calculate_ess returns a valid ESS", {
  set.seed(123)
  chains <- list(rnorm(1000))
  ess <- calculate_ess(chains)
  expect_type(ess, "double")
  expect_gt(ess, 0)
})

test_that("trace_plot generates a plot without error", {
  set.seed(123)
  chain <- rnorm(1000)
  expect_silent(trace_plot(chain, "Parameter 1"))
})

test_that("heidelberger_welch_test detects stationarity properly", {
  set.seed(123)
  chain <- rnorm(1000)
  hw_result <- heidelberger_welch_test(chain)
  expect_true(hw_result$stationarity)
})
