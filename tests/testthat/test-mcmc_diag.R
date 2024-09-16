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
