test_that("gelman_rubin works for well-mixed chains", {
  set.seed(123)
  chains <- list(rnorm(1000), rnorm(1000))
  expect_lt(gelman_rubin(chains), 1.1)
})

test_that("gelman_rubin detects poorly mixed chains", {
  set.seed(123)
  chains <- list(rnorm(1000), rnorm(1000) + 10)
  expect_gt(gelman_rubin(chains), 1.1)
})
