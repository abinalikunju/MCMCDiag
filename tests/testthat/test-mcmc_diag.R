library(testthat)
library(MCMCDiag)

# Helper function for generating MCMC chains
betaMH <- function(n, a, b, start, proposal_sd) {
  x <- rep(NA, n)
  accepted <- rep(NA, n)
  x[1] <- start
  accepted[1] <- 1
  
  for (i in 2:n) {
    y <- rnorm(1, mean = x[i-1], sd = proposal_sd)
    if (y > 0 && y < 1) {
      r <- dbeta(y, a, b) * dnorm(x[i-1], mean = y, sd = proposal_sd) /
        (dbeta(x[i-1], a, b) * dnorm(y, mean = x[i-1], sd = proposal_sd))
      aprob <- min(1, r)
    } else {
      aprob <- 0
    }
    
    if (runif(1) < aprob) {
      x[i] <- y
      accepted[i] <- 1
    } else {
      x[i] <- x[i-1]
      accepted[i] <- 0
    }
  }
  return(list(x = x, accepted = accepted))
}

test_that("mcmc_diag creates correct object structure", {
  set.seed(123)
  n_iter <- 1000
  burnin <- 100
  
  chain1_data <- betaMH(n_iter + burnin, 2, 1, start = 0.5, proposal_sd = 0.1)
  chain2_data <- betaMH(n_iter + burnin, 2, 1, start = 0.8, proposal_sd = 0.1)
  
  chain1 <- chain1_data$x[(burnin + 1):(burnin + n_iter)]
  chain2 <- chain2_data$x[(burnin + 1):(burnin + n_iter)]
  
  chains <- list(chain1, chain2)
  diag <- mcmc_diag(chains)
  
  expect_s3_class(diag, "mcmc_diag")
  expect_named(diag, c("chains", "gelman_rubin", "effective_sample_size"))
  expect_type(diag$gelman_rubin, "double")
  expect_type(diag$effective_sample_size, "double")
})

test_that("mcmc_diag methods work correctly", {
  set.seed(123)
  n_iter <- 1000
  burnin <- 100
  
  chain1_data <- betaMH(n_iter + burnin, 2, 1, start = 0.5, proposal_sd = 0.1)
  chain2_data <- betaMH(n_iter + burnin, 2, 1, start = 0.8, proposal_sd = 0.1)
  
  chain1 <- chain1_data$x[(burnin + 1):(burnin + n_iter)]
  chain2 <- chain2_data$x[(burnin + 1):(burnin + n_iter)]
  
  chains <- list(chain1, chain2)
  diag <- mcmc_diag(chains)
  
  # Test print method
  expect_output(print(diag), "MCMC Diagnostic Results:")
  expect_output(print(diag), "Gelman-Rubin statistic:")
  expect_output(print(diag), "Effective Sample Size:")
  
  # Test summary method
  expect_output(summary(diag), "Summary of MCMC Diagnostics:")
  expect_output(summary(diag), "Number of chains: 2")
  
  # Test plot method
  expect_silent(plot(diag))
})

test_that("mcmc_diag handles non-convergent chains", {
  set.seed(123)
  n_iter <- 1000
  
  # Create two clearly non-converging chains
  chain1 <- cumsum(rnorm(n_iter, 0, 0.1))
  chain2 <- cumsum(rnorm(n_iter, 0, 0.1)) + 10
  
  chains <- list(chain1, chain2)
  diag <- mcmc_diag(chains)
  
  expect_gt(diag$gelman_rubin, 1.1)
  expect_lt(diag$effective_sample_size, n_iter)
})

test_that("mcmc_diag handles invalid inputs", {
  expect_error(mcmc_diag(list()), "Input must be a list of at least two chains")
  expect_error(mcmc_diag(list("a", "b")), "All chains must contain numeric values")
})
