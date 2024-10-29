library(testthat)
library(MCMCDiag)

# Helper function for generating non-converging chains
generate_sticky_chains <- function(n, start_points) {
  lapply(start_points, function(start) {
    chain <- numeric(n)
    chain[1] <- start
    for(i in 2:n) {
      # Very high probability of staying at current value
      chain[i] <- if(runif(1) < 0.999) chain[i-1] else start + rnorm(1, 0, 0.01)
    }
    return(chain)
  })
}

test_that("gelman_rubin detects convergence with well-behaved chains", {
  set.seed(123)
  chains <- list(
    rnorm(1000, mean = 0, sd = 1),
    rnorm(1000, mean = 0, sd = 1)
  )
  expect_lt(gelman_rubin(chains), 1.1)
})

test_that("gelman_rubin detects lack of convergence", {
  set.seed(123)
  n_iter <- 1000
  
  # Generate chains that are stuck in different regions
  chains <- generate_sticky_chains(n_iter, c(0, 10))
  gr_stat <- gelman_rubin(chains)
  
  # Print diagnostic information
  cat("\nGR statistic:", gr_stat)
  cat("\nChain 1 mean:", mean(chains[[1]]))
  cat("\nChain 2 mean:", mean(chains[[2]]))
  cat("\nChain 1 var:", var(chains[[1]]))
  cat("\nChain 2 var:", var(chains[[2]]))
  
  expect_gt(gr_stat, 1.1)
})

test_that("gelman_rubin handles invalid inputs appropriately", {
  expect_error(gelman_rubin(list(1:10)), "Input must be a list of at least two chains")
  expect_error(gelman_rubin(list("a", "b")), "All chains must contain numeric values")
})

test_that("gelman_rubin handles extreme values", {
  set.seed(123)
  n_iter <- 1000
  
  # Create chains with very different scales
  chain1 <- rnorm(n_iter, mean = 0, sd = 1)
  chain2 <- rnorm(n_iter, mean = 1e6, sd = 1e3)
  
  chains <- list(chain1, chain2)
  gr_stat <- gelman_rubin(chains)
  
  expect_gt(gr_stat, 1.1)
})

test_that("gelman_rubin detects slow convergence", {
  set.seed(123)
  n_iter <- 1000
  
  # Create chains with very slow random walks
  chain1 <- cumsum(rnorm(n_iter, 0, 0.01))
  chain2 <- cumsum(rnorm(n_iter, 0, 0.01)) + 10
  
  chains <- list(chain1, chain2)
  gr_stat <- gelman_rubin(chains)
  
  expect_gt(gr_stat, 1.1)
})
