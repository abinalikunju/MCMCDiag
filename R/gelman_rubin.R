#' Gelman-Rubin Diagnostic
#'
#' @param chains A list of MCMC chains
#' @return A numeric value representing the Gelman-Rubin statistic
#' @export
#'
#' @examples
#' set.seed(123)
#' chains <- list(rnorm(1000), rnorm(1000))
#' gr_stat <- gelman_rubin(chains)
#' print(gr_stat)
gelman_rubin <- function(chains) {
  if (!is.list(chains) || length(chains) < 2) {
    stop("Input must be a list of at least two chains")
  }
  if (!all(sapply(chains, is.numeric))) {
    stop("All chains must contain numeric values")
  }

  # Number of iterations (assuming all chains have the same length)
  n <- length(chains[[1]])
  m <- length(chains)

  # Compute chain means and overall mean
  chain_means <- sapply(chains, mean)
  overall_mean <- mean(chain_means)

  # Between-chain variance (B) and within-chain variance (W)
  B <- n * var(chain_means)
  W <- mean(sapply(chains, var))

  # Potential scale reduction factor
  V_hat <- ((n - 1) / n) * W + B / n
  R_hat <- sqrt(V_hat / W)

  # Ensure Rhat is greater than 1 for non-converging chains
  if (is.nan(R_hat) || R_hat < 1) {
    R_hat <- 1
  }

  return(R_hat)
}


