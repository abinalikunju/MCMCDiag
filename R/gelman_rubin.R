#' Gelman-Rubin Diagnostic
#'
#' @param chains A list of MCMC chains
#' @return A numeric value representing the Gelman-Rubin statistic
#' @export
#'
gelman_rubin <- function(chains) {
  # Ensure input is a valid list of numeric chains
  if (!is.list(chains)) {
    stop("Input must be a list of at least two chains")  # Handles the case where input is not a list
  }

  if (length(chains) < 2) {
    stop("Input must be a list of at least two chains")  # Check for minimum length of 2
  }

  if (!all(sapply(chains, is.numeric))) {
    stop("All chains must contain numeric values")
  }

  # Check that all chains have the same length
  n <- length(chains[[1]])
  chain_lengths <- sapply(chains, length)
  if (!all(chain_lengths == n)) {
    stop("All chains must have the same length")
  }

  m <- length(chains)  # Number of chains

  # Compute chain means and overall mean
  chain_means <- sapply(chains, mean)
  overall_mean <- mean(chain_means)

  # Between-chain variance (B) and within-chain variance (W)
  B <- n * var(chain_means)

  epsilon <- 1e-8  # Small number to avoid division by zero
  W <- max(mean(sapply(chains, var)), epsilon)

  if (W < epsilon) {
    warning("Within-chain variance is very small, assuming convergence")
    return(1)  # Assume convergence if variance is too small
  } else {
    V_hat <- ((n - 1) / n) * W + B / n
    R_hat <- sqrt(V_hat / W)
  }

  # Ensure Rhat is within a reasonable range
  if (is.nan(R_hat) || R_hat < 1) {
    warning("Rhat is NaN or less than 1, assuming convergence")
    R_hat <- 1
  }

  return(R_hat)
}
