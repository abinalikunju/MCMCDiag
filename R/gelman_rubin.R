#' Gelman-Rubin Diagnostic
#'
#' @param chains A list of MCMC chains
#' @return A numeric value representing the Gelman-Rubin statistic
#' @export
gelman_rubin <- function(chains) {
  n <- length(chains[[1]])
  m <- length(chains)

  chain_means <- sapply(chains, mean)
  overall_mean <- mean(chain_means)

  B <- n * var(chain_means)
  W <- mean(sapply(chains, var))

  V_hat <- ((n - 1) / n) * W + (1 / n) * B
  R_hat <- sqrt(V_hat / W)

  return(R_hat)
}
