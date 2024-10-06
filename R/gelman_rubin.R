#' Gelman-Rubin Diagnostic
#'
#' @param chains A list of MCMC chains
#' @return A numeric value representing the Gelman-Rubin statistic
#' @export
gelman_rubin <- function(chains) {
  if (!is.list(chains) || length(chains) < 2) {
    stop("Input must be a list of at least two chains")
  }

  # Call the C++ function
  r_hat <- gelman_rubin_cpp(chains)
  return(r_hat)
}

