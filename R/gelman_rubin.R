#' Gelman-Rubin Diagnostic (C++ Implementation)
#'
#' This function calculates the Gelman-Rubin diagnostic for MCMC convergence using a C++ implementation.
#'
#' @param chains A list of MCMC chains. Each chain should be a numeric vector.
#' @return A numeric value representing the potential scale reduction factor (PSRF).
#' @export
#' @examples
#' chains <- list(rnorm(1000), rnorm(1000), rnorm(1000))
#' gelman_rubin_cpp(chains)
gelman_rubin_cpp <- function(chains) {
  # This function calls the C++ implementation
  .Call('_MCMCDiag_gelman_rubin_cpp', PACKAGE = 'MCMCDiag', chains)
}
