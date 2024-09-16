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
gelman_rubin <- function(chains, burn_in = 0) {
  # Implementation using the C++ function
  chains <- lapply(chains, function(chain) chain[(burn_in + 1):length(chain)])
  result <- gelman_rubin_cpp(chains)
  return(result)
}
