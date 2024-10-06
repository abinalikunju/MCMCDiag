#' @useDynLib MCMCDiag, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Calculate Effective Sample Size (ESS)
#'
#' @description This function calculates the Effective Sample Size for MCMC chains.
#' @param chains List of MCMC chains (numeric vectors).
#' @return Effective Sample Size for the provided chains.
#' @export
calculate_ess <- function(chains) {
  # Placeholder for actual ESS calculation
  # Using autocorrelation to approximate ESS
  chain <- chains[[1]] # Assuming single chain for simplicity

  # Estimate autocorrelation for the chain
  acf_values <- acf(chain, plot = FALSE)$acf[-1]

  # Calculate ESS using the sum of autocorrelation values
  n <- length(chain)
  ess <- n / (1 + 2 * sum(acf_values))

  return(ess)
}

#' Gelman-Rubin Diagnostic
#'
#' @param chains A list of MCMC chains
#' @return The Gelman-Rubin statistic
#' @export
gelman_rubin <- function(chains) {
  gelman_rubin_cpp(chains)
}
