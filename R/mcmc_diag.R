#' Comprehensive MCMC Diagnostics
#'
#' @param chains A list of MCMC chains
#' @return A list containing diagnostic results
#' @export
mcmc_diag <- function(chains) {
  gr <- gelman_rubin(chains)
  ess <- calculate_ess(chains)

  result <- list(
    chains = chains,
    gelman_rubin = gr,
    effective_sample_size = ess
  )

  class(result) <- "mcmc_diag"
  return(result)
}

#' @export
print.mcmc_diag <- function(x, ...) {
  cat("MCMC Diagnostic Results:\n")
  cat("Gelman-Rubin statistic:", x$gelman_rubin, "\n")
  cat("Effective Sample Size:", x$effective_sample_size, "\n")
}

#' @export
plot.mcmc_diag <- function(x, ...) {
  trace_plot(x$chains, param_names = paste("Parameter", seq_along(x$chains[[1]])))
}

#' @export
summary.mcmc_diag <- function(object, ...) {
  cat("Summary of MCMC Diagnostics:\n")
  cat("Number of chains:", length(object$chains), "\n")
  cat("Chain length:", length(object$chains[[1]]), "\n")
  cat("Gelman-Rubin statistic:", object$gelman_rubin, "\n")
  cat("Effective Sample Size:", object$effective_sample_size, "\n")
}
