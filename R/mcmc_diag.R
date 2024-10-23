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
  max_length <- max(sapply(x$chains, length))

  chain_data <- data.frame(
    Iteration = rep(1:max_length, length(x$chains)),
    Value = unlist(lapply(x$chains, function(chain) {
      c(chain, rep(NA, max_length - length(chain)))
    })),
    Chain = rep(seq_along(x$chains), each = max_length)
  )

  ggplot2::ggplot(chain_data, ggplot2::aes(x = Iteration, y = Value, color = factor(Chain))) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "MCMC Diagnostic Plots",
                  x = "Iteration",
                  y = "Parameter Value",
                  color = "Chain")
}

#' @export
summary.mcmc_diag <- function(object, ...) {
  cat("Summary of MCMC Diagnostics:\n")
  cat("Number of chains:", length(object$chains), "\n")
  cat("Chain length:", length(object$chains[[1]]), "\n")
  cat("Gelman-Rubin statistic:", object$gelman_rubin, "\n")
  cat("Effective Sample Size:", object$effective_sample_size, "\n")
}
