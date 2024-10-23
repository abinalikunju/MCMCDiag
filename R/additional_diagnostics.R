#' Heidelberger and Welch Test
#'
#' This function performs the Heidelberger and Welch diagnostic test on an MCMC chain.
#' It tests for stationarity and provides additional diagnostic information.
#'
#' @param chain A numeric vector representing an MCMC chain.
#' @param alpha Significance level for the stationarity test (default 0.05).
#' @return A list containing test results: stationarity (logical), halfwidth_test,
#' p_value, and converged_iteration.
#' @export
heidelberger_welch_test <- function(chain, alpha = 0.05) {
  n <- length(chain)
  half_n <- floor(n / 2)

  if (n < 2) {
    stop("The MCMC chain must contain at least two iterations")
  }

  # Split chain into two halves for the stationarity test
  first_half <- chain[1:half_n]
  second_half <- chain[(half_n + 1):n]

  # Calculate the mean difference between the two halves
  mean_diff <- abs(mean(first_half) - mean(second_half))
  var_pooled <- sqrt(var(first_half) / length(first_half) + var(second_half) / length(second_half))

  # Z-statistic for stationarity test
  z_stat <- mean_diff / var_pooled
  p_value <- 2 * (1 - pnorm(abs(z_stat)))  # Two-tailed test

  # Identify converged iteration: iteration at which cumulative diff > alpha
  conv_iter <- which.max(cumsum(abs(diff(chain))) > alpha)
  converged_iteration <- ifelse(length(conv_iter) > 0 && conv_iter > 1, conv_iter, NA)

  # Result of Heidelberger-Welch diagnostic
  result <- list(
    stationarity = p_value > alpha,  # TRUE if chain is stationary
    halfwidth_test = mean(chain) - 2 * sqrt(var(chain)),  # Halfwidth test value
    p_value = p_value,  # P-value for stationarity test
    converged_iteration = converged_iteration  # Iteration where convergence occurs
  )

  return(result)
}


#' Geweke Diagnostic
#' @importFrom stats var
#' @param chain A numeric vector representing an MCMC chain
#' @param first Proportion of chain to use for first segment (default 0.1)
#' @param last Proportion of chain to use for last segment (default 0.5)
#' @return A numeric value representing the Geweke statistic
#' @export

geweke_diagnostic <- function(chain, first = 0.1, last = 0.5) {
  n <- length(chain)

  # First and last sections of the chain
  first_segment <- chain[1:floor(first * n)]
  last_segment <- chain[floor((1 - last) * n):n]

  # Geweke statistic
  z_score <- (mean(first_segment) - mean(last_segment)) /
    sqrt(var(first_segment) / length(first_segment) + var(last_segment) / length(last_segment))

  return(z_score)
}

#' Enhanced Trace Plot for Multiple Chains
#'
#' @param chains A list of numeric vectors representing MCMC chains
#' @param parameter_names A vector of parameter names
#' @return A ggplot object
#' @import ggplot2
#' @export
enhanced_trace_plot <- function(chains, parameter_names) {
  df <- data.frame(
    Iteration = rep(seq_along(chains[[1]]), length(chains)),
    Value = unlist(chains),
    Chain = rep(seq_along(chains), each = length(chains[[1]])),
    Parameter = rep(parameter_names, each = length(chains[[1]]) * length(chains))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Iteration, y = Value, color = factor(Chain))) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ Parameter, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Trace Plots for Multiple Chains",
                  x = "Iteration",
                  y = "Parameter Value",
                  color = "Chain")
}

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
