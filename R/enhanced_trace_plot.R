#' Enhanced Trace Plot
#'
#' @param chains A list of MCMC chains
#' @param parameter_names A vector of parameter names
#' @return A ggplot object
#' @import ggplot2
#' @export
enhanced_trace_plot <- function(chains, parameter_names) {
  max_length <- max(sapply(chains, length))

  df <- data.frame(
    Iteration = rep(1:max_length, length(chains)),
    Value = unlist(lapply(chains, function(x) c(x, rep(NA, max_length - length(x))))),
    Chain = rep(seq_along(chains), each = max_length),
    Parameter = rep(parameter_names, each = max_length * length(chains))
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
