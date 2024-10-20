#' Generate Trace Plot for MCMC Chains
#'
#' @param chains List of MCMC chains
#' @param param_names Vector of parameter names
#' @return A ggplot object
#' @import ggplot2
#' @export
trace_plot <- function(chains, param_names) {
  df <- data.frame(
    Iteration = rep(seq_along(chains[[1]]), length(chains)),
    Value = unlist(chains),
    Chain = factor(rep(seq_along(chains), each = length(chains[[1]])))
  )

  ggplot(df, aes(x = Iteration, y = Value, color = Chain)) +
    geom_line() +
    facet_wrap(~ rep(param_names, each = length(chains[[1]]) * length(chains)), scales = "free_y") +
    labs(title = "Trace Plots", x = "Iteration", y = "Parameter Value") +
    theme_minimal()
}
