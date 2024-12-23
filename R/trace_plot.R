#' Generate Trace Plot for MCMC Chains
#'
#' @param chains List of MCMC chains
#' @param param_names Vector of parameter names
#' @return A ggplot object
#' @import ggplot2
#' @export
trace_plot <- function(chain, param_name, max_points = 10000) {
  n <- length(chain)
  if (n > max_points) {
    indices <- seq(1, n, length.out = max_points)
    chain <- chain[indices]
  }

  df <- data.frame(
    Iteration = seq_along(chain),
    Value = chain,
    Parameter = rep(param_name, length(chain))  # Add this line
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Iteration, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ Parameter, nrow = 1) +  # Changed param_name to Parameter
    ggplot2::labs(title = paste("Trace Plot for", param_name),
                  x = "Iteration",
                  y = "Parameter Value")
}



