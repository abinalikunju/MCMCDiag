#' Generate Trace Plot for MCMC Chain
#'
#' @description This function generates a trace plot for a given MCMC chain.
#' @param chain Numeric vector representing MCMC chain values.
#' @param parameter Name of the parameter being plotted.
#' @return A trace plot using base R graphics.
#' @export

trace_plot <- function(chain, param_name) {
  df <- data.frame(
    Iteration = seq_along(chain),
    Value = chain
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Iteration, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste("Trace Plot for", param_name),
                  x = "Iteration",
                  y = "Parameter Value")
}
# trace_plot <- function(chain, parameter) {
#   plot(
#     x = seq_along(chain),
#     y = chain,
#     type = "l",
#     main = paste("Trace Plot for", parameter),
#     xlab = "Iteration",
#     ylab = "Parameter Value"
#   )
# }

