#' Generate Trace Plot for MCMC Chain
#'
#' @description This function generates a trace plot for a given MCMC chain.
#' @param chain Numeric vector representing MCMC chain values.
#' @param parameter Name of the parameter being plotted.
#' @return A trace plot using base R graphics.
#' @export

trace_plot <- function(chain, parameter) {
  plot(
    x = seq_along(chain),
    y = chain,
    type = "l",
    main = paste("Trace Plot for", parameter),
    xlab = "Iteration",
    ylab = "Parameter Value"
  )
}
