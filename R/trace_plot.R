#' #' Trace Plot
#' #'
#' #' @param chain MCMC chain
#' #' @param parameter Name of the parameter to plot
#' #' @return ggplot object
#' #' @export
#' #' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs
#' trace_plot <- function(chain, parameter) {
#'   df <- data.frame(
#'     Iteration = seq_along(chain),
#'     Value = chain
#'   )
#'
#'   ggplot(df, aes(x = Iteration, y = Value)) +
#'     geom_line() +
#'     theme_minimal() +
#'     labs(title = paste("Trace Plot for", parameter),
#'          x = "Iteration",
#'          y = "Parameter Value")
#' }
#' Generate Trace Plot for MCMC Chain
#'
#' @param chain Numeric vector representing MCMC chain values
#' @param parameter Name of the parameter being plotted
#' @return A trace plot using base R graphics
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
