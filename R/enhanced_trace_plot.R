#' Enhanced Trace Plot
#'
#' @param chains A list of MCMC chains
#' @param param_names A vector of parameter names
#' @return A ggplot object
#' @import ggplot2
#' @export
enhanced_trace_plot <- function(chains, param_names) {
  n_chains <- length(chains)
  n_params <- length(param_names)

  plot_data <- data.frame()
  for (i in 1:n_chains) {
    chain_data <- as.data.frame(chains[[i]])
    chain_data$Iteration <- 1:nrow(chain_data)
    chain_data$Chain <- factor(i)
    plot_data <- rbind(plot_data, chain_data)
  }

  plots <- list()
  for (i in 1:n_params) {
    p <- ggplot(plot_data, aes(x = Iteration, y = plot_data[,i], color = Chain)) +
      geom_line() +
      labs(title = paste("Trace Plot for", param_names[i]),
           y = param_names[i]) +
      theme_minimal()
    plots[[i]] <- p
  }

  return(plots)
}

