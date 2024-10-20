#' Generate a simple MCMC chain using Metropolis-Hastings
#'
#' @param n Number of iterations
#' @param a Shape parameter for Beta distribution
#' @param b Shape parameter for Beta distribution
#' @param start Starting value
#' @param proposal_sd Standard deviation for proposal distribution
#' @return A list containing the chain and acceptance indicators
#' @export
generate_mcmc_chain <- function(n, a, b, start, proposal_sd) {
  x <- numeric(n)
  accepted <- logical(n)
  x[1] <- start
  accepted[1] <- TRUE

  for (i in 2:n) {
    proposal <- rnorm(1, mean = x[i-1], sd = proposal_sd)
    r <- dbeta(proposal, a, b) * dnorm(x[i-1], mean = proposal, sd = proposal_sd) /
      (dbeta(x[i-1], a, b) * dnorm(proposal, mean = x[i-1], sd = proposal_sd))

    if (runif(1) < min(1, r)) {
      x[i] <- proposal
      accepted[i] <- TRUE
    } else {
      x[i] <- x[i-1]
      accepted[i] <- FALSE
    }
  }

  return(list(x = x, accepted = accepted))
}
