% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trace_plot.R
\name{trace_plot}
\alias{trace_plot}
\title{Generate Trace Plot for MCMC Chains}
\usage{
trace_plot(chain, param_name, max_points = 10000)
}
\arguments{
\item{chains}{List of MCMC chains}

\item{param_names}{Vector of parameter names}
}
\value{
A ggplot object
}
\description{
Generate Trace Plot for MCMC Chains
}
