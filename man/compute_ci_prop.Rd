% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_metrics.R
\name{compute_ci_prop}
\alias{compute_ci_prop}
\title{Compute 95\% CI for a proportion}
\usage{
compute_ci_prop(p, n, alpha = 0.05)
}
\arguments{
\item{p}{The proportion.}

\item{n}{Number of observations.}

\item{alpha}{Target error rate. Default is 0.05 for 95\% CI.}
}
\value{
Returns a vector of the 95\% CI.
}
\description{
Compute 95\% CI for a proportion
}
