% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_variance}
\alias{bin_variance}
\title{bin_variance}
\usage{
bin_variance(trials, prob)
}
\arguments{
\item{trials}{number of trials; non-negative integer value}

\item{prob}{probability of success; must be a numeric value between 0 and 1 (inclusive)}
}
\value{
numeric value indicating variance
}
\description{
Computes the expectation of the squared deviation from the mean
}
\examples{
var <- bin_variance(10, 0.3)
var
}
