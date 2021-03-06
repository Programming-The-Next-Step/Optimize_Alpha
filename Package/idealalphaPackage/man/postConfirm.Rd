% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{postConfirm}
\alias{postConfirm}
\title{Calculate the probability that the hypothesis is true, after a significant test result.}
\usage{
postConfirm(power, alpha, prior)
}
\arguments{
\item{power}{statistical power of the test.}

\item{alpha}{alpha level.}

\item{prior}{prior probability of H1.}
}
\value{
posterior probability of hypothesis.
}
\description{
Calculate the probability that the hypothesis is true, after a significant test result.
}
\examples{
postConfirm(0.8, 0.05, 0.5)
}
