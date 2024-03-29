% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotEffects.R
\name{plotEffects}
\alias{plotEffects}
\title{Plot a BFBayesFactorConstraint object}
\usage{
plotEffects(x, .raw = FALSE)
}
\arguments{
\item{x}{an object of class \code{\link{BFBayesFactorConstraint-class}}.}

\item{.raw}{if \code{FALSE}, outputs the plot. If \code{TRUE} returns the
\code{data.frame} that is used for making the plot.}
}
\value{
A \code{\link[ggplot2]{ggplot}} object if \code{.raw = FALSE}. A
  \code{data.frame} otherwise, with columns for ID, type of effect,
  specified constraint, estimates, and ordering of estimate from smallest
  to largest.
}
\description{
Plots observed vs estimated individual effects for each level of the effect
  for which constraints are defined.
}
\examples{
\donttest{
data(stroop)

resStroop <- constraintBF(rtS ~ ID*cond,
                          data = stroop,
                          whichRandom = "ID",
                          ID = "ID",
                          whichConstraint = c(cond = "2 > 1"),
                          rscaleEffects = c("ID" = 1, "cond" = 1/6, "ID:cond" = 1/10))

plotEffects(resStroop)
}

}
