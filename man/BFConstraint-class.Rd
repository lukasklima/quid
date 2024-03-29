% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaClasses.R
\docType{class}
\name{BFConstraint-class}
\alias{BFConstraint-class}
\title{S4 class for representing constraints}
\description{
\code{BFConstraint} is a S4 class that represents the input and output of
user-defined constraints in Bayesian Mixed Model analysis.
}
\section{Slots}{

\describe{
\item{\code{priorProbability}}{a numeric giving the prior probability of all individual
effects being as defined in the constraints.}

\item{\code{posteriorProbability}}{a numeric giving the posterior probability of all
individual effects being as defined in the constraints.}

\item{\code{bayesFactor}}{a numeric giving the Bayes Factor in support of the defined
constraints.}

\item{\code{constraints}}{a data frame containing the specified constraints.}

\item{\code{cleanConstraints}}{A data frame containing the specified constraints as
they are presented by \code{\link{constraintBF}}.}
}}

