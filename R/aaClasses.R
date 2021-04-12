setOldClass("mcmc")

#' S4 class for representing constraints
#'
#' \code{BFConstraint} is a S4 class that represents the input and output of
#' user-defined constraints in Bayesian Mixed Model analysis.
#'
#' @slot priorProbability a numeric giving the prior probability of all individual
#'   effects being as defined in the constraints.
#' @slot posteriorProbability a numeric giving the posterior probability of all
#'   individual effects being as defined in the constraints.
#' @slot bayesFactor a numeric giving the Bayes Factor in support of the defined
#'   constraints.
#' @slot constraints a data frame containing the specified constraints.
#' @slot cleanConstraints A data frame containing the specified constraints as
#'   they are presented by \code{\link{constraintBF}}.
#'
#' @importFrom methods show slot
#'
#' @export
#'
setClass("BFConstraint",
         slots = c(
           priorProbability = "numeric",
           posteriorProbability = "numeric",
           bayesFactor = "numeric",
           constraints = "data.frame",
           cleanConstraints = "data.frame")
)

#' S4 class for representing Bayes factor model comparisons and the Bayes factor
#'   for user-defined constraints
#'
#' \code{BFBayesFactorConstraint} is a S4 class that represents the Bayes factors
#'   of multiple model comparisons and the Bayes factor of all individual effects
#'   adhering to user-defined constraints. Furthermore, it also has slots
#'   defined for representing the design matrices of the constraints model, and
#'   for representing prior and posterior estimates.
#'
#'
#' @slot generalTestObj an object of class \code{BFBayesFactor} representing the
#'   model comparisons. All S4 methods as defined in
#'   \code{\link[BayesFactor]{BFBayesFactor-class}} can be used.
#' @slot constraints an object of class \code{\link{BFConstraint-class}}.
#' @slot individualEffects a list containing a named vector for each level of the
#'   effect defined in the constraints. Effects are comprised of the common
#'   effect plus the individual deviation from it.
#' @slot posteriorMean a named numeric giving the mean of the common effect.
#' @slot posteriorSD a named numeric giving the standard deviation of
#'   individual effects.
#' @slot totalThetas a list containing a \code{data.frame} for each level of the
#'   effect defined in the constraints. Rows are sampling iterations and columns
#'   comprised of individual estimates plus the common effect estimates.
#' @slot mcmcFull an object of class \code{BFmcmc}, containing the MCMC samples
#'   from the posterior.
#' @slot designIndeces a list giving the column indeces of the mcmcFull that
#'   were used to calculate the estimates of interest.
#' @slot observedEffects a list containing a named vector for each level of the
#'   effect defined in the constraints. Values are the mean observed effect for
#'   each individual.
#'
#'@importClassesFrom BayesFactor BFBayesFactor
#' @importFrom methods show slot
#'
#' @export
#'
setClass("BFBayesFactorConstraint",
         slots = c(
           generalTestObj = "BFBayesFactor",
           constraints = "BFConstraint",
           individualEffects = "list",
           posteriorMean = "numeric",
           posteriorSD = "numeric",
           totalThetas = "list",
           mcmcFull = "mcmc",
           designIndeces = "list",
           observedEffects = "list")
)
