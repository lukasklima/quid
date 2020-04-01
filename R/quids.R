#' Qualitative Individual Differences (Stroop)
#'
#' This quid function can be used to estimate individual effect sizes and to
#' evaluate order constraints in settings with two conditions, one experimental
#' and one control.
#'
#' @param id Numeric or factor. A vector indicating the participant ID.
#' @param condition Numeric or factor. A vector indicating the condition.
#' @param rt Numeric. A vector containing the response times for each trial.
#'
#' @import MCMCpack
#' @import BayesFactor
#'
#' @export
#'
#' @return This function returns a \code{list} containing the following elements:
#' \itemize{
#'  \item theta
#'  \item BF
#'}
#'
#' @section Warning:
#' \code{id}, \code{condition} and \code{rt} all must have the same length.
#'

quids <- function(x, ...) {
  UseMethod("quids")
}

quids.default <- function(id #vector with participant ID, can be a factor or numeric
                 , condition #vector with condition ID, can be a factor or numeric
                 , rt #vector with response times per trial, has to be numeric
                 , prior = c(1/6, 1/10) #prior scale settings for the standard deviation of the overall effect and the individual effects
                 , expect = list(lower = 1, higher = 2) #which value of condition represents the condition with the expected higher values, and lower values
                 , iter = 10000 #number of iterations for the posterior sampling
                 , burnin = 1000 #number of to be discarded burn-in iterations
                 , messages = TRUE
){

  require(BayesFactor, quietly = messages)

  if(!(is.numeric(condition) | is.factor(condition))) stop("Condition has to be numeric or a factor.")
  if(!(is.numeric(id) | is.factor(id))) stop("Id has to be numeric or a factor.")
  if(!(is.numeric(rt))) stop("Rt has to be numeric.")

  fcond <- factor(condition)

  if(length(fcond) != length(id)) stop("Your condition vector does not match your id vector.")
  if(length(levels(fcond)) != 2) stop("Your condition vector has more than two levels.")

  sub <- as.numeric(factor(id))

  # get design matrix
  prep.1 <- prep.models(id = sub, fcond = fcond, expect = expect)

  # get bayes factors
  make.bf(y = rt, meanScale = prior[1], effectScale = prior[2], prep = prep.1, iter = iter, burnin = burnin)
}
