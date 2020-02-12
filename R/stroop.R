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
#' @section Warning:
#' \code{id}, \code{condition} and \code{rt} all must have the same length.
#'
#'

## Utils for quid

## Assess prior probability that all effects are greater than zero
prior.p.greater <- function(M, I, a = alpha, b = beta, sd_mu = sigma_mu){

  s2 <- MCMCpack::rinvgamma(M, a, b)
  mu <- rcauchy(M, 0, sd_mu)
  res <- exp(pnorm(0, mu, sqrt(s2), lower.tail = F, log.p = T) * I)

  return(mean(res))
}

## Define Design Matrices
prep.models <- function(id, fcond, expect = list(lower = 1, higher = 2)){

  #define the direction of the effect
  l <- as.numeric(levels(fcond)[expect$lower])
  h <- as.numeric(levels(fcond)[expect$higher])

  #design matrices
  I <- length(unique(id))
  R <- length(id)
  X.full <- matrix(nrow = R, ncol = 2 * I + 1, 0)
  for (r in 1:R){
    X.full[r, id[r]] <- 1
    if (fcond[r] == h) {
      X.full[r, I + 1] <- 1
      X.full[r, I + 1 + id[r]] <- 1}}

  gMap.full <- c(rep(0, I), 1, rep(2, I))

  X.one <- matrix(nrow = R, ncol = I + 1, 0)
  for (r in 1:R){
    X.one[r, id[r]] <- 1
    if (fcond[r] == h) {
      X.one[r, I + 1] <- 1
    }}

  gMap.one <- c(rep(0, I), 1)

  X.null <- matrix(nrow = R, ncol = I, 0)
  for(r in 1:R){
    X.null[r, id[r]] <- 1
  }

  gMap.null <- rep(0, I)

  return(list(X.full = X.full
              , gMap.full = gMap.full
              , X.one = X.one
              , gMap.one = gMap.one
              , X.null = X.null
              , gMap.null = gMap.null
              , R = R
              , I= I))
}

## Evaluating Qualitative Individual Differences
make.bf <- function(y, meanScale, effectScale, prep = prep.1, iter = 10000, burnin = 1000)
{
  keep <- (burnin + 1) : iter
  #posterior computation from the unconstrained model
  mcmc.full <- BayesFactor::nWayAOV(y
                                    , prep$X.full
                                    , prep$gMap.full
                                    , rscale = c(1, meanScale, effectScale)
                                    , posterior = T
                                    , iterations = iter)
  #Bayes factor estimation for the unconstrained model
  bf.full <- BayesFactor::nWayAOV(y
                                  , prep$X.full
                                  , prep$gMap.full
                                  , rscale = c(1, meanScale, effectScale)
                                  , posterior = F
                                  , iterations = iter)
  #Bayes factor computation for the common-effect model (but with unconstrained common effect, i.e. can be positive or negative)
  bf.one <- BayesFactor::nWayAOV(y
                                 , prep$X.one
                                 , prep$gMap.one
                                 , rscale = c(1, meanScale)
                                 , posterior = F
                                 , iterations = iter)
  #posterior estimates from the common-effect model (but with unconstrained common effect, i.e. can be positive or negative)
  mcmc.one <- BayesFactor::nWayAOV(y
                                   , prep$X.one
                                   , prep$gMap.one
                                   , rscale = c(1, meanScale)
                                   , posterior = T
                                   , iterations = iter)
  #Bayes factor computation for the null model (with individual baseline for each participant, but no effect)
  bf.null <- BayesFactor::nWayAOV(y
                                  , prep$X.null
                                  , prep$gMap.null
                                  , rscale = 1
                                  , posterior = F
                                  , iterations = iter)

  #Extracting individual effect estimates from the output above
  i.theta0 <- prep$I + 2 #index for the common effect
  i.theta <- (prep$I + 3):(2 * prep$I + 2) #indices for the individual effects

  myTheta <- mcmc.full[keep, i.theta] + mcmc.full[keep, i.theta0] #add oberall effect to individuals' deviations

  #Evaluate how often all individuals' theta_i are positive in the posterior
  good <- myTheta > 0 #evaluate their sign
  all.good <- apply(good, 1, mean) #evaluate how often all theta estimates are postitive
  PostCount <- mean(all.good == 1) #Posterior probability of all theta_i being positive

  #Evaluate how often all individuals' theta_i are positive in the prior
  #prior settings
  R <- iter * 10
  beta <- .5 * effectScale^2
  alpha <- .5
  mu.theta.sd <- meanScale
  PriorCount <- prior.p.greater(M = R, I = prep$I, a = alpha, b = beta, sd_mu = mu.theta.sd)

  #Calculate Bayes factors
  bf.FP <- PriorCount/PostCount
  bf.F0 <- exp(bf.full$bf - bf.null$bf)
  bf.F1 <- exp(bf.full$bf - bf.one$bf)

  #Return posterior estimates from the unconstrained model
  m <- colMeans(myTheta)
  new.sd <- sd(m)
  new.mean <- mean(mcmc.full[keep, i.theta0])
  # i.g3 <- 2 * prep$I + 6
  # new.pop.sd <- mean(mcmc.full[keep, i.g3])
  return(list(ind.effects = m
              , posterior.mean = new.mean, posterior.sd = new.sd
              , bfs = c(bf.1u = 1/ bf.F1, bf.pu =  1/ bf.FP, bf.0u = 1 / bf.F0)
              , theta = myTheta
              , bf.unconstrained = bf.full, bf.common = bf.one, bf.null = bf.null
              , mcmc.unconstrained = mcmc.full[keep, ], mcmc.one = mcmc.one[keep, ]
              , prior.prob = PriorCount, posterior.prob = PostCount
              , design.matrices = prep))
}

quid <- function(id #vector with participant ID, can be a factor or numeric
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
