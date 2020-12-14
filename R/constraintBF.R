constraintBF <- function(formula, data, whichRandom = NULL, ID,
                        whichConstraint, rscaleEffects,
                        iterationsPosterior = 10000, iterationsPrior = iterationsPosterior*10,
                        burnin = 1000, ...) {

  # make priors: if random then rscale prior = 1

  # run all models
  generalTestObj <- BayesFactor::generalTestBF(formula = formula, data = data,
                                               whichRandom = whichRandom, rscaleEffects = rscaleEffects)

  # get index of full model
  indexFullModel <- extractIndexFullModel(generalTestObj)
  # sample from full model posterior
  thetas <- BayesFactor::posterior(generalTestObj, index = indexFullModel, iterations = iterationsPosterior)
  # clean colnames of posterior sample
  colnames(thetas) <- janitor::make_clean_names(colnames(thetas))

  # get constraints
  constraints <- createConstraints(whichConstraint = whichConstraint)
  IDclean <- janitor::make_clean_names(ID)

  # get indeces for posterior
  regexTheta0 <- paste0("^", constraints$constraintEffect, "_", constraints$constraintUpper, "$")
  iTheta0 <- grep(regexTheta0, colnames(thetas))

  regexThetaID <- paste0("^", IDclean, "_", constraints$constraintEffect, "_", "\\d+", "_", constraints$constraintUpper, "$")
  iThetaID <- grep("^id_cond_\\d+_2$", colnames(thetas))

  # add overall effect to individuals' deviations
  keep <- (burnin + 1) : iterationsPosterior

  totalTheta <- thetas[keep, iTheta0] * 2 + thetas[keep, iThetaID] * 2
  colnames(totalTheta) <- colnames(thetas)[iThetaID] # name columns

  # evaluate posterior probability of all thetas being positive
  good <- totalTheta > 0 #evaluate their sign
  allGood <- apply(good, 1, mean) #evaluate how often all theta estimates are positive
  postProb <- mean(allGood == 1) #Posterior probability of all theta_i being positive

  # get prior probability of all thetas being positive
  # set priors
  a <- 1/2
  b <- 1/2 * 1/10^2
  sd_mu <- 1/6
  I <- length(unique(do.call(`$`, list(x = data, name = ID))))

  # simulate
  s2 <- MCMCpack::rinvgamma(iterationsPrior, a, b)
  mu <- rcauchy(iterationsPrior, 0, sd_mu)
  res <- exp(pnorm(0, mu, sqrt(s2), lower.tail = F, log.p = T) * I)
  priorProb <- mean(res)

  # prepare return values
  bfPU <- postProb / priorProb
  individualEffects <- colMeans(totalTheta)
  posteriorSD <- sd(individualEffects)
  posteriorMean <- mean(thetas[keep, iTheta0])


  return(list(generalTestObj = generalTestObj,
              postProb = postProb,
              priorProb = priorProb,
              bfPU = bfPU,
              individualEffects = individualEffects,
              posteriorSD = posteriorSD,
              posteriorMean = posteriorMean,
              totalTheta = totalTheta,
              mcmcFull = thetas[keep, ]))
}
