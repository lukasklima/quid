constraintBF <- function(formula, data, whichRandom = NULL, ID,
                        whichConstraint, rscaleEffects,
                        iterationsPosterior = 10000, iterationsPrior = iterationsPosterior * 10,
                        burnin = 1000, ...) {

  if (any(attr(terms(formula, data = data), "order") > 3 )) {
    stop("constraintBF currently only supports interactions of up to 3 terms.")
  }

  # make priors: if random then rscale prior = 1
  # capture ellipsis and pass to generalTestBF

  # run all models
  generalTestObj <- BayesFactor::generalTestBF(formula = formula, data = data,
                                               whichRandom = whichRandom, rscaleEffects = rscaleEffects)

  # get index of full model
  indexFullModel <- extractIndexFullModel(generalTestObj)
  # sample from full model posterior
  thetas <- BayesFactor::posterior(generalTestObj, index = indexFullModel, iterations = iterationsPosterior)

  # clean names
  colnames(thetas) <- janitor::make_clean_names(colnames(thetas))
  ID <- janitor::make_clean_names(ID)

  # get constraints
  constraints <- createConstraints(whichConstraint = whichConstraint)
  cleanConstraints <- createCleanConstraints(constraints = constraints)

  # get indeces for posterior
  iTheta <- extractIndeces(constraints = constraints, thetas = thetas, ID = ID, data = data)

  # add overall effect to individuals' deviations
  keep <- (burnin + 1) : iterationsPosterior

  totalThetas <- addThetas(thetas = thetas, iTheta = iTheta, keep = keep)

  # evaluate posterior probability of all thetas being positive
  constrainedThetas <- estimateConstrainedThetas(totalThetas = totalThetas, cleanConstraints = cleanConstraints)
  pass <- apply(constrainedThetas, 1, mean) == 1
  posteriorProbability <- mean(pass)

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
