constraintBF <- function(formula, data, whichRandom, ID,
                        whichConstraint, rscaleEffects,
                        iterationsPosterior = 10000, iterationsPrior = iterationsPosterior * 10,
                        burnin = 1000, ...) {

  # Assertions
  ellipsis::check_dots_used()
  checkFormulaData(formula = formula, data = data)
  checkID(ID = ID, data = data)
  checkConstraints(whichConstraint = whichConstraint, data = data)
  checkPriors(rscaleEffects = rscaleEffects, formula = formula, data = data, ID = ID, whichConstraint = whichConstraint)

  # run all models
  generalTestObj <- BayesFactor::generalTestBF(formula = formula, data = data,
                                               whichRandom = whichRandom,
                                               rscaleEffects = rscaleEffects,
                                               ...)

  # get index of full model
  indexFullModel <- extractIndexFullModel(generalTestObj)
  # sample from full model posterior
  thetas <- BayesFactor::posterior(generalTestObj, index = indexFullModel, iterations = iterationsPosterior)

  # clean names
  colnames(thetas) <- cleanName(colnames(thetas))
  IDorg <- ID
  effectNameOrg <- unique(names(whichConstraint))
  ID <- cleanName(ID)

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
  passThetas <- apply(constrainedThetas, 1, mean) == 1
  posteriorProbability <- mean(passThetas)

  # get prior probability of all thetas being positive
  passPrior <- estimatePriorProbability(iTheta = iTheta,
                                        rscaleEffects = rscaleEffects,
                                        iterationsPrior = iterationsPrior,
                                        cleanConstraints = cleanConstraints,
                                        IDorg = IDorg,
                                        effectNameOrg = effectNameOrg)

  priorProbability <- mean(passPrior)

  # prepare return values
  bfCU <- posteriorProbability / priorProbability
  individualEffects <- lapply(totalThetas, colMeans)
  posteriorSD <- lapply(individualEffects, sd)
  posteriorMean <- colMeans(thetas[keep, iTheta$commonEffect])

  return(list(generalTestObj = generalTestObj,
              posteriorProbability = posteriorProbability,
              priorProbability = priorProbability,
              bfConstrainedUnconstrained = bfCU,
              individualEffects = individualEffects,
              posteriorSD = posteriorSD,
              posteriorMean = posteriorMean,
              totalThetas = totalThetas,
              mcmcFull = thetas[keep, ],
              constraints = constraints,
              cleanConstraints = cleanConstraints,
              iTheta = iTheta
              ))
}

