#' Title
#'
#' @param formula
#' @param data
#' @param whichRandom
#' @param ID
#' @param whichConstraint
#' @param rscaleEffects
#' @param iterationsPosterior
#' @param iterationsPrior
#' @param burnin
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
  checkIterations(iterationsPosterior = iterationsPosterior, burnin = burnin)

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
  iTheta <- extractIndeces(constraints = constraints,
                           thetas = thetas,
                           ID = ID,
                           data = data,
                           formula = formula,
                           IDorg = IDorg)

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
  posteriorSD <- sapply(individualEffects, sd)
  posteriorMean <- colMeans(thetas[keep, iTheta$commonEffect])
  observedEffects <- calculateObservedEffects(constraints = constraints,
                                              data = data,
                                              IDorg = IDorg,
                                              iTheta = iTheta,
                                              formula = formula,
                                              effectNameOrg = effectNameOrg)

  # make S4 objects
  newConstraint <- BFConstraint(priorProbability = priorProbability,
                                posteriorProbability = posteriorProbability,
                                bayesFactor = bfCU,
                                constraints = constraints,
                                cleanConstraints = cleanConstraints)


  newBFConstraint <- BFBayesFactorConstraint(generalTestObj = generalTestObj,
                                             constraints = newConstraint,
                                             individualEffects = individualEffects,
                                             posteriorMean = posteriorMean,
                                             posteriorSD = posteriorSD,
                                             totalThetas = totalThetas,
                                             mcmcFull = thetas[keep, ],
                                             designIndeces = iTheta,
                                             observedEffects = observedEffects)

  return(newBFConstraint)
}
