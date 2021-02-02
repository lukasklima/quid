constraintBF <- function(formula, data, whichRandom, ID,
                        whichConstraint, rscaleEffects,
                        iterationsPosterior = 10000, iterationsPrior = iterationsPosterior * 10,
                        burnin = 1000, ...) {

  ## ASSERTIONS
  ellipsis::check_dots_used()
  if (any(grepl("[[:punct:]]", colnames(data)))) {
    stop("Data column names must not contain any special characters.", call. = FALSE)
  }

  if (any(attr(terms(formula, data = data), "order") > 2)) {
    stop("constraintBF currently only supports interactions of up to 2 terms.", call. = FALSE)
  }

  # ID
  checkmate::assertCharacter(ID, len = 1)
  checkmate::assertChoice(ID, choices = colnames(data))
  checkmate::assertFactor(do.call(`$`, args = list(x = data, name = ID)), .var.name = "ID column in input data")


  # constraints
  if (length(unique(names(whichConstraint))) != 1) {
    stop("constraintBF() currently only supports testing constraints on 1 effect.",
         call. = FALSE)
  }
  if (length(whichConstraint) == 0 | is.null(names(whichConstraint))) {
    stop("constraintBF() can only be used with constraints defined. To perform
         multiple model comparison without constraints use BayesFactor::generalTestBF().",
         call. = FALSE)
  }
  checkmate::assertNames(names(whichConstraint), subset.of = colnames(data))
  checkmate::assertFactor(do.call(`$`,
                                  args = list(x = data, name = unique(names(whichConstraint)))),
                          .var.name = "constraint column in input data")
  if (!all((grepl("^[[:alnum:]]+<{1}[[:alnum:]]+$", gsub("\\s", "", whichConstraint)) | grepl("^[[:alnum:]]+>{1}[[:alnum:]]+$", gsub("\\s", "", whichConstraint))))) {
    stop('whichConstraint must be of the form:
         \n condition = "control < experimental"
         \n OR
         \n condition = "experimental > control"', call. = FALSE)
  }

  # priors
  checkmate::assertNames(names(rscaleEffects),
                         subset.of = attr(terms(formula, data = data), "term.labels"),
                         must.include = c(ID, unique(names(whichConstraint)), paste0(ID, ":", unique(names(whichConstraint)))))
  checkmate::assertNumeric(rscaleEffects, lower = 0)

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

