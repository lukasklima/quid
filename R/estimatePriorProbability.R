estimatePriorProbability <- function(iTheta = iTheta,
                                     rscaleEffects = rscaleEffects,
                                     iterationsPrior = iterationsPrior,
                                     cleanConstraints = cleanConstraints,
                                     IDorg = IDorg,
                                     effectNameOrg = effectNameOrg) {
  # get parameterization
  nLevels <- length(iTheta[["effectLevels"]])
  I <- length(iTheta[["IDLevels"]])
  regexEffect <- paste0("^", effectNameOrg, ":", IDorg, "$", "|", "^", IDorg, ":", effectNameOrg, "$")
  indEffect <- grep(regexEffect, names(rscaleEffects))

  # nLevels is projected onto n-1 levels by use of the following projection matrix
  params <- fixedFromRandomProjection(nLevels, sparse = FALSE)

  # sample n main effects m times
  mus <- matrix(nrow = iterationsPrior, ncol = ncol(params))
  for(i in 1:ncol(mus)) {
    mus[, i] <- rcauchy(iterationsPrior, 0, rscaleEffects[effectNameOrg])
  }

  # sample SDs for individual effects
  gID <- MCMCpack::rinvgamma(iterationsPrior, .5, .5 * rscaleEffects[indEffect])

  # set up for loop to estimate how often effects are in the expected direction
  pass <- 1:iterationsPrior

  # sample n individual effects m times
  for (m in 1:iterationsPrior){
    rEffects <- matrix(nrow = I, ncol = ncol(params))

    for (n in 1:ncol(rEffects)) {
      rEffects[, n] <- rnorm(I, 0, sqrt(gID[m]))
    }

    # make empty list of lenght n
    totalPrior <- vector(mode = "list", length = nLevels)
    names(totalPrior) <- colnames(iTheta$indEffect)

    # multiply effects with parameterization
    for (i in seq_along(totalPrior)) {
      totalPrior[[i]] <- sum(mus[m, ] * params[i, ]) +
        rEffects %*% params[i, ]
    }

    # estimate how often effects are in the expected direction
    constrainedPrior <- estimateConstrainedThetas(totalThetas = totalPrior, cleanConstraints = cleanConstraints)
    pass[m] <- sum(constrainedPrior) == I
  }
  return(pass)
}

# BayesFactor:::fixedFromRandomProjection
#' @importFrom Matrix Matrix
#'
fixedFromRandomProjection <- function (nlevRandom, sparse = FALSE) {
  centering = diag(nlevRandom) - (1/nlevRandom)
  S = as.vector((eigen(centering)$vectors)[, 1:(nlevRandom - 1)])
  return(Matrix::Matrix(S, nrow = nlevRandom, sparse = sparse))
}
