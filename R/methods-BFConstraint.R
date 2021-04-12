# Constructors
BFConstraint <- function(priorProbability,
                         posteriorProbability,
                         bayesFactor,
                         constraints,
                         cleanConstraints) {
  methods::new("BFConstraint",
               priorProbability = priorProbability,
               posteriorProbability = posteriorProbability,
               bayesFactor = bayesFactor,
               constraints = constraints,
               cleanConstraints = cleanConstraints
  )
}

BFBayesFactorConstraint <- function(generalTestObj,
                                    constraints,
                                    individualEffects,
                                    posteriorMean,
                                    posteriorSD,
                                    totalThetas,
                                    mcmcFull,
                                    designIndeces,
                                    observedEffects) {
  methods::new("BFBayesFactorConstraint",
               generalTestObj = generalTestObj,
               constraints = constraints,
               individualEffects = individualEffects,
               posteriorMean = posteriorMean,
               posteriorSD = posteriorSD,
               totalThetas = totalThetas,
               mcmcFull = mcmcFull,
               designIndeces = designIndeces,
               observedEffects = observedEffects
  )
}

#' @importFrom methods show slot
setMethod("show", "BFBayesFactorConstraint", function(object) {
  cat("\n")
  cat("Constraints analysis\n--------------\n")

  # pad bfs
  nms <- c("Bayes factor", "Posterior probability", "Prior probability")
  maxwidth <- max(nchar(nms))
  nms <- stringr::str_pad(nms, maxwidth, side="right", pad=" ")
  values <- c(object@constraints@bayesFactor, object@constraints@posteriorProbability, object@constraints@priorProbability)

  for (b in seq_along(nms)) {
    cat(nms[b], " : ", values[b], "\n", sep = "")
  }

  # pad model names
  ef <- object@constraints@constraints[["constraintEffect"]]
  consLow <- object@constraints@constraints[["constraintLower"]]
  consUp <- object@constraints@constraints[["constraintUpper"]]
  maxwidth <- max(nchar(consLow))
  consLow <- stringr::str_pad(consLow, maxwidth, side="right", pad=" ")

  cat("\nConstraints defined: \n")
  for(i in 1:nrow(object@constraints@constraints)) {
    cat(" ", ef[i], " : ", consLow[i], " < ", consUp[i], "\n", sep = "")
  }

  cat("\n=========================\n\n")
  show(object@generalTestObj)
})
