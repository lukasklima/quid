# Classes
setClass("BFConstraint",
         slots = c(
           priorProbability = "numeric",
           posteriorProbability = "numeric",
           bayesFactor = "numeric",
           constraints = "data.frame",
           cleanConstraints = "data.frame")
)

setClass("BFBayesFactorConstraint",
         slots = c(
           generalTestObj = "BFBayesFactor",
           constraints = "BFConstraint",
           individualEffects = "list",
           posteriorMean = "numeric",
           posteriorSD = "numeric",
           totalThetas = "list",
           mcmcFull = "matrix",
           designIndeces = "list")
)
