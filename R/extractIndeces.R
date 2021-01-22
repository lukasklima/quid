extractIndeces <- function(constraints = constraints, thetas = thetas, ID = ID, data = data) {
  effectName <- unique(constraints$constraintEffect)
  effectName <- cleanName(effectName)

  if (length(effectName) > 1) {
    stop("constraintBF currently only supports testing constraints of 1 effect")
  }

  # get all unique values of relevant factors
  colnames(data) <- cleanName(colnames(data))

  effectLevels <- sort(unique(c(constraints$constraintUpper, constraints$constraintLower)))
  IDLevels <- unique(do.call(`$`, args = list(x = data, name = ID)))

  # common effect
  regexTheta0 <- paste0("^", effectName, "_", effectLevels, "$")
  iTheta0 <- sapply(regexTheta0, function(pat) grep(pattern = pat, x = colnames(thetas)))
  names(iTheta0) <- paste0(effectName, "_", effectLevels)

  # individual effects
  regexThetaID <- crossRegex(IDLevels = IDLevels, effectLevels = effectLevels, ID = ID, effectName = effectName)
  iThetaID <- apply(regexThetaID, MARGIN = c(1, 2), function(pat) grep(pattern = pat, x = colnames(thetas)))

  return(list(commonEffect = iTheta0,
              indEffect = iThetaID,
              IDLevels = IDLevels,
              effectLevels = effectLevels
              ))
}
