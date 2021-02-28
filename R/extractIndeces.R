extractIndeces <- function(constraints, thetas, ID, data, formula, IDorg) {
  effectName <- unique(constraints$constraintEffect)
  effectName <- cleanName(effectName)

  # get all unique values of relevant factors
  colnames(data) <- cleanName(colnames(data))

  effectLevels <- as.factor(sort(unique(c(constraints$constraintUpper, constraints$constraintLower))))
  IDLevels <- unique(do.call(`$`, args = list(x = data, name = ID)))

  # common effect
  regexTheta0 <- paste0("^", effectName, "_", effectLevels, "$")
  iTheta0 <- sapply(regexTheta0, function(pat) grep(pattern = pat, x = colnames(thetas)))
  names(iTheta0) <- paste0(effectName, "_", effectLevels)

  # individual effects
  regexThetaID <- crossRegex(IDLevels = IDLevels, effectLevels = effectLevels, ID = ID, effectName = effectName, formula = formula)
  iThetaID <- apply(regexThetaID, MARGIN = c(1, 2), function(pat) grep(pattern = pat, x = colnames(thetas)))

  return(list(commonEffect = iTheta0,
              indEffect = iThetaID,
              IDLevels = IDLevels,
              effectLevels = effectLevels,
              ID = IDorg
              ))
}
