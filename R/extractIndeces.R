extractIndeces <- function(constraints = constraints, thetas = thetas, ID = ID, data = data) {
  effectName <- unique(constraints$constraintEffect)
  if (length(effectName) > 1) {
    stop("constraintBF currently only supports testing constraints of 1 effect")
  }

  # clean names
  colnames(thetas) <- janitor::make_clean_names(colnames(thetas))
  ID <- janitor::make_clean_names(ID)

  # get all unique values of relevant factors
  effectLevels <- sort(unique(c(constraints$constraintUpper, constraints$constraintLower)))
  IDLevels <- unique(do.call(`$`, args = list(x = data, name = ID)))

  # common effect
  regexTheta0 <- paste0("^", effectName, "_", effectLevels, "$")
  iTheta0 <- sapply(regexTheta0, function(pat) grep(pattern = pat, x = colnames(thetas)))

  # individual effects
  regexThetaID <- crossRegex(IDLevels = IDLevels, effectLevels = effectLevels, ID = ID, effectName = effectName)
  iThetaID <- apply(regexThetaID, MARGIN = c(1, 2), function(pat) grep(pattern = pat, x = colnames(thetas)))

  return(list(iTheta0 = iTheta0,
              iThetaID = iThetaID,
              IDLevels = IDLevels,
              effectLevels = effectLevels
              ))
}
