crossRegex <- function(IDLevels, effectLevels, ID, effectName, formula) {

  trms <- attr(terms(formula), "term.labels")
  trms <- cleanName(trms)
  idFirst <- paste0(ID, "_", effectName)
  effectFirst <- paste0(effectName, "_", ID)

  if (idFirst %in% trms) {
    suffix <- outer(IDLevels, effectLevels, FUN = paste, sep = "_")
    regexThetaID <- apply(suffix, MARGIN = c(1, 2), function(x) paste0("^", ID, "_", effectName, "_", x, "$"))
    regexThetaID <- as.data.frame(regexThetaID)
    colnames(regexThetaID) <- paste0(effectName, "_", effectLevels)
    return(regexThetaID)
  } else if (effectFirst %in% trms) {
    suffix <- outer(effectLevels, IDLevels, FUN = paste, sep = "_")
    suffix <- t(suffix)
    regexThetaID <- apply(suffix, MARGIN = c(1, 2), function(x) paste0("^", effectName, "_", ID, "_", x, "$"))
    regexThetaID <- as.data.frame(regexThetaID)
    colnames(regexThetaID) <- paste0(effectName, "_", effectLevels)
    return(regexThetaID)
  } else {
    stop("Unable to match formula elements with column names from posterior samples.")
  }
}
