crossRegex <- function(IDLevels = IDLevels, effectLevels = effectLevels, ID = ID, effectName = effectName) {
  suffix <- outer(IDLevels, effectLevels, FUN = paste, sep = "_")
  regexThetaID <- apply(suffix, MARGIN = c(1, 2), function(x) paste0("^", ID, "_", effectName, "_", x, "$"))
  regexThetaID <- as.data.frame(regexThetaID)
  colnames(regexThetaID) <- paste0(effectName, "_", effectLevels)
  return(regexThetaID)
}
