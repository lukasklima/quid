extractIndexFullModel <- function(generalTestBFObj){
  numerators <- sapply(result@numerator, slot, "longName")
  numeratorsLengthFormulaNames <- sapply(numerators, function(x) length(all.names(as.formula(x))))
  which.max(numeratorsFormula)
}
