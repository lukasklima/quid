extractIndexFullModel <- function(generalTestBFObj){
  numerators <- sapply(generalTestBFObj@numerator, slot, "longName")
  numeratorsLengthFormulaNames <- sapply(numerators, function(x) length(all.names(as.formula(x))))
  which.max(numeratorsLengthFormulaNames)
}
