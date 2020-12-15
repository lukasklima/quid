extractIndexFullModel <- function(generalTestBFObj) {
  numerators <- sapply(generalTestBFObj@numerator, slot, "longName")
  numerators <- sapply(numerators, as.formula)
  numerators <- sapply(numerators, function(x) length(attr(terms(x), "term.labels")))
  which.max(numerators)
}
