createCleanConstraints <- function(constraints = constraints) {
  upper <- vector("character")
  lower <- vector("character")
  for (i in seq_along(constraints$constraintEffect)) {
    upper[i] <- paste(constraints$constraintEffect[i], constraints$constraintUpper[i], sep = "_")
    lower[i] <- paste(constraints$constraintEffect[i], constraints$constraintLower[i], sep = "_")
  }
  as.data.frame(cbind(upper, lower))
}
