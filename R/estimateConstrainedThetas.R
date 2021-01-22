estimateConstrainedThetas <- function(totalThetas = totalThetas, cleanConstraints = cleanConstraints) {
  X <- vector(mode = "list", length = nrow(cleanConstraints))
  # build up expression and then evaluate
  for (i in 1:nrow(cleanConstraints)) {
    X[[i]] <- rlang::expr(totalThetas[[!!cleanConstraints[i, "upper"]]] > totalThetas[[!!cleanConstraints[i, "lower"]]])
  }
  Y <- purrr::reduce(X, ~ rlang::expr(!!.x & !!.y))
  eval(Y)
}
