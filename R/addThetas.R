addThetas <- function(thetas = thetas, iTheta = iTheta, keep = keep) {

  totalTheta <- vector(mode = "list", length = length(iTheta$commonEffect))
  names(totalTheta) <- colnames(iTheta$indEffect)

  for (i in seq_along(totalTheta)) {
    totalTheta[[i]] <- thetas[keep, iTheta$commonEffect[i]] + thetas[keep, iTheta$indEffect[, i]]
  }

  return(totalTheta)
}
