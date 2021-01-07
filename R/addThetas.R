addThetas <- function(thetas = thetas, iTheta = iTheta, keep = keep) {

  totalTheta <- vector(mode = "list", length = length(iTheta$iTheta0))
  names(totalTheta) <- colnames(iTheta$iThetaID)

  for (i in seq_along(totalTheta)) {
    totalTheta[[i]] <- thetas[keep, iTheta$iTheta0[i]] + thetas[keep, iTheta$iThetaID[, i]]
  }

  return(totalTheta)
}
