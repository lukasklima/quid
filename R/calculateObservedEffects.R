calculateObservedEffects <- function(constraints, data, IDorg, iTheta, formula, effectNameOrg) {

  obsEffects <- vector(mode = "list", length = length(iTheta$commonEffect))
  names(obsEffects) <- colnames(iTheta$indEffect)
  outcome <- formula[[2]]

  means <- tapply(do.call(`$`, args = list(x = data, name = outcome)),
                  list(do.call(`$`, args = list(x = data, name = effectNameOrg)),
                       do.call(`$`, args = list(x = data, name = IDorg))), mean)

  for(i in 1:length(obsEffects)) {
    obsEffects[[i]] <- means[i, ]
  }

  return(obsEffects)
}
