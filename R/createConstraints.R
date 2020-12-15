createConstraints <- function(whichConstraint = whichConstraint) {
  if (length(whichConstraint) == 0) {
    stop("constraintBF can only be used with constraints defined. To perform
         multiple model comparison without constraints use BayesFactor::generalTestBF().")
  } else if (length(whichConstraint) == 1) {
    createConstraint(whichConstraint = whichConstraint)
  } else if (length(whichConstraint) > 1) {
    for (cons in seq_along(whichConstraint)) {
      constraint <- createConstraint(whichConstraint[cons])
      constraints <- rbind(constraints, constraint)
    }
    return(constraints)
  }
}

createConstraint <- function(whichConstraint = whichConstraint) {
  constraintEffect <- names(whichConstraint) # name of effect
  constraintElement <- gsub("\\s", "", whichConstraint[1]) # extract element and remove whitespaces

  if (grepl("<", constraintElement, fixed = TRUE)) { # condition where effect is expected to be bigger
    constraintUpper <- sub(".*<", "", constraintElement)
  } else if (grepl(">", constraintElement, fixed = TRUE)) {
    constraintUpper <- sub(">.*", "", constraintElement)
  }

  return(data.frame(constraintEffect = constraintEffect,
                    constraintElement = constraintElement,
                    constraintUpper = constraintUpper))
}
