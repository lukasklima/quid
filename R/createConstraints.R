createConstraints <- function(whichConstraint = whichConstraint) {
  if (length(whichConstraint) == 1) {
    createConstraint(whichConstraint = whichConstraint)
  } else if (length(whichConstraint) > 1) {
    constraints <- data.frame()
    for (cons in seq_along(whichConstraint)) {
      constraint <- createConstraint(whichConstraint[cons])
      constraints <- rbind(constraints, constraint)
    }
    return(constraints)
  }
}

createConstraint <- function(whichConstraint = whichConstraint) {
  constraintEffect <- names(whichConstraint) # name of effect
  constraintEffect <- cleanName(constraintEffect)
  constraintElement <- gsub("\\s", "", whichConstraint[1]) # extract element and remove white spaces

  if (grepl("<", constraintElement, fixed = TRUE)) { # condition where effect is expected to be bigger
    constraintUpper <- sub(".*<", "", constraintElement)
    constraintLower <- sub("<.*", "", constraintElement)
  } else if (grepl(">", constraintElement, fixed = TRUE)) {
    constraintUpper <- sub(">.*", "", constraintElement)
    constraintLower <- sub(".*>", "", constraintElement)
  }

  return(data.frame(constraintEffect = constraintEffect,
                    constraintElement = constraintElement,
                    constraintUpper = constraintUpper,
                    constraintLower = constraintLower))
}
