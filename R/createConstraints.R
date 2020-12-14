createConstraints <- function(whichConstraint = whichConstraint) {
  constraintEffects <- names(whichConstraint) # name of effect
  constraintElement <- gsub("\\s", "", whichConstraint[1]) # extract element and remove whitespaces

  if (grepl("<", constraintElement, fixed = TRUE)) { # condition where effect is expected to be bigger
    constraintUpper <- sub(".*<", "", constraintElement)
  } else if (grepl(">", constraintElement, fixed = TRUE)) {
    constraintUpper <- sub(">.*", "", constraintElement)
  }

  return(list(constraintEffects = constraintEffects,
              constraintElement = constraintElement,
              constraintUpper = constraintUpper))
}
