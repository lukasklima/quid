# ASSERTIONS
checkFormulaData <- function(formula = formula, data = data) {
  if (any(grepl("[[:punct:]]", colnames(data)))) {
    stop("Data column names must not contain any special characters.",
         "\n Check columns: \n", paste0("\u2716 ", colnames(data)[grepl("[[:punct:]]", colnames(data))], "\n"),
         call. = FALSE)
  }
  if (any(attr(terms(formula, data = data), "order") > 2)) {
    stop("constraintBF currently only supports interactions of up to 2 terms.", call. = FALSE)
  }
  smallNames <- tolower(colnames(data))
  same <- duplicated(smallNames) | duplicated(smallNames, fromLast = TRUE)
  if (any(same)) {
    stop("Data column names are not case sensitive and must be unique values.",
         "\n Check columns: \n", paste0("\u2716 ", colnames(data)[same], "\n"))
  }
}

checkID <- function(ID = ID, data = data) {
  checkmate::assertCharacter(ID, len = 1)
  checkmate::assertChoice(ID, choices = colnames(data))
  checkmate::assertFactor(do.call(`$`, args = list(x = data, name = ID)), .var.name = "ID column in input data")
}

checkConstraints <- function(whichConstraint = whichConstraint, data = data) {
  if (length(unique(names(whichConstraint))) > 1) {
    stop("constraintBF() currently only supports testing constraints on 1 effect.",
         call. = FALSE)
  }
  if (length(whichConstraint) == 0 | is.null(names(whichConstraint))) {
    stop("constraintBF() can only be used with constraints defined. To perform
         multiple model comparison without constraints use BayesFactor::generalTestBF().",
         call. = FALSE)
  }
  checkmate::assertNames(names(whichConstraint), subset.of = colnames(data))
  checkmate::assertFactor(do.call(`$`,
                                  args = list(x = data, name = unique(names(whichConstraint)))),
                          .var.name = "constraint column in input data")
  if (!constraintsConform(whichConstraint = whichConstraint)) {
    stop('whichConstraint must be of the form:
         \n condition = "control < experimental"
         \n OR
         \n condition = "experimental > control"', call. = FALSE)
  }
}

constraintsConform <- function(whichConstraint = whichConstraint) {
  all((grepl("^[[:alnum:]]+<{1}[[:alnum:]]+$", gsub("\\s", "", whichConstraint)) | grepl("^[[:alnum:]]+>{1}[[:alnum:]]+$", gsub("\\s", "", whichConstraint))))
}

checkPriors <- function(rscaleEffects = rscaleEffects, formula = formula,
                        data = data, ID = ID, whichConstraint = whichConstraint) {
  interactionTerms <- c(paste0(ID, ":", unique(names(whichConstraint))), paste0(paste0(unique(names(whichConstraint)), ":", ID)))
  whichTerm <- interactionTerms %in% attr(terms(formula, data = data), "term.labels")
  interactionTerms <- interactionTerms[whichTerm]

  checkmate::assertNames(names(rscaleEffects),
                         subset.of = attr(terms(formula, data = data), "term.labels"),
                         must.include = c(ID, unique(names(whichConstraint)), interactionTerms))
  checkmate::assertNumeric(rscaleEffects, lower = 0)
}

checkIterations <- function(iterationsPosterior = iterationsPosterior, burnin = burnin) {
  if (burnin > iterationsPosterior) {
    stop(" 'iterationsPosterior' must be bigger than 'burnin'.
         \n \u2716 'iterationsPosterior' is ", iterationsPosterior,
         "\n \u2716 'burnin' is ", burnin, call. = FALSE)
  }
}

checkUsedLevels <- function(formula, data) {
  tt <- terms(formula)
  variables <- rownames(attr(tt, "factors"))
  idx <- colnames(data) %in% variables
  x <- data[idx]
  ix <- vapply(x, is.factor, NA)
  x <- x[ix]

  unusedLevels <- vapply(x, function(f) nlevels(f) != nlevels(droplevels(f)), NA)

  if(any(unusedLevels)) {
    data <- droplevels(data)
    warning("Dropped unused factor level(s) in variables: \n\n",
            paste0("\u2716 ", names(unusedLevels[which(unusedLevels)]), "\n"),
            call. = FALSE)
  }
  return(data)
}
