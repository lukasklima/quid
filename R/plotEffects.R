# plotting of observed vs estimated individual effects
#' Plot a BFBayesFactorConstraint object
#'
#' Plots observed vs estimated individual effects for each level of the effect
#'   for which constraints are defined.
#'
#' @param x an object of class \code{\link{BFBayesFactorConstraint-class}}.
#' @param .raw if \code{FALSE}, outputs the plot. If \code{TRUE} returns the
#'   \code{data.frame} that is used for making the plot.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object if \code{.raw = FALSE}. A
#'   \code{data.frame} otherwise, with columns for ID, type of effect,
#'   specified constraint, estimates, and ordering of estimate from smallest
#'   to largest.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(stroop)
#'
#' resStroop <- constraintBF(rtS ~ ID*cond,
#'                           data = stroop,
#'                           whichRandom = "ID",
#'                           ID = "ID",
#'                           whichConstraint = c(cond = "2 > 1"),
#'                           rscaleEffects = c("ID" = 1, "cond" = 1/6, "ID:cond" = 1/10))
#'
#' plotEffects(resStroop)
#' }
#'
plotEffects <- function(x, .raw = FALSE) {
  estimates <- calculateDifferences(x, effect = "estimate")
  observed <- calculateDifferences(x, effect = "observed")

  observed <- observed %>%
    dplyr::arrange(.data$differences, .data$estimate) %>%
    dplyr::mutate(ordering = rep(1:length(levels(.data$individual)), length.out = length(observed$differences)))

  estimates <- observed %>%
    dplyr::select(.data$individual, .data$differences, .data$ordering) %>%
    dplyr::inner_join(estimates, by = c("individual", "differences"))

  Y <- dplyr::bind_rows(observed, estimates) %>%
    dplyr::mutate(effect = factor(.data$effect, levels = c("observed", "estimate"), labels = c("Observed effect", "Model estimates"))) %>%
    dplyr::mutate(differences = factor(.data$differences))

  if(.raw) {
    return(Y)
  }

  ggplot2::ggplot(data = Y, mapping = ggplot2::aes(x = .data$ordering, y = .data$estimate)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$estimate > 0), show.legend = FALSE) +
    ggplot2::facet_grid(.data$differences ~ .data$effect) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::scale_colour_manual(values = c("FALSE" = "#FC4E07", "TRUE" = "#00AFBB")) +
    ggplot2::scale_x_continuous(name = "Individual", breaks = c(min(Y$ordering), max(Y$ordering))) +
    ggplot2::ylab("Effect") +
    ggplot2::theme_light() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
}

#' Calculate differences between conditions specified in constraints
#'
#' Calculates the differences between the conditions specified in the constraints
#'   of a \code{BFBayesFactorConstraint} object for each individual.
#'
#' @param x an object of class \code{\link{BFBayesFactorConstraint-class}}.
#' @param effect the effect differences to be calculated. \code{effect = "estimate"}
#'   computes the effect differences from the model estimates;
#'   \code{effect = "observed"} computes the observed effect differences.
#'
#' @return \code{calculateDifferences} returns an object of class
#'   \code{\link[tibble]{tbl_df}}, with columns for ID, type of effect,
#'   specified constraint, and estimates.
#' @export
#'
#' @examples
#' \dontrun{
#' data(stroop)
#'
#' resStroop <- constraintBF(rtS ~ ID*cond,
#'                           data = stroop,
#'                           whichRandom = "ID",
#'                           ID = "ID",
#'                           whichConstraint = c(cond = "2 > 1"),
#'                           rscaleEffects = c("ID" = 1, "cond" = 1/6, "ID:cond" = 1/10))
#'
#' calculateDifferences(resStroop, effect = "estimate")
#' calculateDifferences(resStroop, effect = "observed")
#' }
#'
calculateDifferences <- function(x, effect = c("estimate", "observed")) {
  checkmate::assertChoice(effect, choices = c("estimate", "observed"))

  if (effect == "estimate") {
    estimates <- dplyr::as_tibble(x@individualEffects)
  } else if (effect == "observed") {
    estimates <- dplyr::as_tibble(x@observedEffects)
  }
  Y <- vector(mode = "list", length = nrow(x@constraints@constraints))
  names(Y) <- x@constraints@constraints[["constraintElement"]]

  for (i in seq_along(Y)) {
    Y[[i]] <- estimates[, colnames(estimates) == x@constraints@cleanConstraints$upper[i]] - estimates[, colnames(estimates) == x@constraints@cleanConstraints$lower[i]]
  }
  Y <- as.data.frame(Y)
  colnames(Y) <- x@constraints@constraints[["constraintElement"]]
  Y <- dplyr::as_tibble(Y) %>%
    tibble::add_column(individual = x@designIndeces[["IDLevels"]], effect = effect) %>%
    tidyr::pivot_longer(cols = 1:tidyselect::last_col(2), names_to = "differences", values_to = "estimate") %>%
    dplyr::arrange(.data$differences, .data$individual)

  return(Y)
}
