# plotting of observed vs estimated individual effects
plotEffects <- function(x, .raw = FALSE) {
  estimates <- calculateDifferences(x, effect = "estimate")
  observed <- calculateDifferences(x, effect = "observed")

  observed <- observed %>%
    dplyr::arrange(differences, estimate) %>%
    dplyr::mutate(ordering = rep(1:length(levels(individual)), length.out = length(observed$differences)))

  estimates <- observed %>%
    dplyr::select(individual, differences, ordering) %>%
    dplyr::inner_join(estimates, by = c("individual", "differences"))

  Y <- dplyr::bind_rows(observed, estimates) %>%
    dplyr::mutate(effect = factor(effect, levels = c("observed", "estimate"), labels = c("Observed effect", "Effect parameter"))) %>%
    dplyr::mutate(differences = factor(differences))

  if(.raw) {
    return(Y)
  }

  ggplot2::ggplot(data = Y, mapping = ggplot2::aes(x = ordering, y = estimate)) +
    ggplot2::geom_point(ggplot2::aes(colour = estimate > 0), show.legend = FALSE) +
    ggplot2::facet_grid(differences ~ effect) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::scale_colour_manual(values = c("#FC4E07", "#00AFBB")) +
    ggplot2::scale_x_continuous(name = "Individual", breaks = c(min(Y$ordering), max(Y$ordering))) +
    ggplot2::ylab("Effect") +
    ggplot2::theme_light() +
    ggplot2::theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

}

# Calculate differences between conditions specified in constraints
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
    tidyr::pivot_longer(cols = 1:last_col(2), names_to = "differences", values_to = "estimate") %>%
    dplyr::arrange(differences, individual)

  return(Y)
}



