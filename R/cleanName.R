cleanName <- function(x) {
  janitor::make_clean_names(tolower(x))
}
