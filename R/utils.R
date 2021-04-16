cleanName <- function(x) {
  janitor::make_clean_names(tolower(x))
}

forceDataFrame <- function(data) {
  if (inherits(data, 'tbl_df')) {
    data <- as.data.frame(data)
    warning('data coerced from tibble to data frame', call.=FALSE)
  }
  data
}
