context("Constraint creation")
library(quid)

test_that("finds smaller or bigger", {
  entry <- c(condition = "small < bigger", condition = "small > smaller")

  expect_equal(createConstraints(entry),
               data.frame(constraintEffect = c("condition"),
                          constraintElement = c("small<bigger", "small>smaller"),
                          constraintUpper = c("bigger", "small"),
                          constraintLower = c("small", "smaller"),
                          row.names = c("condition", "condition1"))
  )

  constraints <- createConstraints(entry)

  expect_equal(createCleanConstraints(constraints),
               data.frame(upper = c("condition_bigger", "condition_small"),
                          lower = c("condition_small", "condition_smaller")))
})
