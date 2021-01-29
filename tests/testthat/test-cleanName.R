context("Clean name")
library(quid)

test_that("weird input gets cleaned", {
  weird_input <- c("cOnDition", "condition-1", "condition_a", "condition.c", "CoNnnDition")

  expect_equal(cleanName(weird_input),
               c("condition", "condition_1", "condition_a", "condition_c", "connndition"))
})
