context("Extract indeces")
library(quid)

test_that("weird input extracts indeces", {
  constraints <- createConstraints(c(condition = "bigger > smaller"))
  ID <- "sUbJect"
  ID <- cleanName(ID)
  thetas <- matrix(ncol = 8)
  colnames(thetas) <- c("Condition.bigger", "COnDiTion-smaller",
                        "sUbJect.Condition.1.bigger", "sUbJect.Condition.2-bigger", "sUbJect-Condition.3.bigger",
                        "sUbJect-Condition.1-smaller", "sUbJect.Condition.2-smaller", "sUbJect-Condition.3.smaller")
  colnames(thetas) <- cleanName(colnames(thetas))
  data <- data.frame(sUbJect = 1:3)

  indEffect <- matrix(3:8, ncol = 2, byrow = FALSE)
  colnames(indEffect) <- c("condition_bigger", "condition_smaller")

  expect_equal(extractIndeces(constraints = constraints,
                              thetas = thetas, ID = ID,
                              data = data,
                              formula = rt ~ subject * condition,
                              IDorg = "subject"),
               list(commonEffect = c(condition_bigger = 1, condition_smaller = 2),
                    indEffect = indEffect,
                    IDLevels = 1:3,
                    effectLevels = as.factor(c("bigger", "smaller")),
                    ID = "subject")
  )
})
