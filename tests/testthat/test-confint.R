test_that("confint output is as expected", {
  # Test confint works on blblm objects
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  # Expect 4 values: 2 lower bounds and 2 upper bounds
  expect_equal(length(confint(fit, c("wt", "hp"))), 4)

  # Expect outputs to be numeric
  expect_equal(class(confint(fit, c("wt", "hp"))[[1]]), "numeric")

  # Test confint works on blblogreg objects
  fit2 <- blblogreg(am ~ mpg, data = mtcars, m = 3, B = 100)

  # Expect 2 values: 1 lower bounds and 1 upper bounds
  expect_equal(length(confint(fit2, c("mpg"))), 2)

  # Expect outputs to be numeric
  expect_equal(class(confint(fit2, c("mpg"))[[1]]), "numeric")
})
