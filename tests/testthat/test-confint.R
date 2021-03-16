test_that("confint output is as expected", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  # Expect 4 values: 2 lower bounds and 2 upper bounds
  expect_equal(length(confint(fit, c("wt", "hp"))), 4)

  # Expect outputs to be numeric
  expect_equal(class(confint(fit, c("wt", "hp"))[[1]]), "numeric")
})