test_that("future_blblm output is as expected", {

  # From given examples
  fit <- future_blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  # We can't test specific values, but we can check the output.

  # In this example, we expect 4 estimates
  expect_equal(length(coef(fit)), 4)

  # Additionally, we expect the estimates to be numeric (a numeric list specifically)
  expect_equal(class(coef(fit)), "numeric")
})