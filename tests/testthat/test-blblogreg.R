test_that("blblogreg outputs as expected", {
  fit <- blblogreg(am ~ mpg, data = mtcars, m = 3, B = 100)

  # Expect 2 estimates
  expect_equal(length(coef(fit)), 2)

  # Expect outputs to be numeric
  expect_equal(class(coef(fit)), "numeric")
})
