test_that("sigma output is as expected", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  # Expect sigma to be computed the same regardless of confidence = T or F
  expect_equal(sigma(fit), sigma(fit, confidence = TRUE)[[1]])

  # Expect sigma to return a singular value if confidence = FALSE
  expect_equal(length(sigma(fit)), 1)
  # Expect sigma to return a list of 3 values if confidence = TRUE
  expect_equal(length(sigma(fit, confidence = TRUE)), 3)

  # Expect sigma outputs to be numeric
  expect_equal(class(sigma(fit)), "numeric")
  expect_equal(class(sigma(fit, confidence = TRUE)), "numeric")
})
