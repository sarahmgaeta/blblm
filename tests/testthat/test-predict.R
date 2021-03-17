test_that("predict output is as expected", {

  # Test predict works on blblm objects
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  new_values1 <- predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
  new_values2 <- predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)

  # Expect the estimated new values to be the same regardless if confidence = TRUE or FALSE
  expect_equal(new_values1[[1]], new_values2[[1]])
  expect_equal(new_values1[[2]], new_values2[[2]])

  # Expect only 2 outputted estimates if confidence = FALSE
  expect_equal(length(new_values1), 2)
  # Expect 2 outputted estimates and their lower and upper bounds (6 total estimates) if confidence = TRUE
  expect_equal(length(new_values2), 6)

  # Expect all output to be numeric
  expect_equal(class(new_values1), "numeric")
  expect_equal(class(new_values2[[1]]), "numeric")

  # Test predict works on blblogreg objects
  fit2 <- blblogreg(am ~ mpg, data = mtcars, m = 3, B = 100)

  new_values12 <- predict(fit2, data.frame(mpg = 15.3, 17.8))
  new_values22 <- predict(fit2, data.frame(mpg = 15.3, 17.8), confidence = TRUE)

  # Expect the estimated new values to be the same regardless if confidence = TRUE or FALSE
  expect_equal(new_values12[[1]], new_values22[[1]])

  # Expect only 2 outputted estimates if confidence = FALSE
  expect_equal(length(new_values12), 1)
  # Expect 2 outputted estimates and their lower and upper bounds (6 total estimates) if confidence = TRUE
  expect_equal(length(new_values22), 3)

  # Expect all output to be numeric
  expect_equal(class(new_values12), "numeric")
  expect_equal(class(new_values22[[1]]), "numeric")
})
