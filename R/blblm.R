#' @import purrr
#' @import furrr
#' @import future
#' @import stats
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' @title Bag of Little Bootstraps Linear Model
#' @description Fits a linear model on a given data set using the bag of little bootstraps algorithm.
#'
#' @param formula A symbolic description of the linear regression model to be fitted.
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param m Number indicating how many sub-samples the data will be split into.
#' @param B Number of bootstrap samples.
#'
#' @return Coefficients of each fit.
#'
#' @export
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' @title Bag of Little Bootstraps Linear Model with Parallelization
#' @description Fits a linear model on a given data set using the bag of little bootstraps algorithm and parallelization for efficiency.
#'
#' @param formula A symbolic description of the linear regression model to be fitted.
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param m Number indicating how many sub-samples the data will be split into.
#' @param B Number of bootstrap samples.
#' @param w Number of workers.
#'
#' @return Coefficients of each fit.
#'
#' @export
future_blblm <- function(formula, data, m = 10, B = 5000, w = 4) {
  library(furrr)
  plan(multiprocess, workers = w)
  options(future.rng.onMisuse = "ignore")

  data_list <- split_data(data, m)

  estimates <- future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' @title Split Data
#' @description Splits the given data set into m sub-samples of approximated equal sizes.
#'
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param m Number indicating how many sub-samples the data will be split into.
#'
#' @return A split data set of m sub-samples of approximated equal sizes.
#'
#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' @title Linear Model for Each Sub-sample
#' @description Fits a linear model on each sub-sample and returns the model estimates (coefficients and sigma).
#'
#' @param formula A symbolic description of the linear regression model to be fitted.
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param n Number of rows of the given data set.
#' @param B Number of bootstrap samples.
#'
#' @return Model estimates (coefficients and sigma).
#'
#' compute the estimates
lm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, lm1(X, y, n), simplify = FALSE)
}


#' @title Linear Model for Each Bag of Little Bootstrap Data Set
#' @description Computes the regression estimates for a bag of little bootstraps data set.
#'
#' @param X Model matrix.
#' @param y Model response.
#' @param n Number of rows of the given data set.
#'
#' @return List of coefficients and sigma.
#'
#' compute the regression estimates for a blb dataset
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' @title Bag of Little Bootstraps Coefficients
#' @description Extracts the coefficients of a given linear model.
#'
#' @param fit Fitted linear model.
#'
#' @return Coefficients from the given fit.
#'
#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}


#' @title Bag of Little Bootstraps Sigma
#' @description Computes sigma of a given linear model.
#'
#' @param fit Fitted linear model.
#'
#' @return Sigma of the given fit.
#'
#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @title Print Bag of Little Bootstraps Linear Model
#' @description Prints the formula used to create the given fitted blblm model.
#'
#' @param x Fitted blblm model.
#' @param ... Additional arguments.
#'
#' @return Formula used to create the given fitted blblm model.
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @title Sigma of blblm Model
#' @description Estimates sigma and, optionally, the confidence interval for sigma for a fitted blblm model.
#'
#' @param object Fitted blblm model.
#' @param confidence Single logical indicating whether the output should contain the confidence interval.
#' @param level Confidence level.
#' @param ... Additional arguments.
#'
#' @return Estimated value for sigma of the given fitted blblm model and, optionally, the confidence interval for sigma at the specified confidence level.
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}


#' @title Coefficients of blblm Model
#' @description Estimates the coefficients for a fitted blblm model.
#'
#' @param object Fitted blblm model.
#' @param ... Additional arguments.
#'
#' @return Estimated coefficients of the given fitted blblm model.
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @title Confidence Interval for blblm Model Estimates
#' @description Computes confidence interval(s) for model estimate(s) or given parameter(s) at a specified confidence level.
#'
#' @param object Fitted blblm model.
#' @param parm Parameters to calculate confidence intervals for.
#' @param level Confidence level.
#' @param ... Additional arguments.
#'
#' @return Confidence interval(s) for model estimate(s) or given parameter(s) at a specified confidence level.
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}


#' @title Predict New Values with blblm Model
#' @description Predicts new values given new data.
#'
#' @param object Fitted blblm model.
#' @param new_data New data set to be used in prediction.
#' @param confidence Single logical indicating whether the output should contain the confidence interval.
#' @param level Confidence level.
#' @param ... Additional arguments.
#'
#' @return New, predicted values based on given new data.
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}


map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}


map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}


map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
