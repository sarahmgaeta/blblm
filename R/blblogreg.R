#' @import purrr
#' @import furrr
#' @import future
#' @import stats
#' @importFrom magrittr %>%
#' @details
#' Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' @title Bag of Little Bootstraps Logistic Regression Model
#' @description Fits a logistic regression model on a given data set using the bag of little bootstraps algorithm.
#'
#' @param formula A symbolic description of the logistic regression model to be fitted.
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param m Number indicating how many sub-samples the data will be split into.
#' @param B Number of bootstrap samples.
#'
#' @return Coefficients of each fit.
#'
#' @export
blblogreg <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' @title Bag of Little Bootstraps Logistic Regression Model with Parallelization
#' @description Fits a logistic regression model on a given data set using the bag of little bootstraps algorithm and parallelization for efficiency.
#'
#' @param formula A symbolic description of the logistic regression model to be fitted.
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param m Number indicating how many sub-samples the data will be split into.
#' @param B Number of bootstrap samples.
#' @param w Number of workers.
#'
#' @return Coefficients of each fit.
#'
#' @export
future_blblogreg <- function(formula, data, m = 10, B = 5000, w = 4) {
  library(furrr)
  suppressWarnings(plan(multiprocess, workers = w))
  options(future.rng.onMisuse = "ignore")

  data_list <- split_data(data, m)

  estimates <- future_map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
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

#' @title Logistic Regression Model for Each Sub-sample
#' @description Fits a logistic regression model on each sub-sample and returns the model coefficients
#'
#' @param formula A symbolic description of the logistic regression model to be fitted.
#' @param data A data set, such as a data frame or list, containing the variables in the model.
#' @param n Number of rows of the given data set.
#' @param B Number of bootstrap samples.
#'
#' @return Model coefficients.
#'
#' compute the estimates
glm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, glm1(X, y, n), simplify = FALSE)
}

#' @title Linear Model for Each Bag of Little Bootstrap Data Set
#' @description Computes the regression estimates for a bag of little bootstraps data set.
#'
#' @param X Model matrix.
#' @param y Model response.
#' @param n Number of rows of the given data set.
#'
#' @return List of coefficients.
#'
#' compute the regression estimates for a blb dataset
glm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- glm.fit(X, y, weights = freqs, family = binomial())
  list(coef = blbcoef(fit))
}

#' @title Bag of Little Bootstraps Coefficients
#' @description Extracts the coefficients of a given logistic regression model.
#'
#' @param fit Fitted logistic regression model.
#'
#' @return Coefficients from the given fit.
#'
#' compute the coefficients from fit
blbcoef <- function(fit) {
  coef(fit)
}

#' @title Print Bag of Little Bootstraps Logistic Regression Model
#' @description Prints the formula used to create the given fitted blblogreg model.
#'
#' @param x Fitted blblogreg model.
#' @param ... Additional arguments.
#'
#' @return Formula used to create the given fitted blblogreg model.
#'
#' @export
#' @method print blblogreg
print.blblogreg <- function(x, ...) {
  cat("blblogreg model:", capture.output(x$formula))
  cat("\n")
}

#' @title Coefficients of blblogreg Model
#' @description Estimates the coefficients for a fitted blblogreg model.
#'
#' @param object Fitted blblogreg model.
#' @param ... Additional arguments.
#'
#' @return Estimated coefficients of the given fitted blblogreg model.
#'
#' @export
#' @method coef blblogreg
coef.blblogreg <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @title Confidence Interval for blblogreg Model Estimates
#' @description Computes confidence interval(s) for model estimate(s) or given parameter(s) at a specified confidence level.
#'
#' @param object Fitted blblogreg model.
#' @param parm Parameters to calculate confidence intervals for.
#' @param level Confidence level.
#' @param ... Additional arguments.
#'
#' @return Confidence interval(s) for model estimate(s) or given parameter(s) at a specified confidence level.
#'
#' @export
#' @method confint blblogreg
confint.blblogreg <- function(object, parm = NULL, level = 0.95, ...) {
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

#' @title Predict New Values with blblogreg Model
#' @description Predicts new values given new data.
#'
#' @param object Fitted blblogreg model.
#' @param parm Parameters to be estimated with confidence intervals.
#' @param level Confidence level.
#' @param ... Additional arguments.
#'
#' @return New, predicted values based on given new data.
#'
#' @export
#' @method predict blblogreg
predict.blblogreg <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
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
