% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{future_blblm}
\alias{future_blblm}
\title{Bag of Little Bootstraps Linear Model with Parallelization}
\usage{
future_blblm(formula, data, m = 10, B = 5000, w = 4)
}
\arguments{
\item{formula}{A symbolic description of the linear regression model to be fitted.}

\item{data}{A data set, such as a data frame or list, containing the variables in the model.}

\item{m}{Number indicating how many sub-samples the data will be split into.}

\item{B}{Number of bootstrap samples.}

\item{w}{Number of workers.}
}
\value{
Coefficients of each fit.
}
\description{
Fits a linear model on a given data set using the bag of little bootstraps algorithm and parallelization for efficiency.
}
