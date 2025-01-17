% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{lm1}
\alias{lm1}
\title{Linear Model for Each Bag of Little Bootstrap Data Set}
\usage{
lm1(X, y, n)
}
\arguments{
\item{X}{Model matrix.}

\item{y}{Model response.}

\item{n}{Number of rows of the given data set.}
}
\value{
List of coefficients and sigma.

compute the regression estimates for a blb dataset
}
\description{
Computes the regression estimates for a bag of little bootstraps data set.
}
