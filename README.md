# blblm package

<!-- badges: start -->
<!-- badges: end -->

# Bag of Little Bootstraps
This R package implements the Bag of Little Bootstraps Algorithm for both linear and logistic regression models. 

This package estimates the following summary statistics: coefficients, sigma values (for linear regression), predicted values, and confidence intervals for linear and/or logistic regressions using the Bag of Little Bootstraps Algorithm. 

To increase efficiency, users have the option of using parallelization depending on which function is used to create the regression model.

Please look at the vignettes for details on the Bag of Little Bootstraps Algorithm and the functions made available to users from this package.

# Installation

One way to install the "blblm" package is to use

``` r
devtools::install_github("ucdavis-sta141c-2021-winter/blblm")
```

## Examples

``` r
library(blblm)
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
fit <- future_blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
fit <- future_blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, w = 6)
coef(fit)
#> (Intercept)          wt          hp       wt:hp 
#> 48.88428523 -7.88702986 -0.11576659  0.02600976
confint(fit, c("wt", "hp"))
#>           2.5%       97.5%
#> wt -10.7902240 -5.61586271
#> hp  -0.1960903 -0.07049867
sigma(fit)
#> [1] 1.838911
sigma(fit, confidence = TRUE)
#>    sigma      lwr      upr 
#> 1.838911 1.350269 2.276347
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#>        1        2 
#> 21.55538 18.80785
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#>        fit      lwr      upr
#> 1 21.55538 20.02457 22.48764
#> 2 18.80785 17.50654 19.71772
fit <- blblogreg(am ~ mpg, data = mtcars, m = 3, B = 100)
fit <- future_blblogreg(am ~ mpg, data = mtcars, m = 3, B = 100)
fit <- future_blblogreg(am ~ mpg, data = mtcars, m = 3, B = 100, w = 6)
coef(fit)
#> (Intercept)         mpg 
#> -17.4902027   0.8125803 
confint(fit, "mpg")
#>          2.5%    97.5%
#> mpg 0.1854961 10.04173
predict(fit, data.frame(mpg = 15.3, 17.8))
#>         1 
#> -6.662252 
predict(fit, data.frame(mpg = 15.3, 17.8), confidence = TRUE)
#>         fit       lwr       upr
#> 1 -6.662252 -40.05448 -1.616695
```
