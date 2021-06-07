
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sppois

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/gregmacfarlane/sppois.svg?branch=master)](https://travis-ci.com/gregmacfarlane/sppois)
[![R-CMD-check](https://github.com/gregmacfarlane/sppois/workflows/R-CMD-check/badge.svg)](https://github.com/gregmacfarlane/sppois/actions)
<!-- badges: end -->

The goal of sppois is to implement the two stage estimator for spatial
autoregressive Poisson models proposed by [Lambert et
al. (2010)](http://dx.doi.org/10.1016/j.regsciurbeco.2010.04.001)

> This package does not currently implement the Lambert methods.
> Contributions welcome.

## Installation

sppois is not on CRAN. When it is, you can install the released version
of sppois from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("sppois")
```

For now, you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gregmacfarlane/sppois")
```

## Example

The package allows users to estimate a full-information maximum
likelihood estimation of spatially-autogressive poisson count models.

``` r
library(sppois)
crime_pois   <- sarpoisson(crime_i ~ income + home_value, data = columbus_crime,
                           listw = columbus_neighbors, method = "non-spatial") 
#> Warning in stats::nlm(f = pois.loglik, p = rep(0, ncol(X)), y = y, X = X, : NA/
#> Inf replaced by maximum positive value

#> Warning in stats::nlm(f = pois.loglik, p = rep(0, ncol(X)), y = y, X = X, : NA/
#> Inf replaced by maximum positive value

#> Warning in stats::nlm(f = pois.loglik, p = rep(0, ncol(X)), y = y, X = X, : NA/
#> Inf replaced by maximum positive value
crime_sppois <- sarpoisson(crime_i ~ income + home_value, data = columbus_crime,
                           listw = columbus_neighbors, method = "fiml") 
#> Warning in sarpoisson(crime_i ~ income + home_value, data = columbus_crime, :
#> Matrix listw is near-singular, results may not be reliable.
#> Warning in stats::nlm(f = sarpois.filoglik, p = c(0, rep(0, ncol(X))), y = y, :
#> NA/Inf replaced by maximum positive value

#> Warning in stats::nlm(f = sarpois.filoglik, p = c(0, rep(0, ncol(X))), y = y, :
#> NA/Inf replaced by maximum positive value

#> Warning in stats::nlm(f = sarpois.filoglik, p = c(0, rep(0, ncol(X))), y = y, :
#> NA/Inf replaced by maximum positive value
summary(crime_sppois)
#> 
#> Call:
#> sarpoisson(formula = crime_i ~ income + home_value, data = columbus_crime, 
#>     listw = columbus_neighbors, method = "fiml")
#> 
#> Residuals:
#>         Min          1Q      Median          3Q         Max 
#> -3.53503130 -0.54293339  0.09595406  0.72296545  2.87309818 
#> 
#> Coefficients: 
#>             Estimate Std. Error t value Pr(>|t|)   
#> rho          0.35221    0.29727    1.18    0.242   
#> (Intercept)  1.65351    0.60572    2.73    0.009 **
#> income      -0.04087    0.01867   -2.19    0.034 * 
#> home_value  -0.00759    0.00501   -1.51    0.137   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Log likelihood: -83.16539 for fiml model
```
