
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
crime_sppois <- sarpoisson(crime_i ~ income + home_value, data = columbus_crime,
                           listw = columbus_neighbors, method = "fiml") 
#> Warning in sarpoisson(crime_i ~ income + home_value, data = columbus_crime, :
#> Matrix listw is near-singular, results may not be reliable.
summary(crime_sppois)
#> 
#> Call:
#> sarpoisson(formula = crime_i ~ income + home_value, data = columbus_crime, 
#>     listw = columbus_neighbors, method = "fiml")
#> 
#> Residuals:
#>        Min         1Q     Median         3Q        Max 
#> -3.5366373 -0.5430774  0.0953256  0.7218270  2.8718411 
#> 
#> Coefficients: 
#>             Estimate Std. Error t value Pr(>|t|)  
#> rho          0.35302    0.31453    1.12    0.268  
#> (Intercept)  1.65185    0.64807    2.55    0.014 *
#> income      -0.04082    0.01914   -2.13    0.038 *
#> home_value  -0.00758    0.00515   -1.47    0.148  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Log likelihood: -83.16539 for fiml model
```
