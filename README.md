
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis557

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/yijunyang/bis557.svg?branch=master)](https://travis-ci.com/yijunyang/bis557)
[![Coveralls test
coverage](https://coveralls.io/repos/github/yijunyang/bis557/badge.svg)](https://coveralls.io/r/yijunyang/bis557?branch=master)
<!-- badges: end -->

This package contains course materials from Yale BIS557 Computational Statistics (2020 Fall).
The goal is to design and implement algorithms for statistical analyses, including regression models (ridge/lasso/multi-logistic), cross-validation, stochastic gradient descent, neural networks, etc. R and Python code are designed for the implementation.

## Installation

<!-- You can install the released version of bis557 from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("bis557")
```
-->

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yijunyang/bis557")
```

## Example

This is a basic example which shows you how to solve a numerically-stable
ridge regression that takes into account colinear regression variables:

``` r
# library(bis557)
data("iris")

# create a colinear term
iris$colinear <- 2*iris$Petal.Width + 0.1

# claim formula and dataset
form <- Sepal.Length ~ .
d <- iris

# make model matrices
mms <- make_model_matrices(form, d, contrasts = NULL)
X <- mms$X
Y <- mms$Y

# implement ridge regression
ridge_regression(form, d, lambda = 10))
```

<!--
What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!

-->
