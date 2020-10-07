library(testthat)
library(MASS)

context("Test the output of homework 2 - ridge regression function.")

test_that("Your ridge_regression() function works in an easy case.", {

  data(iris)

  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris, lambda = 0.01)

  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, lambda = 0.01)

  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression$coef,
                    tolerance = 0.1)
})

test_that("Your ridge_regression() function works with contrasts.", {

  data(iris)

  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris,
                           contrasts = list(Species = "contr.sum"))

  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression$coef,
                    tolerance = 0.1)
})

test_that("Your ridge_regression() function works in a tougher case.", {

  data(lm_patho)

  fit_lm_ridge <- lm.ridge(y ~., lm_patho)

  fit_ridge_regression <- ridge_regression(y ~., lm_patho)

  expect_equivalent(length(fit_ridge_regression$coef), 3)

})
