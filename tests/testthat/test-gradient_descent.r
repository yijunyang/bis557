library(testthat)

context("Test the output of homework 1 - gradient descent function.")

test_that("You gradient_descent() function works in an easy case.", {

  data(iris)

  fit_gradient_descent <- gradient_descent(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-2)
})

test_that("You gradient_descent() function works with contrasts.", {

  data(iris)

  fit_gradient_descent <- gradient_descent(Sepal.Length ~ ., iris,
                                   contrasts = list(Species = "contr.sum"))

  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-2)
})

test_that("Your gradient_descent() function works in a tougher case.", {

  data(lm_patho)

  fit_gradient_descent <- gradient_descent(y ~., lm_patho)

  fit_lm <- lm(y ~., lm_patho)

  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-2)
})

