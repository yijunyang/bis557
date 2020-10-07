library(testthat)

context("Test the output of homework 2 - gradient descent function with out-of-sample accuracy")

test_that("You gradient_descent_new() function works in an easy case.", {

  data(iris)

  fit_new <- gradient_descent_new(Sepal.Length ~ ., iris)

  fit_old <- gradient_descent(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_old$coefficients, fit_new$coefficients,
                    tolerance = 0.1)
})

test_that("You gradient_descent_new() function works with contrasts.", {

  data(iris)

  fit_new <- gradient_descent_new(Sepal.Length ~ ., iris,
                                   contrasts = list(Species = "contr.sum"))

  fit_old <- gradient_descent(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_old$coefficients, fit_new$coefficients,
                    tolerance = 0.1)
})

test_that("Your gradient_descent() function works in a tougher case.", {

  data(lm_patho)

  fit_new <- gradient_descent_new(y ~., lm_patho)

  fit_old <- lm(y ~., lm_patho)

  expect_equivalent(fit_old$coefficients, fit_new$coefficients,
                    tolerance = 0.1)
})

