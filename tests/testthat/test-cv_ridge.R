library(testthat)
library(glmnet)

context("Test the output of homework 2 - ridge regression with cross validation.")

test_that("Your ridge_regression() function works in an easy case.", {

  data(iris)

  lambdas_choice <- seq(0,1,0.01)

  fit_cv_ridge <- suppressWarnings(cv_ridge(Sepal.Length ~ ., iris, folds = 5, lambdas = lambdas_choice))

  fit_glmnet <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris), as.matrix(iris[,1]), lambda = lambdas_choice, alpha = 0)

  expect_equivalent(fit_cv_ridge$lambda, fit_glmnet$lambda.min,
                    tolerance = 0.1)
})

