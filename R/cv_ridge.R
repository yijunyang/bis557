#' @title Cross Validation for Ridge Regression
#' @author Yijun Yang
#' @description Optimize the ridge parameter.
#' @param form a formula
#' @param d a dataframe
#' @param folds number of folds
#' @param lambdas a vector of ridge penalty term
#' @return the best lambda for ridge regression and a plot
#' @examples
#' \dontrun{
#' cv_ridge(body_mass_g ~ bill_depth_mm, penguins, folds = 5, lambdas = seq(1, 10, 1))
#' }
#' @export
#' @importFrom foreach %do% foreach
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_line
#' @importFrom rsample vfold_cv
#' @import purrr


cv_ridge <- function(form, d, folds = 10, lambdas){

  set.seed(557)

  # create folds
  folds <- vfold_cv(d, v = folds)
  # loop through different lambdas and folds
  resids <- foreach(lambda = lambdas) %do% {
    foreach(fold = folds$splits, .combine = c) %do% {

      fit <- ridge_regression(form = form, d = analysis(fold), lambda = lambda)
      as.vector(assessment(fold)[,all.vars(form)[1]] - predict(fit, assessment(fold)))

    }
  }

  # create tibble of results
  rd <- tibble(lambda = lambdas,
               mse = map_dbl(resids, ~ mean(.x^2)))

  # draw a graph to intuitively show
  plot <- ggplot(rd, aes(x = lambda, y = mse)) + geom_line()

  # retrieve best lambda
  lambda_best <- rd$lambda[which.min(rd$mse)]

  ret <- list(lambda = lambda_best, plot = plot)
  class(ret) <- "cv_ridge"
  ret

}

