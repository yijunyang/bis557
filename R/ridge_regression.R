# Define a function for making model matrices
#' @title Make Model Matrices
#' @author Yijun Yang
#' @description Make a model matrix easily.
#' @param form a formula
#' @param d a dataframe
#' @param contrasts contrasts argument
#' @return X and Y for the model
#' @export

make_model_matrices <- function(form, d, contrasts = NULL){
  d_no_na <- model.frame(form, d)
  X <- model.matrix(form, d_no_na, contrasts.arg = contrasts)
  y_name <- as.character(form)[2]
  Y <- matrix(d_no_na[, y_name], ncol = 1)
  list(X=X, Y=Y)
}

#' @title Ridge Regression
#' @author Yijun Yang
#' @description Implement a ridge regression function taking into account colinear (or nearly colinear) regression variables.
#' @param form a formula
#' @param d a dataframe
#' @param contrasts contrasts argument
#' @param lambda ridge penalty term
#' @return a list of beta coefficients
#' @examples
#' \dontrun{
#' ridge_regression(body_mass_g ~ bill_depth_mm, penguins)
#' }
#' @export

ridge_regression <- function(form, d, contrasts = NULL, lambda = 0.1) {

  # define a function to make model matrices

  # extract X and Y
  mms <- make_model_matrices(form, d, contrasts)
  X <- mms$X
  Y <- mms$Y

  # use singular value decomposition to calculate coefficients
  svd_x <- svd(X)
  # beta = V * (Sigma + lambda I) ^{-1} Sigma U^T Y
  ### Sigma <- diag(svd_x$d)
  ### lambda_I <- diag(rep(lambda, length(svd_x$d)))
  d <- svd_x$d
  beta <- svd_x$v %*% diag(d/(d^2 + lambda)) %*% t(svd_x$u) %*% Y

  # return the results
  ret <- list(coef = beta, form = form)
  class(ret) <- "ridge_regression"
  ret
}

#' @title Prediction for Ridge Regression
#' @author Yijun Yang
#' @description Return the predicition of Y given X via ridge regression.
#' @param object ridge_regression object
#' @param ... `(dataframe)`
#' @return estimated Y given X
#' @export

predict.ridge_regression <- function(object, ...){
  dots <- list(...)
  d <- dots[[1]]
  if (!inherits(d, "data.frame")){
    stop("Second argument must be a data frame.")
  }

  # extract X
  mms <- make_model_matrices(object$form, d)
  X <- mms$X

  X %*% object$coef
}
