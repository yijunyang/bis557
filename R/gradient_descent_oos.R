#' @title Gradient Descent with Out-of-sample Accuracy
#' @author Yijun Yang
#' @description Implement a new function fitting the OLS model using gradient descent that calculates the penalty based on the out-of-sample accuracy.
#' @param form A formula
#' @param d A dataframe
#' @param contrasts Contrasts argument
#' @param iters Number of maximum iterations
#' @param gamma The learning rate
#' @param threshold Threshold for the difference between RSS of new and old betas.
#' @return a list of beta coefficients
#' @examples
#' \dontrun{
#' gradient_descent_new(body_mass_g ~ bill_depth_mm, penguins)
#' }
#' @export


gradient_descent_new <- function (form, d, contrasts = NULL, iters = 5e5, gamma = 0.0001, threshold = 1e-12){


  # define a function to make model matrices
  # make_model_matrices <- function(form, d){
  #  d_no_na <- model.frame(form, d)
  #  X <- model.matrix(form, d_no_na, contrasts.arg = contrasts)
  #  y_name <- as.character(form)[2]
  #  Y <- matrix(d_no_na[, y_name], ncol = 1)
  #  list(X=X, Y=Y)
  # }

  # extract X and Y
  mms <- make_model_matrices(form, d)
  X <- mms$X
  Y <- mms$Y

  # create training set and test set
  n <- nrow(X)
  t <- round(n * 0.8)
  i <- sample(c(1:n), t)

  set.seed(557)
  X_train <- X[i, ]
  Y_train <- Y[i, ]
  X_test <- X[-i, ]
  Y_test <- Y[-i, ]

  # initialize a beta value as new
  beta_new <- matrix(1, nrow = ncol(X), ncol = 1)
  # initialize a beta value as old (for the below while step)
  beta_old <- matrix(2, nrow = ncol(X), ncol = 1)

  # define a loss function (in this case, just RSS)
  rss <- function(X, Y, beta){
    sum((X %*% beta - Y)^2)
  }

  # gradient descent: run iterations
  # initialize a counter
  step <- 1
  # initialize a difference (larger than threshold)
  diff <- 1

  # when the function works in a tougher case,
  # collinearity problems may introduce an error
  if (qr(X)$rank == dim(X)[2]){

    while ((abs(diff) > threshold) & step < iters){
      beta_old <- beta_new

      #calculate beta based on training set
      beta_new <- beta_old - gamma*(2*t(X_train) %*% X_train %*% beta_old - 2*t(X_train) %*% Y_train)
      diff <- rss(X_test, Y_test, beta_old) - rss(X_test, Y_test, beta_new)
      step <- step + 1
    }

    #return the same result as lm function
    ret <- list(coefficients = beta_new, form = form)
    class(ret) <- "gradient_descent_new"
    ret

  } else {

    #use linear_model instead
    linear_model(form, d, contrasts = NULL)

  }
}
