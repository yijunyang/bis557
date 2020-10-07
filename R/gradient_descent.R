#' @title Gradient Descent Function
#' @author Yijun Yang
#' @description Implement gradient descent for ordinary least squares.
#' @param form A formula
#' @param d A dataframe
#' @param contrasts Contrasts argument
#' @param iters Number of maximum iterations
#' @param gamma The learning rate
#' @param threshold Threshold for the difference between RSS of new and old betas.
#' @return a list of beta coefficients
#' @examples
#' \dontrun{
#' gradient_descent(body_mass_g ~ bill_depth_mm, penguins)
#' }
#' @export


gradient_descent <- function (form, d, contrasts = NULL, iters = 1e5, gamma = 0.0001, threshold = 1e-12){

  d_no_na <- model.frame(form, d)

  #get design matrix
  X <- model.matrix(form, d_no_na, contrasts.arg = contrasts)

  #get response
  Y <- d_no_na[,all.vars(form)[1]]

  #initialize a beta value as new
  beta_new <- matrix(1, nrow = ncol(X), ncol = 1)

  #initialize a beta value as old (for the below while step)
  beta_old <- matrix(2, nrow = ncol(X), ncol = 1)

  #define a loss function (in this case, is just RSS)
  rss <- function(X, Y, beta){
    sum((X %*% beta - Y)^2)
  }

  #gradient descent: run iterations
  #initialize a counter
  step <- 1
  #initialize a difference (larger than threshold)
  diff <- 1

  #when the function works in a tougher case, collinearity problems may introduce an error
  if (qr(X)$rank == dim(X)[2]){

    while ((abs(diff) > threshold) & step < iters){
      beta_old <- beta_new
      beta_new <- beta_old - gamma*(2*t(X)%*%X%*%beta_old-2*t(X)%*%Y)
      diff <- rss(X, Y, beta_old) - rss(X, Y, beta_new)
      step <- step + 1
    }

    #return the same result as lm function
    return(list(coefficients = beta_new))

    } else {

    #use linear_model instead
    linear_model(form, d, contrasts = NULL)

  }
}
