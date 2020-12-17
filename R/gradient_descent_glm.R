#' @title Gradient Descent for GLM (constant step)
#' @author Yijun Yang
#' @description Implement a first-order solution for the GLM maximum likelihood problem using only gradient information, avoiding the Hessian matrix.
#' @param X design matrix
#' @param Y response vector
#' @param mu_fun function from eta to the expected value
#' @param maxit maximum number of iterations
#' @param gamma The step size
#' @param tol numeric tolerance parameter
#' @return a list of beta coefficients
#' @examples
#' \dontrun{
#' glm_constant(X, Y, mu_fun = function(eta) 1/(1+exp(-eta)), var_fun = function(eta) eta)
#' }
#' @export

glm_constant <- function(X, Y,
                         mu_fun, var_fun,
                         maxit = 1e6,
                         tol = 1e-10,
                         gamma = 1e-5){

  beta <- matrix(rep(0, ncol(X), ncol = 1))

  for (i in seq_len(maxit)){
    beta_old <- beta
    grad <- t(X) %*% (var_fun(Y) - matrix(mu_fun(X %*% beta_old)))
    beta <- beta_old + gamma * grad
    if(sqrt(crossprod(beta - beta_old)) < tol) break
  }

  list(beta = beta)

}


#' @title Gradient Descent for GLM (adaptive step)
#' @author Yijun Yang
#' @description Implement a first-order solution for the GLM maximum likelihood problem using only gradient information, avoiding the Hessian matrix.
#' @param X design matrix
#' @param Y response vector
#' @param mu_fun function from eta to the expected value
#' @param maxit integer maximum number of iterations
#' @param gamma The step size
#' @param tol numeric tolerance parameter
#' @param mom momentum parameter
#' @return a list of beta coefficients
#' @examples
#' \dontrun{
#' glm_adapt(X, Y, mu_fun = function(eta) 1/(1+exp(-eta)), var_fun = function(eta) eta)
#' }
#' @export

glm_adapt <- function(X, Y,
                      mu_fun, var_fun,
                      maxit = 1e6,
                      tol = 1e-10,
                      gamma = 1e-5,
                      m = 0.9){

  beta <- matrix(rep(0, ncol(X), ncol = 1))
  mom <- matrix(rep(0, ncol(X), ncol = 1))

  for (i in seq_len(maxit)){
    beta_old <- beta
    mom_old <- mom
    grad <- gamma * t(X) %*% (var_fun(Y) - matrix(mu_fun(X %*% beta_old)))
    mom <- m*mom_old + grad
    beta <- beta_old + mom
    if(sqrt(crossprod(beta - beta_old)) < tol) break
  }

  list(beta = beta)

}
