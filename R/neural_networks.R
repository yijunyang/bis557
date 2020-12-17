#' @title A 3-layer Neural Networks
#' @author Yijun Yang
#' @description Use a 3-layer artificial neural networks to do classification on sprial data
#' @param X input of X
#' @param y input of y
#' @param maxit maximum number of iterations
#' @param gamma The step size
#' @param H1 number of nodes in the 1st layer
#' @param H2 number of nodes in the 2nd layer
#' @param lambda penalty term
#' @return
#' @examples
#' \dontrun{
#' NN_classify(X, y, maxit = 1e5, gamma = 1e-2, H1 = 10, H2 = 10, lambda = 0.1)
#' }
#' @import fastDummies
#' @export

### a helper function
xy2pos <- function(x, y, nrow) {
  x + nrow * (y-1)
}

NN_classify <- function(X, y, maxit, gamma, H1, H2, lambda){

  # initializa the weights
  K <- length(unique(y)) # number of classes
  D <- ncol(X) # dimension of X
  N <- nrow(X) # total number of X

  if (ncol(as.matrix(y)) == 1 ){
    y_dummy <- as.matrix(dummy_cols(y)[, -1])
  } else {
    y_dummy <- y
  }

  ## weights function has been written in seperated file
  weights <- make_weights(c(D, H1, H2, K))

  ## weights and bias for hidden layer 1
  W1 <- weights[[1]]$w
  b1 <- weights[[1]]$b

  ## weights and bias for hidden layer 2
  W2 <- weights[[2]]$w
  b2 <- weights[[2]]$b

  ## weights and bias for hidden layer 3
  W3 <- weights[[3]]$w
  b3 <- weights[[3]]$b

  # iteration
  for (i in 1:maxit) {

    # forward propagation
    ## hidden layer 1
    h1 <- f_propagate(X, W1, b1, relu)

    ## hidden layer 2
    h2 <- f_propagate(h1, W2, b2, relu)

    ## output layer
    p <- f_propagate(h2, W3, b3, softmax)

    # loss function
    loss <- sum(-log(p)*y_dummy)/N +
      0.5*lambda * (sum(W1*W1) + sum(W2*W2) + sum(W3*W3))

    if (i %% 100 == 0) {
      print(sprintf("iteration %d: loss %f", i, loss))
    }

    # gradients
    #dp <- p
    #dp[xy2pos(1:N, y, nrow(dp))] <- dp[xy2pos(1:N, y, nrow(dp))] - 1
    #dp <- dp / N
    dp <- (p - y_dummy)/N


    # back propagate
    ## output layer
    dW3 <- t(h2) %*% dp + lambda * W3
    db3 <- colSums(dp)

    ## hidden layer 2
    dW2 <- t(h1) %*% (dp %*% t(W3)*relu_p(h2)) + lambda*W2
    db2 <- colSums(dp %*% t(W3)*relu_p(h2))

    ## hidden layer 1
    dW1 <- t(X) %*% ((dp %*% t(W3)*relu_p(h2)) %*% t(W2)*relu_p(h1)) + lambda*W1
    db1 <- colSums((dp %*% t(W3)*relu_p(h2)) %*% t(W2)*relu_p(h1))

    # gradient descent
    W1 <- W1 - gamma * dW1
    b1 <- b1 - gamma * db1
    W2 <- W2 - gamma * dW2
    b2 <- b2 - gamma * db2
    W3 <- W3 - gamma * dW3
    b3 <- b3 - gamma * db3
  }
  return(list(W1, b1, W2, b2, W3, b3))
}


