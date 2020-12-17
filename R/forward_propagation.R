#' @title Forward Propagation
#' @author Yijun Yang
#' @description Apply forward propagation to a set of NN weights and biases.
#' @param X input
#' @param W weights
#' @param b biases
#' @param sigma activation function
#' @return a matrix of
#' @examples
#' \dontrun{
#' f_propagate(X, W1, b1, relu)
#' }
#' @export

f_propagate <- function(X, W, b, sigma){

  h <- sweep(X %*% W, MARGIN = 2, b, FUN = "+")
  h <- sigma(h)

#  if (activation == "relu"){
#    h <- relu(h)
#  } else if (activation == "softmax") {
#    h <- softmax(h)
#  } else {
#    stop("Wrong activation function")
#  }

  return(h)
}


