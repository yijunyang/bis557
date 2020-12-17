#' @title Create Weights for Neural Networks
#' @author (modify based on CASL p217)
#' @description Create list of weights and biases to describe a dense neural network.
#' @param sizes a vector giving the size of each layer, including the input and output layers
#' @return a list containing initialized weights and biases
#' @examples
#' \dontrun{
#' make_weights(c(10, 10, 10, 5))
#' }
#' @export

make_weights <- function(sizes){

  L <- length(sizes) - 1
  weights <- list()

  for (j in 1:L){
    w <- matrix(rnorm(sizes[j] * sizes[j + 1L]), nrow = sizes[j])
    weights[[j]] <- list(w = w, b = rnorm(sizes[j + 1L]))
  }

  return(weights)
}

