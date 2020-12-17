#' @title Softmax Function
#' @author Yijun Yang
#' @description apply a function to the output layer to make it behave as a proper set of probabilities
#' @param v a numeric vector
#' @return a vector after applying the softmax function
#' @examples
#' \dontrun{
#' softmax(c(0, -1, 3, -5, 7))
#' }
#' @export

softmax <- function(v){
  exp(v)/rowSums(exp(v))
}

