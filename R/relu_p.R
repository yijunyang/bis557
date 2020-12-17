#' @title Derivative of ReLU
#' @author (refer to CASL p218)
#' @description Apply derivative of the rectified linear unit (ReLU).
#' @param v a numeric vector or matrix
#' @return the original vector with positive values to 1 and negative values to zero
#' @examples
#' \dontrun{
#' relu(c(0, -1, 3, -5, 7))
#' }
#' @export

relu_p <- function(v){
  p <- v * 0
  p[v > 0] <- 1
  p
}


