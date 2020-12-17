#' @title A Rectified Linear Unit (ReLU)
#' @author Yijun Yang
#' @description pushes negative values to 0 while returns positive values unmodified
#' @param v a numeric vector or matrix
#' @return the original vector with negative values truncated to zero
#' @examples
#' \dontrun{
#' relu(c(0, -1, 3, -5, 7))
#' }
#' @export

relu <- function(v){
  v[v < 0] <- 0
  v
}

