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
