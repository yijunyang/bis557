#' @title Linear Model Function
#' @author Yijun Yang
#' @description This is the 1st homework assignment of BIS557.
#' @param form A formula
#' @param d A dataframe
#' @param contrasts Contrasts argument
#' @return A list of coefficients
#' @examples
#' \dontrun{
#' linear_model(body_mass_g ~ bill_depth_mm, penguins)
#' }
#' @export


linear_model <- function (form, d, contrasts = NULL){

  #get design matrix
  X <- model.matrix(form, d, contrasts.arg = contrasts)

  #get response
  Y <- d[,all.vars(form)[1]]

  #compute coefficients
  beta <- qr.solve(qr(X), Y)

  #fail message: NA - 0 == NA
  #turn beta = 0 into NA
  beta[beta==0] <- NA

  #return same type as lm()
  list(coefficients = beta)

}
