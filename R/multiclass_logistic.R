#' @title Multiclass Logistic Regression
#' @author Yijun Yang
#' @description Describe and implement a classification model generalizing logistic regression to accommodate more than two classes.
#' @param form A formula
#' @param d A dataframe
#' @return (1) beta coefficients; (2) the misclassification error; (3) a detailed classification table
#' @examples
#' \dontrun{
#' multiclass_logistic(form = species ~ bill_length_mm + bill_depth_mm, d = penguinsi)
#' }
#' @export

multiclass_logistic <- function (form, d){

  mms <- make_model_matrices(form, d)
  X <- mms$X
  Y <- mms$Y

  level <- levels(as.data.frame(d)[,all.vars(form)[1]])
  n <- length(level)

  fit = list()

  for (i in 1:n){
    Y_tmp <- ifelse(Y == level[i], 1, 0)
    data_tmp <- d
    data_tmp[,all.vars(form)[1]] <- Y_tmp
    fit[[i]] <- glm(form, data = data_tmp, family = binomial)
  }

  # get the coefficients
  coef <- fit[[1]]$coefficients
  for (i in 2:n){
  coef <- as.matrix(rbind(coef,fit[[i]]$coefficients))
  }

  rownames(coef) <- level

  # get prediction
  prediction <- matrix(0, nrow = nrow(d), ncol = n)
  for (i in 1:n){
    prediction[,i] <- predict(fit[[i]], type="response")
  }

  Y_hat <- apply(prediction, 1, which.max)
  Y_hat <- factor(Y_hat, levels = 1:n, labels = level)

  classerror <- mean(Y != Y_hat)
  classtable <- table(Y_hat, Y)

  ret <- list(Coefficients = coef,
              MisclassificationError = classerror,
              ClassificationTable = classtable)
  ret
}
