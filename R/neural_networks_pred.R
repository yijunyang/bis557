NN_pred <- function(X, para = list()){
  # this part refers to
  # https://towardsdatascience.com/build-your-own-neural-network-classifier-in-r-b7f1f183261d

  W1 <- para[[1]]
  b1 <- para[[2]]
  W2 <- para[[3]]
  b2 <- para[[4]]
  W3 <- para[[5]]
  b3 <- para[[6]]

  N <- nrow(X)

  ## hidden layer 1
  h1 <- f_propagate(X, W1, b1, relu)

  ## hidden layer 2
  h2 <- f_propagate(h1, W2, b2, relu)

  ## output layer
  p <- f_propagate(h2, W3, b3, softmax)
  predicted_class <- apply(p, 1, which.max)

  return(predicted_class)
}
