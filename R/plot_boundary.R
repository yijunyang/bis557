#' @title Plot Neural Network Resulting Classifier
#' @author Yijun Yang
#' @description plot the results of decision boundary of an artificial neural network
#' @param X input X
#' @param y input y
#' @param model weights and biases from neural_network function
#' @param title title of the plot
#' @return a plot with classification
#' @examples
#' \dontrun{
#' plot_boundary(X, y, nn.model)
#' }
#' @export

plot_boundary <- function(X, y, model, topic) {

  # create background grids
  rx <- range(X[,1])
  ry <- range(X[,2])

  lx <- seq(rx[1] - 0.1, rx[2] + 0.1, length.out = 100)
  ly <- seq(ry[1] - 0.1, ry[2] + 0.1, length.out = 100)

  # plot the background
  d_bg <- expand.grid(x = lx, y = ly)
  grid <- as.matrix(expand.grid(lx, ly))
  z <- NN_pred(grid, model)
  d_bg$z <-z

  # plot the real points
  d_real <- data.frame(x1=X[,1], x2=X[,2], y_real = y)
  colnames(d_real)=c("x1","x2","y_real")

  ggplot(data = d_bg, aes(x,y)) +
    geom_raster(aes(fill = as.factor(z)), alpha = 0.3) +
    geom_point(data = d_real, aes(x1, x2, color = as.factor(y_real)), size = 3, pch = 20) +
    scale_color_manual(values=c("darkorange2", "darkolivegreen3", "darkgreen", "dodgerblue3", "darkorchid")) +
    ggtitle(topic) +
    theme_bw(base_size = 15) +
    theme(axis.ticks=element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_blank(), axis.title=element_blank(),
          legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 12)) +
    coord_fixed()
}
