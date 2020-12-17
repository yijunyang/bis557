# number of categories
K <- 5
# number of points for each class
N <- 100
# initialize X and y
X <- matrix(0, nrow = 0, ncol = 2)
y <- c()

# simulate dataset
set.seed(2020)
for (i in 1:K) {
  r <- seq(0, 1, length.out = N)
  t <- seq(i*2*pi/5, (i+1)*2*pi/5, length.out = N) +
    rnorm(N, sd = 0.35) # add some noise
  X <- rbind(X, cbind(r*sin(t), r*cos(t)))
  y <- c(y, rep(i, N))
}

df <- as.data.frame(cbind(X, y))

# set plot images to a nice size.
options(repr.plot.width = 20, repr.plot.height = 20)

ggplot(df, aes(x = df[,1], y = df[,2])) +
  geom_point(aes(color = as.factor(y), alpha = 1), size = 3, pch = 20) +
  scale_color_manual(values=c("darkorange2", "darkolivegreen3", "darkgreen", "dodgerblue3", "darkorchid")) +
  ggtitle('Spiral Data - 5 categories') +
  theme_bw(base_size = 15) +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 12)) +
  coord_fixed()
