library(magrittr)
library(palmerpenguins)
library(missForest)
library(tibble)
data(penguins)
penguinsi <- penguins %>%
  as.data.frame() %>%
  missForest() %$%
  as_tibble(ximp)

use_data(penguinsi)
