library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

d <- read.table("tits.txt", header = TRUE)
head(d, n = 5)
str(d)

C <- as.matrix(d[5:13])

data <- list(
  C=t(C),
  nsite=nrow(C),
  nyear=ncol(C)
)

fit <- stan()



















