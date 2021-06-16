library(ggplot2)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

data.fn <- function(N=100, p=0.5, T=3) {
  yfull <- yobs <- array(NA, dim=c(N, T))
  for (j in 1:T) {
    yfull[,j] <- rbinom(n=N, size=1, prob=p)
  }
  ever.detected <- apply(X=yfull, MARGIN=1, FUN=max)
  C <- sum(ever.detected)
  yobs <- yfull[ever.detected==1,]
  cat("存在する", N, "個体のうち", C, "個体が検出された。　\n")
  return(list(N=N, p=p, C=C, T=T, yfull=yfull, yobs=yobs))
}

data <- data.fn()
data


nz <- 150
yaug <- rbind(data$yobs, array(data=0, dim=c(nz, data$T)))
yaug

d <- list(y=yaug, M=nrow(yaug), T=ncol(yaug))
d

fit <- stan(
  file="6-2-1_model.stan",
  data=d,
  seed=1
)

print(fit)

