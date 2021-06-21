data.fn <- function(
  N=200, T=5, p=0.3, c=0.4){
  yfull <- yobs <- array(NA, dim=c(N, T))
  p.eff <- array(NA, dim=N)
  
  yfull[, 1] <- rbinom(n=N, size=1, prob=p)
  
  for (j in 2:T) {
    p.eff <- (1-yfull[, (j-1)]) * p + yfull[, (j-1)] * c
    yfull[, j] <- rbinom(n=N, size=1, prob=p.eff)
  }
  ever.detected <- apply(yfull, 1, max)
  C <- sum(ever.detected)
  yobs <- yfull[ever.detected == 1,]
  cat("存在する", N, "個体のうち", C, "個体が検出された。　\n")
  return(list(N=N, p=p, c=c, C=C, T=T, yfull=yfull, yobs=yobs))
}

data <- data.fn()
data

nz <- 150
yaug <- rbind(data$yobs, array(data=0, dim=c(nz, data$T)))
yaug

library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- list(y=yaug, M=nrow(yaug), T=ncol(yaug))
d

fit <- stan(
  file="6-2-3_model.stan",
  data=d,
  seed=1
)

print(fit)

ms <- rstan::extract(fit)
N <- ms$N
hist(N)  

