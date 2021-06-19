data.fn <- function(
  N=100, mean.p=0.5, T=3, time.eff=runif(T, -2, 2)){
  yfull <- yobs <- array(NA, dim=c(N, T))
  p.vec <- array(NA, dim=T)
  for (j in 1:T) {
    p <- plogis(log(mean.p/(1-mean.p)) + time.eff[j])
    yfull[,j] <- rbinom(n=N, size=1, prob=p)
    p.vec[j] <- p
  }
  ever.detected <- apply(yfull, 1, max)
  C <- sum(ever.detected)
  yobs <- yfull[ever.detected == 1]
  cat("存在する", N, "個体のうち", C, "個体が検出された。　\n")
  return(list(N=N, p.vec=p.vec, C=C, T=T, yfull=yfull, yobs=yobs))
}

data <- data.fn()

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

ms <- rstan::extract(fit)
N <- ms$N
hist(N)     
            