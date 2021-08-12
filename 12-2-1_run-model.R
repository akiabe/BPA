R <- 200
T <- 3
y <- array(dim=c(R,T))
N <- rpois(n=R, lambda=2)
for (j in 1:T) {
  y[,j] <- rbinom(n=R, size=N, prob=0.5)
}

cbind(N, y)

data <- list(y=y, R=nrow(y), T=ncol(y), K=100)
fit <- stan(file='12-2-1_model.stan', data=data, seed=1234)
fit
