library(ggplot2)

n.years <- 25
N1 <- 30
mean.lambda <- 1.02
sigma2.lambda <- 0.02
sigma2.y <- 20

y <- N <- numeric(n.years)
N[1] <- N1
lambda <- rnorm(n=n.years-1, mean=mean.lambda, sd=sqrt(sigma2.lambda))

for (t in 1:(n.years-1)) {
  N[t+1] <- N[t] * lambda[t]
}

for (t in 1:n.years) {
  y[t] <- rnorm(n=1, mean=N[t], sd=sqrt(sigma2.y))
}

year <- list(1:25)
d <- data.frame(y=y, T=n.years, year=year)
d

data <- list(y=y, T=n.years)

fit <- stan(
  file="5-2_model.stan",
  data=data,
  seed=1
)

print(fit)

ms <- rstan::extract(fit)
d_qua <- t(apply(
  X=ms$lambda,
  MARGIN=2,
  FUN=quantile,
  prob=c(0.025, 0.5, 0.975)
))
colnames(d_qua) <- c('p2.5', 'p50', 'p97.5')




d_qua <- data.frame(d, d_qua)
head(d_qua, n=3)


year

d <- data.frame(y=y, T=)
























