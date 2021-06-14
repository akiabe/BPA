library(ggplot2)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

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

p <- data.frame(y=y, year=1:25)
p

ggplot(data=p, aes(x=year, y=y)) +
  geom_point(alpha=0.6, size=0.9) +
  ylab("Populations size") +
  scale_y_continuous(limits=c(0, NA))

d <- list(y=y, T=n.years)
d

fit <- stan(
  file="5-2_model.stan",
  data=d,
  seed=1
)

print(fit)

ms <- rstan::extract(fit)
d_qua <- data.frame(t(apply(
  X=ms$N_est,
  MARGIN=2,
  FUN=quantile,
  prob=c(0.025, 0.5, 0.975)
)))
d_qua

colnames(d_qua) <- c('p2.5', 'p50', 'p97.5')
d_qua$y <- d$y
d_qua$year <- 1:25
d_qua

ggplot(data=d_qua, aes(x=year, y=y)) +
  geom_point(alpha=0.6, size=0.9) +
  geom_line(aes(y=p50), size=0.9) +
  geom_ribbon(aes(ymin=p2.5, ymax=p97.5), alpha=0.3) +
  ylab("Populations size") +
  scale_y_continuous(limits=c(0, NA))

