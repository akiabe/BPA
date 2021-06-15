nyears <- 25
N <- rep(50, nyears)

lp <- -0.5 + 0.1*(1:nyears)
p <- plogis(lp)

y <- numeric(nyears)

for (i in 1:nyears) {
  y[i] <- rbinom(1, N[i], p[i])
}

p <- data.frame(y=y, year=1:25)

ggplot(data=p, aes(x=year, y=y)) +
  geom_point(alpha=0.6, size=0.9) +
  ylab("Populations size") +
  scale_y_continuous(limits=c(0, NA))

d <- list(y=y, T=n.years)

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

