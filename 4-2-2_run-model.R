library(ggplot2)

d <- read.table("falcons.txt", header = TRUE)
head(d, n = 3)

ggplot(data=d, aes(x=Year, y=Pairs)) +
  geom_point(alpha=0.6, size=0.9) 

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mean_year <- mean(d$Year)
sd_year <- sd(d$Year)
scl_year <- (d$Year-mean_year) / sd_year

data <- list(
  C=d$Pairs,
  n=length(d$Pairs),
  year=scl_year
)

fit <- stan(
  file="4-2-1_model.stan",
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

ggplot(data=d_qua, aes(x=Year, y=Pairs)) +
  geom_point(alpha=0.6, size=0.9) +
  geom_line(aes(y=p50), size=0.9) +
  geom_ribbon(aes(ymin=p2.5, ymax=p97.5), alpha=0.3) +
  ylab("Count") +
  scale_y_continuous(limits=c(0, NA))

