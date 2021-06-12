library(ggplot2)

data.fn <- function(
  n=40, 
  alpha=3.5576, 
  beta1=-0.0912, 
  beta2=0.0091, 
  beta3=-0.00014, 
  sd=1
) {
  year <- 1:n
  eps <- rnorm(n=n, mean=0, sd=sd)
  log.expected.count <- alpha + beta1*year + beta2*year^2 + beta3*year^3 + eps
  expected.count <- exp(log.expected.count)
  C <- rpois(n=n, lambda=expected.count)
  
  return(
    list(
      n=n, 
      alpha=alpha, 
      beta1=beta1, 
      beta2=beta2, 
      beta3=beta3, 
      year=year, 
      sd=sd, 
      expected.count=expected.count, 
      C=C
    )
  )
}

d <- data.fn()
d

plot_df <- data.frame(year=d$year, C=d$C)
ggplot(data=plot_df, aes(x=year, y=C)) +
  geom_point(alpha=0.6, size=0.9) +
  ylab("Count")

library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

mean_year <- mean(d$year)
sd_year <- sd(d$year)
scl_year <- (d$year-mean_year) / sd_year

data <- list(
  n=length(data$C),
  C=d$C,
  year=scl_year
)

fit <- stan(
  file = "4-2-1_model.stan",
  data = data,
  seed = 1
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

ggplot(data=d_qua, aes(x=year, y=C)) +
  geom_point(alpha=0.6, size=0.9) +
  geom_line(aes(y=p50), size=0.9) +
  geom_ribbon(aes(ymin=p2.5, ymax=p97.5), alpha=0.3) +
  ylab("Count") +
  scale_y_continuous(limits=c(0, NA))

