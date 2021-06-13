library(ggplot2)

data.fn <- function (
  nsite=5, 
  nyear=40, 
  alpha=4.18456, 
  beta1=1.90672, 
  beta2=0.10852, 
  beta3=-1.17121, 
  sd.site=0.5, 
  sd.year=0.2
) {
  C = log.expected.count <- array(NA, dim=c(nyear, nsite))
  year <- 1:nyear
  yr <- (year-20) / 20
  site <- 1:nsite
  alpha.site <- rnorm(n=nsite, mean=alpha, sd=sd.site)
  eps.year <- rnorm(n=nyear, mean=0, sd=sd.year)
  for (j in 1:nsite) {
    log.expected.count[,j] <- alpha.site[j] + beta1*yr + beta2*yr^2 + beta3*yr^3 + eps.year
    expected.count <- exp(log.expected.count[,j])
    C[,j] <- rpois(n=nyear, lambda=expected.count)
  }
  
  return(
    list(
      nsite=nsite, 
      nyear=nyear, 
      alpha.site=alpha.site, 
      beta1=beta1, 
      beta2=beta2, 
      beta3=beta3, 
      year=year, 
      sd.site=sd.site,
      sd.year=sd.year,
      expected.count=expected.count,
      C=C
    )
  )
}

d <- data.fn(nsite=100, nyear=40, sd.site=0.3, sd.year=0.2)
d

d_plot <- data.frame(year=d$year, C=d$C)
d_plot

ggplot(d_plot) +
  geom_line(aes(x=year, y=d_plot[,10])) +
  geom_line(aes(x=year, y=d_plot[,25])) +
  geom_line(aes(x=year, y=d_plot[,50])) +
  geom_line(aes(x=year, y=d_plot[,75])) +
  geom_line(aes(x=year, y=d_plot[,100])) +
  ylab("Population size")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mean_year <- mean(d$year)
sd_year <- sd(d$year)
scl_year <- (d$year-mean_year) / sd_year

data <- list(
  nsite=ncol(d$C),
  nyear=nrow(d$C),
  C=d$C,
  year=scl_year
)

fit <- stan(
  file="4-3-1_model.stan", 
  data=data,
  seed=1
)

print(fit)

