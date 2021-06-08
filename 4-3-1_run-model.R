data.fn <- function (nsite = 5, nyear = 40, alpha = 4.18456, beta1 = 1.90672, beta2 = 0.10852, beta3 = -1.17121, sd.site = 0.5, sd.year = 0.2) {
  C = log.expected.count <- array(NA, dim = c(nyear, nsite))
  
  year <- 1:nyear
  yr <- (year - 20) / 20
  site <- 1:nsite
  
  alpha.site <- rnorm(n = nsite, mean = alpha, sd = sd.site)
  eps.year <- rnorm(n = nyear, mean = 0, sd = sd.year)
  
  for (j in 1:nsite) {
    log.expected.count[,j] <- alpha.site[j] + beta1*yr + beta2*yr^2 + beta3*yr^3 + eps.year
    expected.count <- exp(log.expected.count[,j])
    C[,j] <- rpois(n = nyear, lambda = expected.count)
  }
  
  matplot(year, C, type = "l", lty = 1, lwd = 2, main = "", las = 1, ylab = "Size", xlab = "Year")
  return(
    list(
      nsite = nsite, 
      nyear = nyear, 
      alpha.site = alpha.site, 
      beta1 = beta1, 
      beta2 = beta2, 
      beta3 = beta3, 
      year = year, 
      sd.site = sd.site,
      sd.year = sd.year,
      expected.count = expected.count,
      C = C
      )
    )
}

data <- data.fn(nsite = 100, nyear = 40, sd.site = 0.3, sd.year = 0.2)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

stan_data <- list(
  C = data$C,
  nsite = ncol(data$C),
  nyear = nrow(data$C),
  year = (data$year - 20) / 20
)

## Initial values
inits <- function() list(
  mu = runif(1, 0, 2),
  alpha = runif(stan_data$nsite, -1, 1),
  beta = runif(3, -1, 1),
  sd_alpha = runif(1, 0, 0.1),
  sd_year = runif(1, 0, 0.1)
)

## Parameters monitored
params <- c("mu", "alpha", "beta", "sd_alpha", "sd_year")

# MCMC settings
ni <- 40000
nt <- 10
nb <- 30000
nc <- 4

## Call Stan from R
out <- stan(
  file = "4-3-1_model.stan", 
  data = stan_data,
  init = inits, 
  pars = params,
  chains = nc, 
  iter = ni, 
  warmup = nb, 
  thin = nt,
  seed = 1
)

## Summarize posteriors
print(out)












