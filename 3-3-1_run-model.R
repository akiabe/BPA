data.fn <- function(
  n=40, 
  alpha=3.5576, 
  beta1=-0.0912, 
  beta2=0.0091, 
  beta3=-0.00014) {
  
  year <- 1:n
  
  log.expected.count <- alpha + beta1*year + beta2*year^2 + beta3*year^3
  expected.count <- exp(log.expected.count)
  
  C <- rpois(n=n, lambda=expected.count)
  
  plot(
    year, 
    C, 
    type="b", 
    lwd=2, 
    col="black", 
    main="", 
    las=1, 
    ylab="Size", 
    xlab="Year", 
    cex.lab=1.2,
    cex.axis=1.2
  )
  
  lines(
    year, 
    expected.count, 
    type="l", 
    lwd=3, 
    col="red"
  )
  
  return(
    list(
      n=n, 
      alpha=alpha, 
      beta1=beta1, 
      beta2=beta2, 
      beta3=beta3, 
      year=year, 
      expected.count=expected.count, 
      C=C
    )
  )
}

data <- data.fn()
data

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

stan_data <- list(
  C=data$C,
  n=length(data$C),
  year=data$year
)

out <- stan(
  file="3-3-1_model.stan",
  data=stan_data,
  seed=1
)
  
print(out)

mean.year <- mean(data$year)
sd.year <- sd(data$year)

stan_data_std <- list(
  C=data$C,
  n=length(data$C),
  year=(data$year-mean.year) / sd.year
)

out_std <- stan(
  file="3-3-1_model.stan",
  data=stan_data_std,
  seed=1
)

print(out_std)

