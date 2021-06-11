data.fn <- function(
  nyears=40,
  alpha=0,
  beta1=-0.1,
  beta2=-0.9
) {
  year <- 1:nyears;
  YR <- (year-round(nyears/2)) / (nyears/2)
  N <- round(runif(nyears, min=20, max=100))
  exp.p <- plogis(alpha + beta1*YR + beta2*(YR^2))
  C <- rbinom(n=nyears, size=N, prob=exp.p)
  
  plot(
    year, 
    C/N, 
    type="b", 
    ylab="%Successful Pairs", 
    xlab="Year", 
    lwd=2,
    col="black",
    ylim=c(0,1)
  )
  
  points(
    year, 
    exp.p, 
    type="l", 
    lwd=2,
    col="red"
  )
  
  return(
    list(
      nyears=nyears, 
      alpha=alpha, 
      beta1=beta1, 
      beta2=beta2, 
      year=year, 
      YR=YR, 
      exp.p=exp.p, 
      C=C, 
      N=N
    )
  )
}

data <- data.fn(nyears=40, alpha=1, beta1=-0.03, beta2=-0.9)
data

library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
set.seed(123)


stan_data <- list(
  C=data$C,
  N=data$N,
  nyears=length(data$C),
  year=data$YR
)

out <- stan(
  file="3-5-1_model.stan",
  data=stan_data,
  seed=1
)

print(out)

