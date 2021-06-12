library(ggplot2)

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

plot_df <- data.frame(year=data$year, C=data$C, N=data$N) 
ggplot(data=plot_df, aes(x=year, y=C/N)) +
  geom_point(alpha=0.6, size=0.9) +
  ylab("%Successful pairs") +
  scale_y_continuous(limits=c(0, 1))

library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

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

mcmc_sample <- rstan::extract(out)

prob_name <- "p"
quantile(
  mcmc_sample[[prob_name]][,1], 
  probs=c(0.025, 0.5, 0.975)
)

result_df <- data.frame(t(apply(
  X=mcmc_sample[[prob_name]],
  MARGIN=2,
  FUN=quantile,
  probs=c(0.025, 0.5, 0.975)
)))

colnames(result_df) <- c("lwr", "fit", "upr")
result_df$Year <- data$year
result_df$C <- data$C
result_df$N <- data$N

head(result_df, n=3)

ggplot(data=result_df, aes(x=Year, y=C/N)) +
  geom_point(alpha=0.6, size=0.9) +
  geom_line(aes(y=fit), size=0.9) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3) +
  ylab("%Successful pairs") +
  scale_y_continuous(limits=c(0, 1))

