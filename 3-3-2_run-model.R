peregrine <- read.table("falcons.txt", header=TRUE)
head(peregrine, n=3)

plot(peregrine$Year, peregrine$Pairs, ylab="Pairs", xlab="Year", type="b")
plot(peregrine$Year, peregrine$R.Pairs, ylab="R.Pairs", xlab="Year", type="b")
plot(peregrine$Year, peregrine$Eyasses, ylab="Eyasses", xlab="Year", type="b")

library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
set.seed(123)

mean.Year <- mean(peregrine$Year)
sd.Year <- sd(peregrine$Year)

stan_data <- list(
  C=peregrine$Pairs,
  n=length(peregrine$Pairs),
  year=(peregrine$Year-mean.Year) / sd.Year
)

out <- stan(
  file="3-3-2_model.stan",
  data=stan_data,
  seed=1
)

print(out)

