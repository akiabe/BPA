N <- 16
p <- 0.4

rbinom(n=1, size=N, prob=p)
rbinom(n=1, size=N, prob=p)
rbinom(n=1, size=N, prob=p)

C <- rbinom(n=10^6, size=N, prob=p)
hist(C)
mean(C)
var(C)
sd(C)
