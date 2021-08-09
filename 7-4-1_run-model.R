n_occasions <- 6
marked <- rep(50, n_occasions-1)
phi <- rep(0.65, n_occasions-1)
p <- rep(0.4, n_occasions-1)

PHI <- matrix(phi, nrow=sum(marked), ncol=n_occasions-1)
P <- matrix(p, nrow=sum(marked), ncol=n_occasions-1)

simul_cjs <- function(PHI, P, marked) {
  n_occasions <- dim(PHI)[2] + 1
  CH <- matrix(0, nrow=sum(marked), ncol=n_occasions)
  mark_occ <- rep(1:length(marked), marked[1:length(marked)])
  for (i in 1:sum(marked)) {
    CH[i, mark_occ[i]] <- 1
    if (mark_occ[i] == n_occasions) next
    for (t in (mark_occ[i]+1):n_occasions) {
      sur <- rbinom(1,1,PHI[i,t-1])
      if (sur == 0) break
      rp <- rbinom(1,1,P[i,t-1])
      if (rp == 0) CH[i,t] <- 1
    }
  }
  return(CH)
}

CH <- simul_cjs(PHI, P, marked)

get_first <- function(x) min(which(x != 0))
f <- apply(CH, 1, get_first)

library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

data <- list(
  nind=dim(CH)[1],
  n_occasions=dim(CH)[2],
  y=CH
)

data

inits <- function() list(mean_phi=runif(1,0,1),
                         mean_p=runif(1,0,1))

params <- c('alpha', 'beta')

fit <- stan(
  file='7-4-1_model.stan',
  data=data,
  init=inits,
  pars=params,
  seed=1234
)

fit


