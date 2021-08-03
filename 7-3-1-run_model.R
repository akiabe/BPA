n.occasions <- 6
marked <- rep(50, n.occasions-1)
phi <- rep(0.65, n.occasions-1)
p <- rep(0.4, n.occasions-1)

PHI <- matrix(phi, nrow=sum(marked), ncol=n.occasions-1)
P <- matrix(p, nrow=sum(marked), ncol=n.occasions-1)

n.occasions
marked
phi
p
PHI
P

simul.cjs <- function(PHI, P, marked) {
  n.occasions <- dim(PHI)[2] + 1
  CH <- matrix(0, nrow=sum(marked), ncol=n.occasions)
  mark.occ <- rep(1:length(marked), marked[1:length(marked)])
  for (i in 1:sum(marked)) {
    CH[i, mark.occ[i]] <- 1
    if (mark.occ[i] == n.occasions) next
    for (t in (mark.occ[i]+1):n.occasions) {
      sur <- rbinom(1,1,PHI[i,t-1])
      if (sur == 0) break
      rp <- rbinom(1,1,P[i,t-1])
      if (rp == 0) CH[i,t] <- 1
    }
  }
return(CH)
}

CH <- simul.cjs(PHI, P, marked)
CH

get.first <- function(x) min(which(x != 0))
f <- apply(CH, 1, get.first)

f









