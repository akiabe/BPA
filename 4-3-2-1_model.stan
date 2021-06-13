data {
  int<lower=0> nyear;
  int<lower=0> nsite;
  int<lower=0> C;
}

parameters {
  real alpha;
}

transformed parameters {
  matrix[nyear, nsite] log_lambda;
  log_lambda = rep_matrix(alpha, nyear, nsite);
}

model {
  alpha ~ normal(0, 10);
  for (i in 1:nyear)
    obs[i] ~ poisson_log(log_lambda[obsyear[i], obssite[i]]);
}

generated quantities {
  int<lower=0> mis[nmis];

  for (i in 1:nmis)
    mis[i] = poisson_log_rng(log_lambda[misyear[i], missite[i]]);
}






