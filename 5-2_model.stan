data {
  int<lower=0> T;
  vector[T] y;
}

parameters {
  real<lower=0,upper=10> mean_lambda;
  real<lower=0,upper=10> sigma_proc;
  real<lower=0,upper=100> sigma_obs;
  vector<lower=0> [T-1] lambda;
  real<lower=0,upper=500> N_est1;
}

transformed parameters {
  vector<lower=0>[T] N_est;
  N_est[1] = N_est1;
  for (t in 1:(T-1)) {
    N_est[t+1] = N_est[t] * lambda[t];
  }
}

model {
  lambda ~ normal(mean_lambda, sigma_proc);
  y ~ normal(N_est, sigma_obs);
}

generated quantities {
  real<lower=0> sigma2_obs;
  real<lower=0> sigma2_proc;
  sigma2_obs = square(sigma_obs);
  sigma2_proc = square(sigma_proc);
}


