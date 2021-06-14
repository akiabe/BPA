data {
  int<lower=0> T;
  vector[T] y;
}

parameters {
  vector<lower=0>[T] lambda;
  real<lower=0,upper=10> mean_lambda;
  real<lower=0,upper=10> sigma_proc;
  real<lower=0,upper=100> sigma_obs;
  real<lower=0,upper=500> N_est1;
}

transformed parameters {
  vector<lower=0>[T] N_est;
  N_est[1] = N_est1;
  for (i in 2:T) {
    N_est[i] = N_est[i-1] * lambda[i];
  }
}

model {
  lambda ~ normal(mean_lambda, sigma_proc);
  for (i in 1:T) {
    y[i] ~ normal(N_est[i], sigma_obs);
  }
}

generated quantities {
  real<lower=0> sigma2_obs;
  real<lower=0> sigma2_proc;
  sigma2_obs = square(sigma_obs);
  sigma2_proc = square(sigma_proc);
}


