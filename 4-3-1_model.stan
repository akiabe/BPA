data {
  int<lower=0> nsite;
  int<lower=0> nyear;
  int<lower=0> C[nyear, nsite];
  vector[nyear] year;
}

transformed data {
  vector[nyear] year_squared;
  vector[nyear] year_cubed;
  year_squared = year .* year;
  year_cubed = year .* year .* year;
}

parameters {
  real mu;
  vector[nsite] alpha;
  real eps[nyear];
  real beta[3];
  real<lower=0> sd_alpha;
  real<lower=0> sd_year;
}

transformed parameters {
  vector[nsite] log_lambda[nyear];
  for (i in 1:nyear) {
    log_lambda[i] = alpha 
                  + beta[1]*year[i] 
                  + beta[2]*year_squared[i] 
                  + beta[3]*year_cubed[i] 
                  + eps[i];
  }
}

model {
   alpha ~ normal(mu, sd_alpha);
   mu ~ normal(0, 10);
   beta ~ normal(0, 10);
   eps ~ normal(0, sd_year);
   for (i in 1:nyear) {
    C[i] ~ poisson_log(log_lambda[i]);
  }
}


