data {
  int<lower=0> n;
  int<lower=0> C[n];
  vector[n] year;
}

transformed data {
 vector[n] year_squared;
 vector[n] year_cubed;
 
 year_squared = year .* year;
 year_cubed = year .* year .* year;
}

parameters {
  real alpha;
  real beta1;
  real beta2;
  real beta3;
  vector[n] eps;
  real<lower=0> sigma;
}

transformed parameters {
  vector[n] log_lambda;
  log_lambda = alpha + beta1*year + beta2*year_squared + beta3*year_cubed + eps;
}

model {
  C ~ poisson_log(log_lambda);
  eps ~ normal(0, sigma);
}

generated quantities {
  vector<lower=0>[n] lambda;
  lambda = exp(log_lambda);
}

