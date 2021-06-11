data {
  int<lower=0> nyears;
  int<lower=0> C[nyears];
  int<lower=0> N[nyears];
  vector[nyears] year;
}

transformed data {
  vector[nyears] year_squared;
  year_squared = year .* year;
}

parameters {
  real alpha;
  real beta1;
  real beta2;
}

transformed parameters {
  vector[nyears] logit_p;
  logit_p = alpha + beta1*year + beta2*year_squared;
}

model {
  alpha ~ normal(0, 100);
  beta1 ~ normal(0, 100);
  beta2 ~ normal(0, 100);
  C ~ binomial_logit(N, logit_p);
}

generated quantities {
  real<lower=0, upper=1> p[nyears];
  for (i in 1:nyears) {
    p[i] = inv_logit(logit_p[i]);
  }
}

