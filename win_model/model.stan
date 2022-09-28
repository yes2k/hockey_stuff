data {
  int<lower=0> N;
  vector[N] P_curr;
  vector[N] P_prev;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

model {
  alpha ~ normal(75, 15)
  beta ~ normal(20, 10)
  P_curr ~ normal(alpha + beta * P_prev, sigma);
}