data {
  int<lower = 0> N;
  array[N] int<lower = 0> score;
  real<lower = 0> alpha;
  real<lower = 0> beta;
  int<lower = 0, upper=1> only_prior;
}

parameters {
  real<lower = 0> lambda;
}

model {
  // Priors
  lambda ~ gamma(alpha, beta);
  
  // Model
  if(only_prior == 0){
    score ~ poisson(lambda);
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N] score_pred;
  for(i in 1:N){
    score_pred[i] = poisson_rng(lambda);
    log_lik[i] = poisson_lpmf(score[i]| lambda);
  }
}
