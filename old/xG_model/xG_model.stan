
data {
  int<lower=0> N; 
  vector<lower=0, upper=1>[N] isgoal; 
  vector[N] x; 
  vector[N] y;
  vector[N] shot_distance;
  vector[N] shot_angle;
  vector[N] seconds_since_last_shot;
  int<lower=0> num_shooters;
  int<lower=0> num_goalies;
  int<lower=0> num_event_detail;
  int<lower=0> num_score_state;
  int<lower=0> num_game_strength_state;
  int<lower=0, upper=num_shooters> shooters[N];
  int<lower=0, upper=num_goalies> goalies[N];
  int<lower=0, upper=num_event_detail> event_detail[N];
  int<lower=0, upper=num_score_state> score_state[N];
  int<lower=0, upper=num_game_strength_state> game_strength_state[N];
}


parameters {
  real mu;
  real<lower=0> sigma;
}


model {
  y ~ normal(mu, sigma);
}

