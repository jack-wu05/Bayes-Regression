data {
  int<lower=0> N;
  vector[N] xs;
  vector[N] ys;
  real x_pred;
}

parameters {
  real slope;
  real<lower=0> sigma;
}

model {
  slope ~ student_t(3, 0, 100);
  sigma ~ exponential(0.001);

  ys ~ normal(slope*xs, sigma);
}

generated quantities {
  real y_pred;
  
  y_pred = normal_rng(slope*x_pred, sigma);
}