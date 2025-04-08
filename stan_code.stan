data {
  int N;
  int S;
  
  int<lower=1,upper=S> subject[N];
  vector[N] Age;
  vector[N] SES;
  vector[N] EDUC;
  vector[N] Group;
  vector[N] Time;
  vector[N] MMSE;
}

parameters {
  real delta_1;
  real<lower=0> sigma_1;
  
  real<lower=0> sigma;
  
  real delta_2;
  real<lower=0> sigma_2;
  
  real beta_Age;
  real beta_SES;
  real beta_EDUC;
  real beta_Group;
  
  vector[S] alpha;
  vector[S] gamma;
}

model {
  delta_1 ~ normal(25,15);
  sigma_1 ~ exponential(0.01);
  
  delta_2 ~ normal(1,10);
  sigma_2 ~ exponential(0.01);
  
  sigma ~ exponential(0.01);
  
  beta_Age ~ normal(1,5);
  beta_SES ~ normal(1,5);
  beta_EDUC ~ normal(1,5);
  beta_Group ~ normal(1,5);
  
  alpha ~ normal(delta_1, sigma_1);
  gamma ~ normal(delta_2, sigma_2);
  
  for (i in 1:N) {
    real mu = beta_Age*Age[i] + 
              beta_SES*SES[i] + 
              beta_EDUC*EDUC[i] + 
              beta_Group*Group[i] + 
              alpha[subject[i]] + 
              gamma[subject[i]]*Time[i];
              
    MMSE[i] ~ normal(mu, sigma);
  }
}