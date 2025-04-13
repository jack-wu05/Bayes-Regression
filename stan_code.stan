data {
  int N;
  int S;
  
  int<lower=1> subject[N];
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
  
  vector[S] alpha_i;
  vector[S] gamma_i;
}

model {
  delta_1 ~ normal(0,1);
  sigma_1 ~ exponential(0.5);
  
  delta_2 ~ normal(-1,2);
  sigma_2 ~ exponential(0.5);
  
  sigma ~ exponential(0.1);
  
  beta_Age ~ normal(-1,3);
  beta_SES ~ normal(1,3);
  beta_EDUC ~ normal(1,3);
  beta_Group ~ normal(-1,3);
  
  alpha_i ~ normal(delta_1, sigma_1);
  gamma_i ~ normal(delta_2, sigma_2);
  
  for (i in 1:N) {
    real mu = beta_Age*Age[i] + 
              beta_SES*SES[i] + 
              beta_EDUC*EDUC[i] + 
              beta_Group*Group[i] + 
              alpha_i[subject[i]] + 
              gamma_i[subject[i]]*Time[i];
              
    MMSE[i] ~ normal(mu, sigma);
  }
}

