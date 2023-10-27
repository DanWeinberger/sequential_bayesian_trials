stan1 <-
  "
data {
  int< lower = 1 > N; // number of observations
  int<lower=0>  N_cases_orig[N]; //  data from original trial
  int N_cases[N]; // data from new trial
  int vax[N];
  real log_pop[N];
  real log_pop_orig[N];
}

parameters {
  real int_orig; //prior on intercept from original trial
  real delta; // prior on VE for original trial
  real int_new; //intercept from new trial
  real beta1; //VE for new trial
  real<lower=0, upper=10> sd1; 
  real<lower=0, upper=10> sd2; 
}

model {
  // Priors for original trial
  int_orig ~ normal(0, 100);
  delta ~ normal(0, 100);

  // Priors from new trial
  int_new ~ normal(int_orig, sd2);
  beta1 ~ normal(delta, sd1);

  //tau ~ gamma(0.01, 0.01);
  //tau2 ~ gamma(0.01, 0.01);
  
  // Likelihoods

  for(i in 1:N){
  N_cases_orig[i] ~ poisson_log(int_orig + delta*vax[i] + log_pop_orig[i]);
  
  N_cases[i] ~ poisson_log(int_new + beta1*vax[i] + log_pop[i]);
   }
}
"


