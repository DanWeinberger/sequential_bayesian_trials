model_string_modified_pois <- "
model{
for(i in 1:2){ 

  N_cases_orig[i] ~ dpois(mu[i]) #Likelihood for data from original trial
 
  N_cases[i] ~ dpois(lambda[i]) #Likelihood for data from new trial
  
  log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i]*100000)
  
  log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]*100000)

} 
  

  int ~ dnorm(0, 1e-4)

  int_orig ~ dnorm(0, 1e-4)

  delta ~ dnorm(0, 1e-4) #uninformative prior for original trial
  
  beta1 ~ dnorm(delta, 100*alpha) #beta centered on alpha with highlight informative prior, which can become less informative if it does not match

  alpha ~ dgamma(0.01, 0.01)T(1e-6, 1)

}
"
