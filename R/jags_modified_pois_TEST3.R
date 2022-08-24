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
  
  beta1 <- delta*alpha + eta*(1-alpha)
  
  eta ~ dnorm(0, 1e-4)

  delta ~ dnorm(0, 1e-4)
  
  logit_alpha ~ dunif(0, 1e-4)

  alpha <- exp(logit_alpha/(1+logit_alpha))

}
"
