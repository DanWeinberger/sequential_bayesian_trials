model_string_modified_pois <- "
model{
for(i in 1:2){ 
  N_cases[i] ~ dpois(lambda[i])
  
  log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i])

} 

  #estimate from beta1 is centered on historical sample
  beta1_new ~ dnorm(0, 1e-4 )
  
  beta1 <- alpha*beta1_hist + (1-alpha)*beta1_new
  int ~ dnorm(0, 1e-4)
  
  #Take samples from historical data
  beta1_hist ~ dnorm(log_irr.obs, prec.log.irr.obs )

  
  #alpha ~ dunif(1e-6, 1 )
  alpha ~ dgamma(0.001, 0.001)T(1e-6, 1)
  
  actual_prior_prec = prec.log.irr.obs*alpha

}
"
