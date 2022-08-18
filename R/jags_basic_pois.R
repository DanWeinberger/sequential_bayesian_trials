model_string_basic_pois <- "
model{
for(i in 1:2){ 
  N_cases[i] ~ dpois(lambda[i])
  
  log(lambda[i]) <- int  +  vax[i]*beta1 + log(pop[i])

} 
  

  int ~ dnorm(0, 1e-4)
  
  beta1 ~ dnorm(log_irr.obs, prec.log.irr.obs )

  actual_prior_prec =prec.log.irr.obs
  alpha=1
}
"
