model_string_modified_logit <- "
model{
for(i in 1:2){ 
  N_cases[i] ~ dbinom(pi[i], pop[i])
  
  logit(pi[i]) <- int  +  vax[i]*beta1 

} 
  

  int ~ dnorm(0, 1e-4)
  
  beta1 ~ dnorm(log_irr.obs, prec.log.irr.obs*alpha )
  
  alpha ~ dunif(1e-6, 1-(1e-6) )
  
  actual_prior_prec = prec.log.irr.obs*alpha

}
"
