model_string_basic_pois <- "
model{
for(i in 1:2){ 
  N_cases_orig[i] ~ dpois(mu[i])

  N_cases[i] ~ dpois(lambda[i])
  
  log(lambda[i]) <- int  +  vax[i]*beta1 + log(pop[i]*100000)
  
  log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]*100000) #this is just included as a check--do we get same estimates out as provided in original study when we start with the counts?

} 
  


  int ~ dnorm(0, 1e-4)
  
  beta1 ~ dnorm(log_irr.obs, prec.log.irr.obs )


  int_orig ~ dnorm(0, 1e-4)

  delta ~ dnorm(0, 1e-4)

  alpha=1
}
"
