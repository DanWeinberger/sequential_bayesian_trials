model_string_commensurate_gamma <- "
model{
for(i in 1:2){ 

  N_cases_orig[i] ~ dpois(mu[i]) #Likelihood for data from original trial
 
   log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]*100000)

  N_cases[i] ~ dpois(lambda[i]) #Likelihood for data from new trial
  
   log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i]*100000)
  

} 
  
#Priors for original trial
  int_orig ~ dnorm(0, 1e-4)

  delta ~ dnorm(0, 1e-4) #uninformative prior for original trial

# Priors from new trial

  int ~ dnorm(0, 1e-4)

  beta1 ~ dnorm(delta, tau) #beta centered on alpha with highlight informative prior, which can become less informative if it does not match

 # tau ~ dgamma(1, 0.001) # gamma hyperprior from psborrow
  tau ~ dgamma(0.001, 0.001) # gamma hyperprior from psborrow

	    a1 <- prec.log.irr.obs
	    a2 <- log_irr.obs

}
"
