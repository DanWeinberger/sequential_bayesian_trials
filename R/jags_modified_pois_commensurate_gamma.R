model_string_commensurate_gamma <- "
model{
for(i in 1:2){ 
  N_cases_orig[i] ~ dpois(mu[i]) #Likelihood for data from original trial
 
   log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]/100000)
  N_cases[i] ~ dpois(lambda[i]) #Likelihood for data from new trial
  
   log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i]/100000)
  
} 
  
#Priors for original trial
  int_orig ~ dnorm(0, 1e-4) #uninformative prior on intercept from originl trial
  delta ~ dnorm(0, 1e-4)    #uninformative prior on VE for original trial

# Priors from new trial
  int ~ dnorm(int_orig, tau2)
  beta1 ~ dnorm(delta, tau) #beta centered on delta with highly informative prior, which can become less informative if it does not match

  tau ~ dgamma(set_tau_shp, set_tau_rate) # skeptical variance--prevents extremes--might bias towards prior value?
  tau2 ~ dgamma(set_tau_shp, set_tau_rate) # gamma hyperprior from psborrow; uninformative
	    
	    #these don't do anything; they are just carried through so function works
	    a1 <- prior_prec_log_irr
	    a2 <- prior_mean_log_irr
	    a3 <- sd.upper

	   # alpha=1
}
"