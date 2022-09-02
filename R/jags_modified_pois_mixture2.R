#Commensurate prior: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4007051/
#https://onlinelibrary.wiley.com/doi/10.1111/j.1541-0420.2011.01564.x
model_string_modified_pois_mixture <- "
model{
for(i in 1:2){ 

  N_cases_orig[i] ~ dpois(mu[i]) #Likelihood for data from original trial
 
  log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]*100000)




  N_cases[i] ~ dpois(lambda[i]) #Likelihood for data from new trial
  
  log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i]*100000)
  

} 
  

  int ~ dnorm(0, 1e-4)

  int_orig ~ dnorm(0, 1e-4)

  delta ~ dnorm(0, 1e-4) #uninformative prior for original trial
  
  beta1  <- delta*mix.select + eta*(1- mix.select)
  
  eta ~ dnorm(0, 5) #Highly informativ 
  
  mix.select ~ dbern(alpha)
  
 alpha ~ dunif(0,1)


  a=prec.log.irr.obs
  b=log_irr.obs
  tau=1
}
"
