#Commensurate prior: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4007051/
#https://onlinelibrary.wiley.com/doi/10.1111/j.1541-0420.2011.01564.x
model_string_modified_pois <- "
model{
for(i in 1:2){ 

  N_cases_orig[i] ~ dpois(mu[i]) #Likelihood for data from original trial
 
  N_cases[i] ~ dpois(lambda[i]) #Likelihood for data from new trial
  
  log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i]*100000)
  
  log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]*100000)

} 
  
  beta1 ~ dnormmix(mu2, tau2, pi2) 
  
  mu2[1] <- delta
  mu2[2] <- 0
  
  tau2[1] <- 100
  tau2[2] <- 1e-4
  
  pi2[1] <- alpha
  pi2[2] <- (1-alpha)
  

  int ~ dnorm(0, 1e-4)

  int_orig ~ dnorm(0, 1e-4)

  delta ~ dnorm(0, 1e-4)

  alpha ~ dbeta(1,1)

}
"
