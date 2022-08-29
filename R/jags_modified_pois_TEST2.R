model_string_modified_pois <- "
model{
for(i in 1:2){ 
  N_cases[i] ~ dpois(lambda[i])
  
  log(lambda[i])  <- int  +  vax[i]*beta1 + log(pop[i])

} 

  int ~ dnorm(0, 1e-4)
  
  #Take samples from historical data
  mu[1] <- log_irr.obs
  tau[1] <- prec.log.irr.obs 
  
  mu[2] <-0
  tau[2] <- 1e-4

  beta1 ~ dnorm(mu[r], tau[r])
  r ~ dcat(alpha)
  
   alpha[1:2] ~ ddirch(pi[1:2])

  pi <- c(1,1)


}
"
