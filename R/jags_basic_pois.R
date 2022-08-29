model_string_basic_pois <- "
model{
for(i in 1:2){ 
  N_cases[i] ~ dpois(lambda[i])
  
  log(lambda[i]) <- int  +  vax[i]*beta1 + log(pop[i]*100000)
  
} 
  
  int ~ dnorm(0, 1e-4)
  
  beta1 ~ dnorm(0, 1e-4 )

  tau=1
  delta=1
  a=pop_orig
  b=N_cases_orig
}
"
