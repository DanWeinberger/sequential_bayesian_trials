#test when there is or is not an effect in the follow up study

run_all_models <- function(sim.ds1, sim.ds2){
## Logistic regression model with uninformative prior

  mod1a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1, repN=x))
  
  mod1b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2, repN=x))

## Informative prior, but with Alpha to downweight variance if needed
  
  mod3a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,repN=x, model.select=model_string_commensurate_gamma) )
  
  mod3b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2,repN=x, model.select=model_string_commensurate_gamma) )

  
  mod4a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,repN=x, model.select=model_string_commensurate_hcauchy) )
  
  mod4b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2,repN=x, model.select=model_string_commensurate_hcauchy) )
  


  out.list= list('mod1a'=mod1a,'mod1b'=mod1b,  'mod3a'=mod3a,'mod3b'=mod3b,'mod4a'=mod4a,'mod4b'=mod4b) #, 'mod5a'=mod5a, 'mod5b'=mod5b )
}