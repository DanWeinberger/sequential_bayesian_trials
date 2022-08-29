#test when there is or is not an effect in the follow up study

run_all_models <- function(sim.ds1, sim.ds2){
## Logistic regression model with uninformative prior

  mod1a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1, prior.mean=0, prior.prec=1e-4,repN=x))
  
  mod1b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2, prior.mean=0, prior.prec=1e-4,repN=x))
  
## Informative prior, with mean and precision from Novavax
  
  mod2a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x))
  
  mod2b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x))
  
## Informative prior, but with Alpha to downweight variance if needed
  
  mod3a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_gamma) )
  
  mod3b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_gamma) )

  
  mod4a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_hcauchy) )
  
  mod4b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_hcauchy) )
  

  #mod5a <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_uniform) )
  
  #mod5b <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds2,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_uniform) )
  
  
  out.list= list('mod1a'=mod1a,'mod1b'=mod1b, 'mod2a'=mod2a,'mod2b'=mod2b, 'mod3a'=mod3a,'mod3b'=mod3b,'mod4a'=mod4a,'mod4b'=mod4b) #, 'mod5a'=mod5a, 'mod5b'=mod5b )
}