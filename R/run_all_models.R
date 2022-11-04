#test when there is or is not an effect in the follow up study

run_all_models <- function(sim.ds1, sim.ds2,sim.ds3){
## Logistic regression model with uninformative prior

  numCores = detectCores() -1
  
   cl <- makeCluster(numCores)
   clusterEvalQ(cl,{ 
                library(rjags, quietly = TRUE) 
                library(HDInterval, quietly = TRUE)
                })
   
  clusterExport(cl, c('sim.ds1','sim.ds2','sim.ds3','call_jags','model_string_basic_pois','model_string_commensurate_gamma','prior.data'), environment())
  
  
  mod1 <- parLapply(cl,1:N.sim, function(x) call_jags(sim.ds=sim.ds1, prior.mean=0, prior.prec=1e-4,repN=x))
  

## Informative prior, with mean and precision from Novavax

  mod2 <- mclapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x))
  
## Informative prior, but with Alpha to downweight variance if needed
  
  mod3 <- lapply(1:N.sim, function(x) call_jags(sim.ds=sim.ds1,prior.mean=prior.data$log_irr.obs[1], prior.prec=prior.data$prec.log.irr.obs,repN=x, model.select=model_string_commensurate_gamma) )
  
  
  stopCluster(cl)
  
  out.list= list('mod1a'=mod1a,'mod1b'=mod1b, 'mod1c'=mod1c, 'mod2a'=mod2a,'mod2b'=mod2b,'mod2c'=mod2c, 'mod3a'=mod3a,'mod3b'=mod3b,'mod3c'=mod3c) #,'mod4a'=mod4a,'mod4b'=mod4b, 'mod5a'=mod5a, 'mod5b'=mod5b )
}