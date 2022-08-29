call_jags <- function(sim.ds, repN=1, model.select=model_string_basic_pois){

  #Select replicate from the data generated in sim.data.R
  vax.status=sim.ds$vax[sim.ds$rep==repN]

  pop=sim.ds$pop[sim.ds$rep==repN]
  
  N_cases=sim.ds$N_cases[sim.ds$rep==repN]


##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
#inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
#inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')


##############################################
#Model Organization
##############################################
model_spec<-textConnection(model.select)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1), #,inits2, inits3),
                       data=list('N_cases'=N_cases,
                                 'vax'=vax.status, #could use whole dataset but would take a long time
                                 'pop'=pop,
                                 'N_cases_orig'=prior.data$N_cases_orig,
                                 'pop_orig'= prior.data$pop_orig
                                
                       ),
                       n.adapt=5000, 
                       n.chains=1, quiet=T)


params<-c('int', 'beta1', 'delta', 'tau')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=20000,quiet=T,progress.bar='none')

posterior_samples.all<-do.call(rbind,posterior_samples)

post_means<-apply(posterior_samples.all, 2, mean)
sample.labs<-names(post_means)
ci<-t(hdi(posterior_samples.all, credMass = 0.95))
#ci<-matrix(sprintf("%.1f",round(ci,1)), ncol=2)
ci<-matrix(ci, ncol=2)

row.names(ci)<-sample.labs
#post_means<-sprintf("%.1f",round(post_means,1))
names(post_means)<-sample.labs

post_var <- apply(posterior_samples.all,2, var)

combined <- cbind.data.frame(post_means, ci ,post_var, repN,names(post_means))
names(combined) <- c('mean','lcl','ucl', 'var', 'repN', 'parm')

#post_beta <- combined[grep('beta',names(post_means)),]

return(combined)
}