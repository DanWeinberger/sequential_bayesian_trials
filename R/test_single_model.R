
repN=1
sim.ds <- sim2[[12]]
model.select = model_string_modified_pois_mixture
#Select replicate from the data generated in sim.data.R
vax.status=sim.ds$vax[sim.ds$rep==repN]
prior.mean=prior.data$log_irr.obs[1]
prior.prec=prior.data$prec.log.irr.obs

pop=sim.ds$pop[sim.ds$rep==repN]

N_cases=sim.ds$N_cases[sim.ds$rep==repN]


##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')


##############################################
#Model Organization
##############################################
model_spec<-textConnection(model.select)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1), #,inits2, inits3),
                       data=list('N_cases'=N_cases,
                                 'vax'=vax.status, #could use whole dataset but would take a long time
                                 'pop'=pop,
                                 'log_irr.obs'=prior.mean,
                                 'prec.log.irr.obs'=prior.prec,
                                 'N_cases_orig'=prior.data$N_cases_orig,
                                 'pop_orig'= prior.data$pop_orig
                       ),
                       n.adapt=5000, 
                       n.chains=1, quiet=T)


params<-c('int', 'beta1', 'delta', 'tau', 'alpha','eta')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000,quiet=T,progress.bar='none')

posterior_samples.all<-do.call(rbind,posterior_samples)

#plot(posterior_samples.all[,'logit_alpha'], type='l')

plot(posterior_samples.all[,'alpha'], type='l')
plot(posterior_samples.all[,'delta'], type='l', ylim=c(-1.0,1.0))

plot(posterior_samples.all[,'beta1'], type='l', ylim=c(-1.0,1.0))
plot(posterior_samples.all[,'eta'], type='l', ylim=c(-1.0,1.0))


post_means<-apply(posterior_samples.all, 2, mean)
sample.labs<-names(post_means)
ci<-t(hdi(posterior_samples.all, credMass = 0.95))
#ci<-matrix(sprintf("%.1f",round(ci,1)), ncol=2)
ci<-matrix(ci, ncol=2)

row.names(ci)<-sample.labs
#post_means<-sprintf("%.1f",round(post_means,1))
names(post_means)<-sample.labs

post_var <- apply(posterior_samples.all,2, var)

combined <- cbind.data.frame(post_means, ci ,post_var,prior.mean, repN,names(post_means))
names(combined) <- c('mean','lcl','ucl', 'var', 'prior.mean', 'repN', 'parm')

#post_beta <- combined[grep('beta',names(post_means)),]