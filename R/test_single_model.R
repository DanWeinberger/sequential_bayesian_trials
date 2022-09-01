

fun1 <- function( samp.size,repN){
sim.ds <- sim2[[samp.size]]
model.select = model_string_modified_pois_mixture
#Select replicate from the data generated in sim.data.R
vax.status=sim.ds$vax[sim.ds$rep %in% repN]


prior.mean=prior.data$log_irr.obs[1]
prior.prec=prior.data$prec.log.irr.obs

pop=sim.ds$pop[sim.ds$rep %in% repN]

N_cases=sim.ds$N_cases[sim.ds$rep %in% repN]


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


params<-c('int', 'beta1', 'delta', 'tau', 'alpha','eta', 'mix.select')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=10000,quiet=T,progress.bar='none')

posterior_samples.all<-do.call(rbind,posterior_samples)

return(posterior_samples.all[,'alpha'])
}


alphas <- pblapply(1:N.sim, function(x) lapply( 1:length(sim2),  function(y) fun1(samp.size=y,  repN=x))  ) 



#plot(posterior_samples.all[,'logit_alpha'], type='l')

plot(posterior_samples.all[,'alpha'], type='l')
hist(posterior_samples.all[,'alpha'])


mean(posterior_samples.all[1:2000,'alpha'])
mean(posterior_samples.all[8:10000,'alpha'])

plot(posterior_samples.all[,'tau'], type='l')
hist(posterior_samples.all[,'tau'])
mean(posterior_samples.all[1:2000,'tau'])
mean(posterior_samples.all[8:10000,'tau'])


mean(posterior_samples.all[,'mix.select'])


plot(posterior_samples.all[,'delta'], type='l', ylim=c(-1.0,1.0))
exp(mean(posterior_samples.all[,'delta'])) #well-estimated without bias


plot(posterior_samples.all[,'beta1'], type='l', ylim=c(-1.0,1.0))
exp(mean(posterior_samples.all[,'beta1']))

plot(exp(posterior_samples.all[,'eta0']), type='l')


plot(posterior_samples.all[,'beta1'], posterior_samples.all[,'delta'], ylim=c(-3,3), xlim=c(-3,3))
abline(h=0, v=0)

plot(posterior_samples.all[,'beta1'], posterior_samples.all[,'alpha'], ylim=c(-3,3), xlim=c(-3,3))
abline(h=0, v=0)

plot(posterior_samples.all[,'delta'], posterior_samples.all[,'alpha'], ylim=c(-3,3), xlim=c(-3,3))
abline(h=0, v=0)

#hist(posterior_samples.all[,'alpha']*posterior_samples.all[,'eta'])


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