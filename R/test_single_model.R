
load.module('glm')
fun1 <- function( samp.size,repN){
sim.ds <- sim2[[samp.size]]
model.select = model_string_commensurate_gamma
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

return(posterior_samples.all)
}


parms <- pblapply(1:N.sim, function(x) lapply( 1:length(sim2),  function(y) fun1(samp.size=y,  repN=x))  ) 

tau_mean <- sapply(parms, function(x) sapply(x, function(y) mean(y[,'tau'])))

matplot(tau_mean, type='l')
tau_mean_mean <- apply(tau_mean,1,mean)
plot(tau_mean_mean, type='l')
#plot(posterior_samples.all[,'logit_alpha'], type='l')

delta_mean <- sapply(parms, function(x) sapply(x, function(y) mean(y[,'delta'])))
matplot(delta_mean, type='l')


beta1_mean <- sapply(parms, function(x) sapply(x, function(y) mean(y[,'beta1'])))
matplot(beta1_mean, type='l')

int_mean <- sapply(parms, function(x) sapply(x, function(y) mean(y[,'int'])))

plot(beta1_mean, tau_mean)
abline(v=log(1-0.4))

plot(beta1_mean, delta_mean)
plot(beta1_mean, int_mean)
plot(delta_mean, int_mean)


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

plot(posterior_samples.all[,'int'], posterior_samples.all[,'beta1'], ylim=c(-3,3), xlim=c(-3,3))
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