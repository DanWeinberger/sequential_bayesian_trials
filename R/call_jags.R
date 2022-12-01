call_jags <- function(sim.ds, prior.mean=0, prior.prec=1e-4,  model.select=model_string_basic_pois){


  #Select replicate from the data generated in sim.data.R
  vax.status=sim.ds$vax

  pop=sim.ds$pop
  
  N_cases=sim.ds$N_cases


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
                                 'log_irr.obs'=prior.mean,
                                 'prec.log.irr.obs'=prior.prec,
                                 'N_cases_orig'=prior.data$N_cases_orig,
                                 'pop_orig'= prior.data$pop_orig
                       ),
                       n.adapt=5000, 
                       n.chains=1, quiet=T)


params<-c('int', 'beta1', 'delta', 'tau', 'alpha')

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
ci<-matrix(ci, ncol=2)

ci975<-t(hdi(posterior_samples.all, credMass = 0.975))
ci975<-matrix(ci975, ncol=2)


#criteria from Frank Harrell: https://hbiostat.org/proj/covid19/bayesplan.html
p_any_benefit0 <- mean(exp(posterior_samples.all[,'beta1']) < 1)
p_nontrivial_0_95 <- mean(exp(posterior_samples.all[,'beta1']) < 0.95)
p_moderate_0_75 <- mean(exp(posterior_samples.all[,'beta1']) < 0.75)
p_harm_o1 <- mean(exp(posterior_samples.all[,'beta1']) >1)
p_similar_0_8__1_25 <- mean(exp(posterior_samples.all[,'beta1']) <1.25 & exp(posterior_samples.all[,'beta1']) >0.8)

row.names(ci)<-sample.labs
#post_means<-sprintf("%.1f",round(post_means,1))
names(post_means)<-sample.labs

post_var <- apply(posterior_samples.all,2, var)

combined <- cbind.data.frame(post_means, ci ,ci975,post_var,prior.mean, names(post_means),'repN'=unique(sim.ds$repN),
                             'pop'=unique(sim.ds$pop),
                             've.new.trial'=unique(sim.ds$ve.new.trial),
                           'p_any_benefit0'=p_any_benefit0,
                           'p_nontrivial_0_95'=p_nontrivial_0_95,
                           'p_moderate_0_75'=p_moderate_0_75,
                           'p_harm_o1' =p_harm_o1,
                           'p_similar_0_8__1_25'=p_similar_0_8__1_25
                           
)
names(combined) <- c('mean','lcl','ucl','lcl975','ucl975', 'var', 'prior.mean',  'parm', 'repN','pop','ve.new.trial',
                     'p_any_benefit0','p_nontrivial_0_95','p_moderate_0_75','p_harm_o1','p_similar_0_8__1_25'
                     )


return(combined)
}