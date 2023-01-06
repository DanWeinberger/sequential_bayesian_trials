call_jags <- function(sim.ds, prior.mean=999, set_tau_shp=999, set_tau_rate=999, prior.prec=999, prior.sd.upper=999, model.select=model_string_basic_pois){

##See 3.3 for section on selecting priors: https://hbiostat.org/proj/covid19/bayesplan.html
 # ptm <- proc.time()
  
  #Select replicate from the data generated in sim.data.R
  vax.status=sim.ds$vax

  pop=sim.ds$pop_grp
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
                                 'prior_mean_log_irr'=prior.mean,
                                 'prior_prec_log_irr'=prior.prec,
                                 'N_cases_orig'=prior.data$N_cases_orig,
                                 'pop_orig'= prior.data$pop_orig,
                                 'set_tau_shp'= set_tau_shp,
                                  'set_tau_rate'=set_tau_rate,
                                 'sd.upper'=prior.sd.upper
                       ),
                       n.adapt=10000, 
                       n.chains=1, quiet=T)


params<-c('int', 'beta1', 'delta', 'tau', 'alpha')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=1000,quiet=T,progress.bar='none')

posterior_samples.all<-do.call(rbind,posterior_samples)

##proc.time() - ptm

#ptm <- proc.time()

post_means<-apply(posterior_samples.all, 2, mean)
sample.labs<-names(post_means)
ci<-t(hdi(posterior_samples.all, credMass = 0.95))
ci<-matrix(ci, ncol=2)

ci975<-t(hdi(posterior_samples.all, credMass = 0.975))
ci975<-matrix(ci975, ncol=2)

#proc.time() - ptm

#ptm <- proc.time()

#criteria from Frank Harrell: https://hbiostat.org/proj/covid19/bayesplan.html
p_0 <- mean(exp(posterior_samples.all[,'beta1']) < 1)
#p_0_95 <- mean(exp(posterior_samples.all[,'beta1']) < 0.95)
#p_0_90 <- mean(exp(posterior_samples.all[,'beta1']) < 0.90)
#p_0_85 <- mean(exp(posterior_samples.all[,'beta1']) < 0.85)
p_0_80 <- mean(exp(posterior_samples.all[,'beta1']) < 0.80)
p_0_75 <- mean(exp(posterior_samples.all[,'beta1']) < 0.75)
p_0_70 <- mean(exp(posterior_samples.all[,'beta1']) < 0.70)

#p_harm_o1 <- mean(exp(posterior_samples.all[,'beta1']) >1)
#p_similar_0_8__1_25 <- mean(exp(posterior_samples.all[,'beta1']) <1.30 & exp(posterior_samples.all[,'beta1']) >0.8)
#p_futile <- mean(exp(posterior_samples.all[,'beta1']) > 0.7 ) #if VE<30%, it is futile

#proc.time() - ptm
#ptm <- proc.time()

row.names(ci)<-sample.labs
#post_means<-sprintf("%.1f",round(post_means,1))
names(post_means)<-sample.labs

post_var <- apply(posterior_samples.all,2, var)


combined <- tibble(post_means, ci ,
                             ci975,
                             post_var, 
                             parm=names(post_means),
                             repN=unique(sim.ds$repN),
                             pop=unique(sim.ds$pop),
                             N_event_look=unique(sim.ds$N_event_look),
                             ve.new.trial=unique(sim.ds$ve.new.trial),
                             p_0=p_0,
                             p_0_80=p_0_80,
                             p_0_75=p_0_75,
                             p_0_70=p_0_70,
)
#proc.time() - ptm

return(combined)
}