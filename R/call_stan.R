library(rstan)
source('./R/stan_modified_pois_commensurate_gamma.R')

prior.mean=prior.data$log_irr.obs[1]
prior.prec=1/1.821^2
model.select=model_string_commensurate_gamma
set_tau_shp=0.01
set_tau_rate=0.01
prior.sd.upper=999

sim.ds <- mod.ds1 %>%
  filter(repN==1 & ve.new.trial==44.4 & pop==1000)
#call_stan <- function(sim.ds, prior.mean=999, set_tau_shp=999, set_tau_rate=999, prior.prec=999, prior.sd.upper=999, model.select=model_string_basic_pois){
  
#Select replicate from the data generated in sim.data.R
vax.status=sim.ds$vax

pop=sim.ds$pop

N_cases=sim.ds$N_cases


fit = stan(model_code=stan1, data=list('N_cases'=N_cases,
                                       'N'=length(N_cases),
                                       'vax'=vax.status, #could use whole dataset but would take a long time
                                       'log_pop'=log(pop/10),
                                       'prior_mean_log_irr'=prior.mean,
                                       'prior_prec_log_irr'=prior.prec,
                                       'N_cases_orig'=prior.data$N_cases_orig,
                                       'log_pop_orig'= log(prior.data$pop_orig/10),
                                       'set_tau_shp'= set_tau_shp,
                                       'set_tau_rate'=set_tau_rate,
                                       'sd.upper'=prior.sd.upper), iter=10000, chains=1)
}


