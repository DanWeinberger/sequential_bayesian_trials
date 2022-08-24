prior.data.func <- function( ve.obs.mean=39.4, ve.obs.lcl=5.3, ve.obs.ucl=61.2, N_cases_orig=prior.data$N_cases_orig, pop_orig=prior.data$pop_orig){ #mean and 95% CI (note the 97.5% CI was used for evaluationpurposes due to spending rule)
  ve.obs <- c(ve.obs.mean, ve.obs.lcl, ve.obs.ucl) #mean and 95% CI (note the 97.5% CI was used for evaluationpurposes due to spending rule)
  irr.obs <- 1-ve.obs/100
  
  log_irr.obs <- log(irr.obs)
  
  #compare mean to upper and lower CI, and take mean of that
  se.obs <- mean(c( abs(log_irr.obs[1] - log_irr.obs[3]), abs(log_irr.obs[1] - log_irr.obs[2])))/1.96
  
  prec.log.irr.obs <- 1/se.obs^2
  
  out.list=list('ve.obs.mean'=ve.obs.mean,'log_irr.obs'=log_irr.obs,'prec.log.irr.obs'=prec.log.irr.obs, 'N_cases_orig'=N_cases_orig,'pop_orig'=pop_orig)
  return(out.list)
}
