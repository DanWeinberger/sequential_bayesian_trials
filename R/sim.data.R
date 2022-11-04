sim.data.func <- function(N.vax = 3045,
                     N.control = 1581,
                     rate.control = 0.024,
                     ve.new.trial=prior.data$ve.obs.mean,
                     n.sim=3){
  
  irr.new.trial <- 1 - ve.new.trial/100
  
  sim1.control <- rbinom(n.sim,N.control, rate.control) 
  sim1.vax <- rbinom(n.sim,N.vax, rate.control*irr.new.trial) 
  
  N_cases <- c(sim1.control,sim1.vax)
  
  pop <- c(rep(N.control, n.sim),rep(N.vax, n.sim))
  
  vax <- c(rep(0, n.sim),rep(1, n.sim))
  
  rep <- rep( 1:n.sim, times=2)
  
  sim1.combined <- cbind.data.frame('rep'=rep,'vax'=vax, 'N_cases'= N_cases, 'pop'=pop)
  
  sim1.combined <- sim1.combined %>%
    arrange(rep) %>%
    mutate(ve.new.trial=ve.new.trial)
  return(sim1.combined)
}
