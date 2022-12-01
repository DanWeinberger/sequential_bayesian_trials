sim.data.func <- function(N.vax = 10000,
                     N.control = 10000,
                     rate.control = 0.024,
                     ve.new.trial=prior.data$ve.obs.mean,
                     n.sim=3){
  
  irr.new.trial <- 1 - ve.new.trial/100
  

  all.reps <- lapply(1:n.sim, which_cases, N.control=N.control, N.vax=N.vax, irr.new.trial=irr.new.trial, rate.control=rate.control)
  
  sim1.combined <- bind_rows(all.reps)
  
  sim1.combined <- sim1.combined %>%
    arrange(repN) %>%
    mutate(ve.new.trial=ve.new.trial)
  return(sim1.combined)
}


#This function simulates the data and determines which subjects become cases
## for a single trial,generate binary outcome for each person and identify which person it is...
#it is necessary to do it this way so that we know specifically which people are the cases for 
#the adaptive evaluation

which_cases <- function(Nsim, N.control,N.vax, irr.new.trial, rate.control){
  sim1.control <- rbinom(N.control,1, rate.control) 
  sim1.control.cases <- which(sim1.control==1) #Which subjects in placebo group become cases?
  df1 <- cbind.data.frame('ID'=sim1.control.cases, 'treatment'='placebo', 'repN'=Nsim)
  
  sim1.vax <- rbinom(N.vax,1,  rate.control*irr.new.trial) 
  sim1.vax.cases <- which(sim1.vax==1) #Which subjects in placebo group become cases?
  df2 <- cbind.data.frame('ID'=sim1.vax.cases, 'treatment'='vax', 'repN'=Nsim)
  
  out.df <- bind_rows(df1,df2)
  return(out.df)
}
