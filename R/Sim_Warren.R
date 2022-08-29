#######################
#Global Settings
#######################
n_sim<-250
n_h<-c(26,   #Control
       23)   #Treatment
n_c<-c(240,  #Control
       249)  #Treatment

##########################################################################################
#Setting 1:  All Parameters Equal
##########################################################################################
set.seed(13529)
beta0_h<-log((6/26)/(1.00 - (6/26)))  #Observed Proportion in Historical Control Group
beta0_c<-log((6/26)/(1.00 - (6/26)))
beta1_h<-log((8/23)/(1.00 - (8/23))) -  #Observed Proportion in Historical Treatment Group
  log((6/26)/(1.00 - (6/26)))
beta1_c<-log((8/23)/(1.00 - (8/23))) -
  log((6/26)/(1.00 - (6/26)))
sim_data<-list()
y_h<-rep(NA,
         times = 2)
y_c<-rep(NA, 
         times = 2)
for(j in 1:n_sim){
  p0<-1.00/(1.00 + exp(-beta0_h))
  y_h[1]<-rbinom(n = 1,
                 size = n_h[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_h - beta1_h))
  y_h[2]<-rbinom(n = 1,
                 size = n_h[2],
                 prob = p1)
  p0<-1.00/(1.00 + exp(-beta0_c))
  y_c[1]<-rbinom(n = 1,
                 size = n_c[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_c - beta1_c))
  y_c[2]<-rbinom(n = 1,
                 size = n_c[2],
                 prob = p1)
  sim_data[[j]]<-list("beta0_h" = beta0_h,
                      "beta1_h" = beta1_h,
                      "beta0_c" = beta0_c,
                      "beta1_c" = beta1_c,
                      "y_h" = y_h,
                      "y_c" = y_c,
                      "n_h" = n_h,
                      "n_c" = n_c)
}
#############################################################################################
#Setting 2:  \beta1_c != \beta1_h
#############################################################################################
set.seed(19863)
beta0_h<-log((6/26)/(1.00 - (6/26)))  #Observed Proportion in Historical Control Group
beta0_c<-log((6/26)/(1.00 - (6/26)))
beta1_h<-log((8/23)/(1.00 - (8/23))) -  #Observed Proportion in Historical Treatment Group
  log((6/26)/(1.00 - (6/26)))
beta1_c<-log((110/249)/(1.00 - (110/249))) -  #Observed Proportion in Current Treatment Group
  log((6/26)/(1.00 - (6/26)))
sim_data<-list()
y_h<-rep(NA,
         times = 2)
y_c<-rep(NA, 
         times = 2)
for(j in 1:n_sim){
  p0<-1.00/(1.00 + exp(-beta0_h))
  y_h[1]<-rbinom(n = 1,
                 size = n_h[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_h - beta1_h))
  y_h[2]<-rbinom(n = 1,
                 size = n_h[2],
                 prob = p1)
  p0<-1.00/(1.00 + exp(-beta0_c))
  y_c[1]<-rbinom(n = 1,
                 size = n_c[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_c - beta1_c))
  y_c[2]<-rbinom(n = 1,
                 size = n_c[2],
                 prob = p1)
  sim_data[[j]]<-list("beta0_h" = beta0_h,
                      "beta1_h" = beta1_h,
                      "beta0_c" = beta0_c,
                      "beta1_c" = beta1_c,
                      "y_h" = y_h,
                      "y_c" = y_c,
                      "n_h" = n_h,
                      "n_c" = n_c)
}
##########################################################################################
#Setting 3:  \beta0_c != \beta0_h
##########################################################################################
set.seed(12214)
beta0_h<-log((6/26)/(1.00 - (6/26)))  #Observed Proportion in Historical Control Group
beta0_c<-log((100/240)/(1.00 - (100/240)))  #Observed Proportion in Current Control Group
beta1_h<-log((8/23)/(1.00 - (8/23))) -  #Observed Proportion in Historical Treatment Group
  log((6/26)/(1.00 - (6/26)))
beta1_c<-log((8/23)/(1.00 - (8/23))) -
  log((6/26)/(1.00 - (6/26)))
sim_data<-list()
y_h<-rep(NA,
         times = 2)
y_c<-rep(NA, 
         times = 2)
for(j in 1:n_sim){
  p0<-1.00/(1.00 + exp(-beta0_h))
  y_h[1]<-rbinom(n = 1,
                 size = n_h[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_h - beta1_h))
  y_h[2]<-rbinom(n = 1,
                 size = n_h[2],
                 prob = p1)
  p0<-1.00/(1.00 + exp(-beta0_c))
  y_c[1]<-rbinom(n = 1,
                 size = n_c[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_c - beta1_c))
  y_c[2]<-rbinom(n = 1,
                 size = n_c[2],
                 prob = p1)
  sim_data[[j]]<-list("beta0_h" = beta0_h,
                      "beta1_h" = beta1_h,
                      "beta0_c" = beta0_c,
                      "beta1_c" = beta1_c,
                      "y_h" = y_h,
                      "y_c" = y_c,
                      "n_h" = n_h,
                      "n_c" = n_c)
}
#############################################################################################
#Setting 4:  \beta0_c != \beta0_h, \beta1_c != \beta1_h
#############################################################################################
set.seed(15611)
beta0_h<-log((6/26)/(1.00 - (6/26)))  #Observed Proportion in Historical Control Group
beta0_c<-log((100/240)/(1.00 - (100/240)))  #Observed Proportion in Current Control Group
beta1_h<-log((8/23)/(1.00 - (8/23))) -  #Observed Proportion in Treatment Group
  log((6/26)/(1.00 - (6/26)))
beta1_c<-log((110/249)/(1.00 - (110/249))) -  #Observed Proportion in Current Treatment Group
  log((100/240)/(1.00 - (100/240)))
sim_data<-list()
y_h<-rep(NA,
         times = 2)
y_c<-rep(NA, 
         times = 2)
for(j in 1:n_sim){
  p0<-1.00/(1.00 + exp(-beta0_h))
  y_h[1]<-rbinom(n = 1,
                 size = n_h[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_h - beta1_h))
  y_h[2]<-rbinom(n = 1,
                 size = n_h[2],
                 prob = p1)
  p0<-1.00/(1.00 + exp(-beta0_c))
  y_c[1]<-rbinom(n = 1,
                 size = n_c[1],
                 prob = p0)
  p1<-1.00/(1.00 + exp(-beta0_c - beta1_c))
  y_c[2]<-rbinom(n = 1,
                 size = n_c[2],
                 prob = p1)
  sim_data[[j]]<-list("beta0_h" = beta0_h,
                      "beta1_h" = beta1_h,
                      "beta0_c" = beta0_c,
                      "beta1_c" = beta1_c,
                      "y_h" = y_h,
                      "y_c" = y_c,
                      "n_h" = n_h,
                      "n_c" = n_c)
}
