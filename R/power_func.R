#Power for a fixed sample size
power.func <- function(set.alpha=0.05){ 
  power <- readRDS( './Results/all.mods_novavax.rds') %>%
  rename(parm_mean=mean) %>%
  mutate(  parm_mean = if_else( parm_mean< (-4), (-4),
                                if_else(parm_mean> 4,4, parm_mean)),
           lcl = if_else(lcl< (-4), (-4),
                         if_else(lcl>4,4, lcl)),
           ucl = if_else(ucl< (-4), (-4),
                         if_else(ucl>4,4, ucl))

  ) %>%
  group_by(repN, pop, ve.new.trial,Pooling_type,prior.info) %>%
  filter(parm=='beta1')  %>%
  ungroup() %>%
  arrange(repN, ve.new.trial,prior.info ,pop) %>%
  group_by(repN,  ve.new.trial,prior.info) %>%
  mutate( eff_0 = if_else(p_0>(1-set.alpha),1,0),
          eff_5 = if_else(p_0_95>(1-set.alpha),1,0),
          eff_10 = if_else(p_0_90>(1-set.alpha),1,0),
          eff_15 = if_else(p_0_85>(1-set.alpha),1,0),
          eff_20 = if_else(p_0_80>(1-set.alpha),1,0),
          eff_25 = if_else(p_0_75>(1-set.alpha),1,0),
          eff_30 = if_else(p_0_70>(1-set.alpha),1,0),
          futile_30 = if_else(p_futile>(1-set.alpha),1,0),
  ) %>%
  ungroup() %>%
  group_by(pop, ve.new.trial,prior.info) %>%
  summarize(prop_eff_0 = mean(eff_0),
            prop_eff_5 = mean(eff_5),
            prop_eff_10 = mean(eff_10),
            prop_eff_15 = mean(eff_15),
            prop_eff_20 = mean(eff_20),
            prop_eff_25 = mean(eff_25),
            prop_eff_30 = mean(eff_30),
            prop_futile_30 = mean(futile_30),
  )%>%
  mutate(alpha=set.alpha) %>%
  reshape2::melt(id.vars=c('pop','ve.new.trial','prior.info')) %>%
  rename(proportion=value, outcome=variable) %>%
  mutate( outcome= factor(outcome, c('prop_eff_0',     
                                     'prop_eff_5',
                                     'prop_eff_10',
                                     'prop_eff_15',
                                     'prop_eff_20',
                                     'prop_eff_25',
                                     'prop_eff_30',
                                     'prop_futile_30'), labels=c('VE>0%','VE>5%','VE>10%','VE>15%','VE>20%','VE>25%','VE>30%','Futile: VE<30%'))
  )
return(power)
}


cum_power_func <- function(parms){
  parms <- as.numeric(parms)
  cum_power <-
  readRDS( './Results/all.mods_novavax.rds') %>%
  reshape2::melt(id.vars=c('pop','ve.new.trial','prior.info','repN')) %>%
   rename(outcome=variable) %>%
  filter(pop %in% seq(from=parms[1], to=2000, by=parms[1])) %>%
  arrange(repN,outcome, ve.new.trial,prior.info ,pop) %>%
  group_by(repN, outcome, ve.new.trial,prior.info) %>%
  mutate( eff = 1*(value > (1-parms[2])),
          stop = cumsum(eff) ,
          stop = if_else(stop>=1,1,0)) %>%
  ungroup() %>%
  group_by(outcome,pop, ve.new.trial,prior.info) %>%
  summarize(prop_stopped = mean(stop))    %>%
    mutate(alpha=parms[2], look_freq=parms[1],
           outcome= factor(outcome, c('prop_eff_0',     
                 'prop_eff_5',
                 'prop_eff_10',
                 'prop_eff_15',
                 'prop_eff_20',
                 'prop_eff_25',
                 'prop_eff_30',
                 'prop_futile_30'), 
                 labels=c('VE>0%','VE>5%','VE>10%','VE>15%','VE>20%','VE>25%','VE>30%','Futile: VE<30%'))
    )

}

