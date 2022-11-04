caterplot.func <- function(mod.ds,mod.version='mod1a', parm.select='beta1'){

  ds <-  mod.ds %>%
  filter(parm==parm.select)
  
power <- mean(ds$ucl <0) 

pc1 <- ggplot(ds, aes(y=repN, x=mean)) +
  geom_point() + 
  theme_classic()+
  xlim(-4, 2) + 
  geom_pointrange(aes(xmax=ucl, xmin=lcl))+
  geom_vline(xintercept=0, lty=2, col='gray80') +
  ggtitle(paste0("Power ", round(100*power), '%'))
return(pc1)
}