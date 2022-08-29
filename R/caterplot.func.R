caterplot.func <- function(mod.ds,mod.version='mod1a', samp_size_selecter=1, parm.select='beta1'){
cater.df.list <- sapply(all.mods,'[[',mod.version, simplify=F) %>%
  lapply( function(x){ 
    bind_rows(x)
  })

cater.df <- cater.df.list[[samp_size_selecter]] %>%
  filter(parm==parm.select)
power <- mean(cater.df$lcl >0)

pc1 <- ggplot(cater.df, aes(y=repN, x=mean)) +
  geom_point() + 
  theme_classic()+
  xlim(-2, 2) + 
  geom_pointrange(aes(xmax=ucl, xmin=lcl))+
  geom_vline(xintercept=0, lty=2, col='gray80') +
  ggtitle(paste0("Power ", round(100*power), '%'))
return(pc1)
}