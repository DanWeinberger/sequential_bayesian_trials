caterplot.func <- function(mod.ds){
cater.df <- lapply(mod.ds, function(x) as.data.frame(matrix( unlist(x['beta', c('mean','lcl','ucl')]), ncol=3))) %>%
  bind_rows() %>%
  rename(mean=V1, lcl=V2, ucl=V3) %>%
  mutate(repN=row_number())


pc1 <- ggplot(cater.df, aes(y=repN, x=mean)) +
  geom_point() + 
  theme_classic()+
  xlim(-2, 2) + 
  geom_pointrange(aes(xmax=ucl, xmin=lcl))+
  geom_vline(xintercept=0, lty=2, col='gray80')
return(pc1)
}