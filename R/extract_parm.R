extract_parm <-  function(mod.ds,mod.version='mod1a', samp_size_selecter=1, parm.select='beta1'){
  cater.df.list <- sapply(all.mods,'[[',mod.version, simplify=F) %>%
    lapply( function(x){ 
      bind_rows(x)
    })
  
  cater.df <- cater.df.list[[samp_size_selecter]] %>%
    filter(parm==parm.select)
 return(cater.df) 
}