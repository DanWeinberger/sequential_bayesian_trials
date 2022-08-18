prior_post_compare <- function(ds){
  x <- seq(-4, 4, length=100)
  prior.y <- dnorm(x, mean=ds['beta1','prior.mean'], sd=sqrt(1/ds['actual_prior_prec','mean']))
  
  meanpost <- ds['beta1','mean']
  
  sdpost <- sqrt(ds['beta1','var'])
  
  ypost <- dnorm(x, mean=meanpost, sd=sdpost)
  
  density_df <-  cbind.data.frame(x, prior.y, ypost)
  
  p1 <- ggplot(density_df, aes(x=x, y=prior.y),color='gray80') +
    geom_line() +
    theme_classic() +
    geom_line(aes(x=x, y=ypost),color='red')
  return(p1)
}