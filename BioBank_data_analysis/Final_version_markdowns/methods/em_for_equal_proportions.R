#' Method helper for equal proportions EM

computeEqualProprtionsEM <- function(feature, not_equal_var = T){
  browser()	
  centers_for_em <- feature %>% 
    group_by(sex) %>% 
    select(sex, value) %>% 
    summarise(avg = mean(value), std = sd(value))
  if(not_equal_var)
{
  equal_proportion_em <- normalmixEM(feature$value, lambda = 0.5, mu = centers_for_em$avg, sigma = centers_for_em$std, maxit = 10000)
}else{
  equal_proportion_em <- normalmixEM(feature$value, lambda = 0.5, mu = centers_for_em$avg, sigma = NULL, maxit = 10000, arbvar = F)  
}
  return(equal_proportion_em)
}
