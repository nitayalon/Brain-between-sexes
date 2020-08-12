#' Method helper for equal proportions EM

computeEqualProprtionsEM <- function(feature){
  centers_for_em <- feature %>% 
    group_by(sex) %>% 
    select(sex, value) %>% 
    summarise(avg = mean(value), std = sd(value))
  equal_proportion_em <- normalmixEM(feature$value, lambda = 0.5, mu = centers_for_em$avg, sigma = centers_for_em$std, maxit = 10000)
  return(equal_proportion_em)
}
