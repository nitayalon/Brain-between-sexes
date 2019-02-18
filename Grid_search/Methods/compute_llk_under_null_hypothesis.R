computeLlkNullHypothesis <- function(mu,men_feature_data, women_feature_data){
  p <- 1
  q <- 0
  sigma2_men <- sigma2_women <-  1 + mu * -mu
  
  llk <- computeLogLikelihoodForFeature(men_feature_data, women_feature_data, mu, -mu,
                                 p,q,
                                 sigma2_men, sigma2_women)
  return(c(llk[1], llk[2]))
}
