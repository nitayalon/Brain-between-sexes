computeMLEAndLlkRatioUnderHypothesis <- function(m,mu,p,q,n, just_sample = F){
  sigma_2 <- 1 + mu * -mu
  if(just_sample){
    sample <- sampleMockData(mu,-mu,p,q,sigma_2,m)
    norm_pop <- normalizingData(sample$men,sample$women)
    return(list(
      EDA = dataEDA(norm_pop),
      MLE_results = computeMLEOverGridMockData(norm_pop)
    ))
  }
  
  llk_ratio <- c()
  for(i in 1:n){
    sample <- sampleMockData(mu,-mu,p,q,sigma_2,m)
    norm_pop <- normalizingData(sample$men,sample$women)
    validation <- validateSample(norm_pop$men, norm_pop$women)
    MLE_results <- computeMLEOverGridMockData(norm_pop)
    llk_ratio <- c(llk_ratio, MLE_results$llk_ratio)
  }
  return(list(
    llk_ratio = llk_ratio,
    p_value = exp(-llk_ratio)
  ))
}

