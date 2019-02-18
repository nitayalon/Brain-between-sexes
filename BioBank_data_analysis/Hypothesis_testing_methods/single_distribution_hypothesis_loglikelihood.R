# Likelihood computation for single distribution hypothesis
singleDistributionHypothesisLoglikelihood <- function(feature_data)
{
  null_mean <- mean(feature_data)
  null_sd <- sd(feature_data)
  null_hypothesis_llk <- sum(dnorm(feature_data,null_mean,null_sd,T))
  return(null_hypothesis_llk)
}
