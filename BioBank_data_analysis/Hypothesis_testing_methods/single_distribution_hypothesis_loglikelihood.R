# Likelihood computation for single distribution hypothesis
singleDistributionHypothesisLoglikelihood <- function(feature_data)
{
  null_mean <- mean(feature_data)
  null_sd <- sd(feature_data)
  null_hypothesis_llk <- sum(log(dnorm(feature_data,null_mean,null_sd,F)))
  return(null_hypothesis_llk)
}
