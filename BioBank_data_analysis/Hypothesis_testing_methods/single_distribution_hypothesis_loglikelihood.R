# Likelihood computation for single distribution hypothesis
singleDistributionHypothesisLoglikelihood <- function(feature_data)
{
  mean_and_std <- feature_data %>% 
    summarise(avg = mean(value),
              std = sd(value))
  null_hypothesis_llk <- sum(log(dnorm(feature_data$value, mean_and_std$avg,mean_and_std$std, F)))
  return(null_hypothesis_llk)
}
