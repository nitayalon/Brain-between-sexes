# Method helper for EStep 
validateEStepParameters <- function(parameters)
{
  if(parameters$p < 0 || parameters$p > 1)
  {
    return(F)
  }
  if(parameters$q < 0 || parameters$q > 1)
  {
    return(F)
  }
  if(parameters$sigma_2_men < 0)
  {
    return(F)
  }
  if(parameters$sigma_2_women < 0)
  {
    return(F)
  }
  return(T)
}

computeConditionalExpectation <- function(observations, prop, mean_1, mean_2, std_1, std_2)
{
  cond_exp <- prop * dnorm(observations,mean_1,std_1) / 
    (prop * dnorm(observations,mean_1,std_1) + (1-prop)*dnorm(observations,mean_2,std_2))
  return(cond_exp)
}