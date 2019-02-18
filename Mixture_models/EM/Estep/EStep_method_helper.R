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
  if(parameters$sigma_2 < 0)
  {
    return(F)
  }
  return(T)
}

computeConditionalExpectation <- function(observations, prop, mean_1, mean_2, std)
{
  cond_exp <- prop * dnorm(observations,mean_1,std) / 
    (prop * dnorm(observations,mean_1,std) + (1-prop)*dnorm(observations,mean_2,std))
  cond_exp[cond_exp = NA] <- 0
  return(cond_exp)
}