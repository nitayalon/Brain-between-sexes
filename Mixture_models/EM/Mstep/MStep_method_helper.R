# Method helper for MStep 
validateMStepParameters <- function(parameters)
{
  if(any(parameters$I < 0) || any(parameters$I > 1))
  {
    return(F)
  }
  if(any(parameters$J < 0) || any(parameters$J > 1))
  {
    return(F)
  }
  return(T)
}
