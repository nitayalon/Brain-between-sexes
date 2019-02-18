checkBrainFeatureDataValidity <- function(object)
{
  errors <- character()
  
  if(any(object@value < 0))
  {
    msg <- "Brain measurements cannot be negative"
    errors <- c(errors,msg)
  }
  
  if(length(errors) == 0) TRUE else errors
}