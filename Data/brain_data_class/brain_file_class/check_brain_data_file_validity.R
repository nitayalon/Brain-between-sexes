checkBrainDataFileValidity <- function(object)
{
  errors <- character()
  
  if(!dir.exists(object@target_dir))
  {
    msg <- "The target dir doesn't exists"
    errors <- c(errors,msg)
  }
  
  # if(!dir.exists(object@destination_dir))
  # {
  #   msg <- "The destination dir doesn't exists"
  #   errors <- c(errors,msg)
  # }
  
  if(length(errors) == 0) TRUE else errors
}