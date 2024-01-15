#' Forcing residuals to follow a noraml distirbution by replacing their values with the
#' proper standard normal quantile
#' @param residuals - linear model residuals
#' @return standard normally distributed residuals

normalizeResiduales <- function(residuals, use_standard_data = F)
{
  residuals_location <- order(residuals)
  
  n <- length(residuals)
  
  standard_normal <- qnorm(residuals_location / (n+1))
  
  if(use_standard_data)
  {
    residuals <- standard_normal
  }
  else
  {
    residuals <- (residuals - mean(residuals)) / 
      sd(residuals)
  }
  return(residuals)
}