#' Forcing residuals to follow a noraml distirbution by replacing their values with the
#' proper standard normal quantile
#' @param residuals - linear model residuals
#' @return standard normally distributed residuals

normalizeResiduales <- function(residuals_tibble, use_standard_data = F)
{
  residuals <- residuals_tibble$value
  
  residuals_location <- order(residuals)
  
  n <- length(residuals)
  
  standard_normal <- qnorm(residuals_location / (n+1))
  
  if(use_standard_data)
  {
    residuals_tibble$value <- standard_normal
  }
  else
  {
    residuals_tibble$value <- (residuals_tibble$value - mean(residuals_tibble$value)) / 
      sd(residuals_tibble$value)
  }
  return(residuals_tibble)
}