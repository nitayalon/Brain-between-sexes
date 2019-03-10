#' Forcing residuals to follow a noraml distirbution by replacing their values with the
#' proper standard normal quantile
#' @param residuals - linear model residuals
#' @return standard normally distributed residuals

normalizeResiduales <- function(residuals_tibble)
{
  residuals <- residuals_tibble$value
  residuals_location <- order(residuals)
  n <- length(residuals)
  standard_normal <- qnorm(residuals_location / (n+1))
  residuals_tibble$value <- residuals
  return(residuals_tibble)
}