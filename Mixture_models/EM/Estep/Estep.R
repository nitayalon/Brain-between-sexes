# This code implements the EStep in the EM algorithm for
# double-double mixture model
EStep <- function(parameters, observations, warn = NULL, ...)
{
  # the parameters from iteration (t-1) are stored in a list
  stopifnot(class(parameters) == "list")  
  # the parameters from iteration (t-1) are 5
  stopifnot(length(names(parameters)) == 5)  
  # Validate that all the parameters are in the parameter list and are valid
  stopifnot(validateEStepParameters(parameters))
  
  p <- parameters$p; q <- parameters$q; mu_1 <- parameters$mu_1;
  mu_2 <- parameters$mu_2; sigma <- sqrt(parameters$sigma_2)
  
  men <- observations$men
  women <- observations$women
  
  I <- computeConditionalExpectation(men,p,mu_1,mu_2,sigma)
  J <- computeConditionalExpectation(women,q,mu_1,mu_2,sigma)
  
  e_step <- list(I = I, J = J)
  return(e_step)
}

