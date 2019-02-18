# This code implements the MStep in the EM algorithm for
# double-double mixture model
MStep <- function(parameters
                  ,observations
                  ,do_not_ignore_NA = T,
                  warn = NULL, ...)
{
  # the parameters from iteration (t-1) are stored in a list
  stopifnot(class(parameters) == "list")  
  # the parameters from iteration (t-1) are 2
  stopifnot(length(names(parameters)) == 2)  
  # Validate that all the parameters are in the parameter list and are valid
  if(do_not_ignore_NA)
  {
    stopifnot(validateMStepParameters(parameters))  
    I <- parameters$I; J <- parameters$J
    men <- observations$men
    women <- observations$women
  }
  else
  {
    I <- parameters$I[!is.na(parameters$I)]
    J <- parameters$J[!is.na(parameters$J)]
    men <- observations$men[!is.na(parameters$I)]
    women <- observations$women[!is.na(parameters$J)]
  }
  
  m <- length(men); n <- length(women)
  # M-Step
  p <- mean(I)
  q <- mean(J)
  mu_1 <- as.numeric((I %*% men + J %*% women) / (sum(I) + sum(J)))
  mu_2 <- as.numeric((sum(men) + sum(women) - (I %*% men + J %*% women)) / 
    (m + n - (sum(I) + sum(J))))
  sigma_2 <- as.numeric((I %*% (men - mu_1)^2 + (1-I) %*% (men - mu_2)^2 + 
                J %*% (women - mu_1)^2 + (1-J) %*% (women - mu_2)^2) / 
    (m + n))
  
  m_step <- list(p = p, q = q, mu_1 = mu_1, mu_2 = mu_2, sigma_2 = sigma_2)
  return(m_step)
}