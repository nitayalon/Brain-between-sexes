computeLogLikelihood <- function(men,women,theta_mas, theta_fem, p, q, sigma_2)
{
  
  men_llk <- sum(log(p * dnorm(men, theta_mas, sqrt(sigma_2), log = F) + 
                       (1 - p) * dnorm(men, theta_fem, sqrt(sigma_2), log = F)))
  women_llk <- sum(log(q * dnorm(women, theta_mas, sqrt(sigma_2), log = F) +
                         (1-q) * dnorm(women, theta_fem, sqrt(sigma_2), log = F)))
  
  return(men_llk + women_llk)
}

computeLogLikelihoodFull <- function(observations,mle_parameters)
{
  men <- observations$men
  women <- observations$women
  mu_1 <- mle_parameters$mu_1
  mu_2 <- mle_parameters$mu_2
  p <-  mle_parameters$p
  q <-  mle_parameters$q 
  sigma_2 <- mle_parameters$sigma_2 
  
  men_llk <- sum(log(p * dnorm(men, mu_1, sqrt(sigma_2), log = F) + 
                       (1 - p) * dnorm(men, mu_2, sqrt(sigma_2), log = F)))
  women_llk <- sum(log(q * dnorm(women, mu_1, sqrt(sigma_2), log = F) +
                         (1-q) * dnorm(women, mu_2, sqrt(sigma_2), log = F)))
  
  return(men_llk + women_llk)
}

