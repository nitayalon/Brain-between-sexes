computeGradientForMixtureModel <- function(observations, MLE,...)
{
  epsilon <- delta <- 1e-4
  women <- observations %>% 
    filter(sex == 0) %>% 
    select(value)
  men <- observations %>% 
    filter(sex == 1) %>% 
    select(value)
  llk_function <- function(men, women,theta_mas, theta_fem, p, q, sigma_2_men, sigma_2_women)
  {
    men_llk <- sum(log(p * dnorm(men, theta_mas, sqrt(sigma_2_men), log = F) + 
                         (1 - p) * dnorm(men, theta_fem, sqrt(sigma_2_women), log = F)))
    women_llk <- sum(log(q * dnorm(women, theta_mas, sqrt(sigma_2_men), log = F) +
                           (1-q) * dnorm(women, theta_fem, sqrt(sigma_2_women), log = F)))
    return(men_llk + women_llk)
  }
  n_params <- length(MLE)
  MLE_vector <- sapply(MLE, function(x){x})
  gradeint <- matrix(NA, nrow = n_params, ncol = n_params)
  colnames(gradeint) <- names(MLE)
  rownames(gradeint) <- names(MLE)
  center_llk <- llk_function(men$value, women$value, 
                             MLE["mu_1"][[1]],
                             MLE["mu_2"][[1]],
                             MLE["p"][[1]],
                             MLE["q"][[1]],
                             MLE["sigma_2_men"][[1]],
                             MLE["sigma_2_women"][[1]])
  for (i in 1:length(names(MLE)))
  {
    for (j in i:length(names(MLE)))
    {
      param_1 = names(MLE)[i]
      param_2 = names(MLE)[j]
      if(param_1 == param_2)
      {
        jitter_minus <- -epsilon * as.numeric(names(MLE) == param_1)
        minus_parameters <- MLE_vector + jitter_minus
        minus_llk <- llk_function(men$value, women$value, 
                                  minus_parameters["mu_1"],
                                  minus_parameters["mu_2"],
                                  minus_parameters["p"],
                                  minus_parameters["q"],
                                  minus_parameters["sigma_2_men"],
                                  minus_parameters["sigma_2_women"])
        jitter_plus <- epsilon * as.numeric(names(MLE) == param_1)
        plus_parameters <- MLE_vector + jitter_plus
        plus_llk <- llk_function(men$value, women$value, 
                                 plus_parameters["mu_1"],
                                 plus_parameters["mu_2"],
                                 plus_parameters["p"],
                                 plus_parameters["q"],
                                 plus_parameters["sigma_2_men"],
                                 plus_parameters["sigma_2_women"])
        # second_derivative = (plus_llk + minus_llk - 2 * center_llk) / 
        #   (epsilon^2)
        second_derivative = (plus_llk - minus_llk) / 
          (2 * epsilon)
        gradeint[param_1,param_2] <- second_derivative
      }
      else
      {
        next
      }
    }  
  }
  return(diag(gradeint))
}