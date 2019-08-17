computeHessianForMixtureModel <- function(observations, MLE,...)
{
  epsilon <- delta <- 1e-3
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
  hessian <- matrix(NA, nrow = n_params, ncol = n_params)
  colnames(hessian) <- names(MLE)
  rownames(hessian) <- names(MLE)
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
          second_derivative = (plus_llk + minus_llk - 2 * center_llk) /
            (epsilon^2)
          hessian[param_1,param_2] <- second_derivative
        }
      else{
        jitter_minus_minus <- -epsilon * as.numeric(names(MLE) %in% c(param_1,param_2))
        minus_minus_parameters <- MLE_vector + jitter_minus_minus
        minus_minus_llk <- llk_function(men$value, women$value, 
                                        minus_minus_parameters["mu_1"],
                                        minus_minus_parameters["mu_2"],
                                        minus_minus_parameters["p"],
                                        minus_minus_parameters["q"],
                                        minus_minus_parameters["sigma_2_men"],
                                        minus_minus_parameters["sigma_2_women"])
        jitter_plus_plus <- epsilon * as.numeric(names(MLE) %in% c(param_1,param_2))
        plus_plus_parameters <- MLE_vector + jitter_plus_plus
        plus_plus_llk <- llk_function(men$value, women$value, 
                                      plus_plus_parameters["mu_1"],
                                      plus_plus_parameters["mu_2"],
                                      plus_plus_parameters["p"],
                                      plus_plus_parameters["q"],
                                      plus_plus_parameters["sigma_2_men"],
                                      plus_plus_parameters["sigma_2_women"])
        jitter_plus_minus_a <- -epsilon * as.numeric(names(MLE) == param_1)
        jitter_plus_minus_b <- epsilon * as.numeric(names(MLE) == param_2)
        jitter_plus_minus <- jitter_plus_minus_a + jitter_plus_minus_b
        plus_minus_parameters <- MLE_vector + jitter_plus_minus
        plus_minus_llk <- llk_function(men$value, women$value, 
                                      plus_minus_parameters["mu_1"],
                                      plus_minus_parameters["mu_2"],
                                      plus_minus_parameters["p"],
                                      plus_minus_parameters["q"],
                                      plus_minus_parameters["sigma_2_men"],
                                      plus_minus_parameters["sigma_2_women"])
        jitter_minus_plus_a <- epsilon * as.numeric(names(MLE) == param_1)
        jitter_minus_plus_b <- -epsilon * as.numeric(names(MLE) == param_2)
        jitter_minus_plus <- jitter_minus_plus_a + jitter_minus_plus_b
        minus_plus_parameters <- MLE_vector + jitter_minus_plus
        minus_plus_llk <- llk_function(men$value, women$value, 
                                       minus_plus_parameters["mu_1"],
                                       minus_plus_parameters["mu_2"],
                                       minus_plus_parameters["p"],
                                       minus_plus_parameters["q"],
                                       minus_plus_parameters["sigma_2_men"],
                                       minus_plus_parameters["sigma_2_women"])
        second_derivative = 
          (plus_plus_llk + minus_minus_llk - plus_minus_llk - minus_plus_llk) / (4*epsilon^2)
        hessian[param_1,param_2] <- second_derivative
        hessian[param_2,param_1] <- second_derivative
      }
    }  
  }
  return(hessian)
}