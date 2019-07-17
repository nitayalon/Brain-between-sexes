# This script samples observations from a mixture model (just men)
# and computes the KLD of the mixture vs pure models:
simpleKLDComputation <- function(obs,p, 
                                 theta_male, theta_female, 
                                 male_var, female_var,
                                 men_mu, women_mu){
  
  men_feature_data <- obs$men
  women_feature_data <- obs$women
  men_null_var <- var(men_feature_data)
  women_null_var <- var(women_feature_data)
  # Compute the likelihood for each observation:
    f_men_alternative <- evaluateLogLikelihoodOFMixtureModel(
        men_feature_data, 
        p, 
        theta_mas = theta_male,
        theta_fem = theta_female,
        sigma_2_mas = male_var,
        sigma_2_fem = female_var,
        log = T, full_data = T)
    
    f_women_alternative <- evaluateLogLikelihoodOFMixtureModel(
        women_feature_data, 
        1-p, 
        theta_mas = theta_male,
        theta_fem = theta_female,
        sigma_2_mas = male_var,
        sigma_2_fem = female_var,
        log = T, full_data = T)
    
    
    f_men_null <- evaluateLogLikelihoodOFMixtureModel(
        men_feature_data, 
        1,
        theta_mas = men_mu, 
        theta_fem = 0,
        sigma_2_mas = men_null_var,
        sigma_2_fem = women_null_var,
        log = T, full_data = T)
    
    f_women_null <- evaluateLogLikelihoodOFMixtureModel(
        women_feature_data, 
        1,
        theta_mas = men_mu, 
        theta_fem = 0,
        sigma_2_mas = men_null_var,
        sigma_2_fem = women_null_var,
        log = T, full_data = T)
  
  llk_men <- f_men_alternative/f_men_null
  llk_women <- f_women_alternative/f_women_null
  
  kldMen <- function(gender_data){
    p_x <- p * dnorm(gender_data, theta_male, male_var) + 
      (1-p)* dnorm(gender_data, theta_female, female_var)
    # p_x[p_x == 0] <- 1e-6
    q_x <- dnorm(gender_data, men_mu, men_null_var)
    # return(p_x * log(p_x / q_x))  
    return(log(p_x / q_x))
  }
   
  kldWomen <- function(gender_data){
    p_x <- (1-p) * dnorm(gender_data, theta_male, male_var) + 
      p * dnorm(gender_data, theta_female, female_var)
    # p_x[p_x == 0] <- 1e-6
    q_x <- dnorm(gender_data, women_mu, women_null_var)
    # return(p_x * log(p_x / q_x))  
    return(log(p_x / q_x))  
  }
   
  # KLD_men <- integrate(kldMen, min(men_feature_data), max(men_feature_data),subdivisions = 100) 
  # KLD_women <- integrate(kldWomen, min(women_feature_data), max(women_feature_data),subdivisions = 100) 
  KLD_men <- mean(log(llk_men))
  KLD_women <- mean(log(llk_women))
  # return(c(KLD_men$value , KLD_women$value))
  return(KLD_men+KLD_women)
}

