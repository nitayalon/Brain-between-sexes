computeLogLikelihoodForFeature <- function(men_feature_data, women_feature_data,
                                           theta_2, theta_1, p, q, 
                                           sigma_2_mas,sigma_2_fem){
  
  men_log_likelihood <- evaluateLogLikelihoodOFMixtureModel(
    men_feature_data,p, theta_2, theta_1,
    sigma_2_mas,sigma_2_fem)
  
  women_log_likelihood <- evaluateLogLikelihoodOFMixtureModel(
    women_feature_data, q, theta_2, theta_1,
    sigma_2_mas,sigma_2_fem)
  
  return(c(
    men_log_likelihood = men_log_likelihood, 
    women_log_likelihood = women_log_likelihood
  ))
}