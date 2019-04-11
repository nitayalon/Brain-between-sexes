computeModelBasedLogLikeihoodRatio <- function(em_paramters_per_feature,
                                               men_residuals,
                                               women_residuals,
                                               trim_level = 6)
{
  stopifnot(names(em_paramters_per_feature) ==
              c("p","q","mu_1","mu_2","sigma_2_men","sigma_2_women"))
  p <- em_paramters_per_feature$p
  q <- em_paramters_per_feature$q
  mu_1 <- em_paramters_per_feature$mu_1
  mu_2 <- em_paramters_per_feature$mu_2
  sigma_2_men <- em_paramters_per_feature$sigma_2_men
  sigma_2_women <- em_paramters_per_feature$sigma_2_women
  men_resid_sorted <- sort(men_residuals[men_residuals > -trim_level & men_residuals < trim_level])
  women_resid_sorted <- sort(women_residuals[women_residuals > -trim_level & women_residuals < trim_level])
  
  population <- sort(c(men_resid_sorted, women_resid_sorted))
  # men_likelihood <- p * dnorm(men_resid_sorted, mu_1, sigma_2_men) + 
  #   (1-p) * dnorm(men_resid_sorted, mu_2, sigma_2_women)
  # women_likelihood <- q * dnorm(women_resid_sorted, mu_1, sigma_2_men) + 
  #   (1-q) * dnorm(women_resid_sorted, mu_2, sigma_2_women)
  
  men_likelihood <- p * dnorm(population, mu_1, sqrt(sigma_2_men)) + 
    (1-p) * dnorm(population, mu_2, sqrt(sigma_2_women))
  women_likelihood <- q * dnorm(population, mu_1, sqrt(sigma_2_men)) + 
    (1-q) * dnorm(population, mu_2, sqrt(sigma_2_women))
  
  llrt = log(men_likelihood/ women_likelihood)
  
  return(list(
    men_likelihood = men_likelihood, 
    women_likelihood = women_likelihood,
    llrt = llrt,
    population = population))
}