# Computing the -llk under the null hypothesis
logNormalNullLlkComputation <- function(feature_data,
                                        is_bio_bank_data = F)
{
  if(is_bio_bank_data)
  {
    men <- feature_data$men
    women <- feature_data$women
    # men <- feature_data$value[feature_data$bio_sex == 1]
    # women <- feature_data$value[feature_data$bio_sex == 0]
  }
  else
  {
    men <- feature_data$value[feature_data$bio_sex == 1]
    women <- feature_data$value[feature_data$bio_sex == 2]
  }
  theta_mas <- mean(men)
  theta_fem <- mean(women)
  p <- 1
  q <- 0
  sigma_2_men = sd(men)
  sigma_2_women = sd(women)
  parameter_row <- c(theta_mas,theta_fem,p,q,sigma_2_men,sigma_2_women)
  llk <- logLikelihoodComputationForFeatureSet(feature_data,
                                               parameter_row,
                                               is_bio_bank_data)
  return(list(llk = llk,
              men_mean = theta_mas,
              women_mean = theta_fem,
              sigma_2_men = sigma_2_men,
              sigma_2_women = sigma_2_women))
}

computePooledVariance <- function(men,women)
{
  m <- length(men)
  n <- length(women)
  men_var <- var(men)
  women_var <- var(women)
  pooled <- ((m-1)*men_var + (n-1)*women_var) / (m + n - 2)
  return(pooled)
}