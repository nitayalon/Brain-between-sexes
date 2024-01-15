# Computing the -llk under the null hypothesis
logNormalNullLlkComputation <- function(feature_data,
                                        is_bio_bank_data = T)
{
  p <- 1
  q <- 0
  parameters <- feature_data %>% 
    group_by(bio_sex) %>% 
    summarise(theta = mean(value),
              var = var(value))
  theta_mas = parameters$theta[parameters$bio_sex == 0]
  theta_fem = parameters$theta[parameters$bio_sex == 1]
  sigma_2_men = parameters$var[parameters$bio_sex == 0]
  sigma_2_women = parameters$var[parameters$bio_sex == 1]
  parameter_row <- c(theta_mas,theta_fem,p,q,sigma_2_men,sigma_2_women)
  llk <- computeLogLikelihood(feature_data$value[feature_data$bio_sex == 0],
                              feature_data$value[feature_data$bio_sex == 1],
                              theta_mas,
                              theta_fem,
                              p,
                              q,
                              sigma_2_men,
                              sigma_2_women)
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