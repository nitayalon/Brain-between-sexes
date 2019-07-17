### Validating the llk computation method

generateMockGenderData <- function(mu,n,std = 1)
{
  return(rnorm(n,mu))
}

generateMockFeatureData <- function(mu = 0.5,n = 3000,...)
{
  men_data <- generateMockGenderData(mu,n)
  women_data <- generateMockGenderData(-mu,n)
  pop <- c(men_data,women_data)
  feature_data <- data.frame(val = pop, sex = c(rep(1,n),rep(2,n)))
  return(feature_data)
}

feat <- generateMockFeatureData()
parameter.row <- data.frame(theta_mas = 0.5,theta_fem = -0.5,
                            p = 1,q = 0,sigma_2 = 1)
logLikelihoodComputationForFeatureSet(feat,parameter.row)
