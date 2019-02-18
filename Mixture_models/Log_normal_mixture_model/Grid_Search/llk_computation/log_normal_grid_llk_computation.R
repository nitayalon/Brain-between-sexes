# Computeing the llk for feature data - assuming log normal distribution, standardize data and so on

logNormalGridLlkComputation <- function(feature_data,...)
{
  # create parameter grid
  validateInput(feature_data)
  parameter_set <- createSexParameterGrid(feature_data)
  # compute log likelihood for the data and each parameter set
  llk_for_feature_set <- apply(parameter_set, 1, 
                               function(x){logLikelihoodComputationForFeatureSet(feature_data,x)})
  results <- cbind(parameter_set,llk_for_feature_set)
  names(results) <- c(names(parameter_set),"llk")
  return(results)
}

validateInput <- function(feature_data)
{
  return(T)
}