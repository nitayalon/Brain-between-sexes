#' This is the main script for applying grid search over the mixture of mixture model
#' following the algorithm decribed in the notebook number 2

createSexParameterGrid <- function(feature_data,grid_size = 500)
{
  #INPUT: feature data - data frame with normalized measurments data and a gender column
  #OUTPUT:
  
  men_avg <- mean(feature_data$val[feature_data$sex == 1])
  women_avg <- mean(feature_data$val[feature_data$sex == 2])
  
  max_value_for_grid <- max(men_avg,women_avg)
  min_value_for_grid <- min(men_avg,women_avg)
  
  theta_positive <- seq(max_value_for_grid,max_value_for_grid + 5,length.out = grid_size)
  theta_negative <- seq(min_value_for_grid - 5,min_value_for_grid,length.out = grid_size)
  outer_parameter_grid <- expand.grid(theta_positive,theta_negative) 
  mixture_probs <- apply(outer_parameter_grid,1,function(x){
    computeMixtureProbabilityForPair(x[1],x[2],men_avg,women_avg)})
  sigma_2 <- apply(outer_parameter_grid,1,function(x){
    computeGroupVarianceGridSearch(x[1],x[2])})
  feasible_set <- partitionFeasibleParameterSet(outer_parameter_grid,mixture_probs,sigma_2)
  names(feasible_set) <- c("theta_mas","theta_fem","p","q","sigma_2")
  return(feasible_set)
}

