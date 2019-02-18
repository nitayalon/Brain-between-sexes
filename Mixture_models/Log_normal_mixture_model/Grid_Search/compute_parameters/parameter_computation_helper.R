computeMixtureProbabilityForPair <- function(theta_mas,theta_fem,men_avg,women_avg)
{
  #INPUT: theta_mas nad theta_fem are two grid parameters, men_avg is the empirical average of the men group
  #OUTPUT: p,q - vector of the mixture probabilities
  p <- (men_avg - theta_fem) / (theta_mas - theta_fem)
  q <- (women_avg - theta_fem) / (theta_mas - theta_fem)
  results <- c(p,q)
  if(any(results > 1) | any(results < 0))
  {
    return(c(NaN,NaN))
  }
  return(results)
}

computeGroupVarianceGridSearch <- function(theta_mas,theta_fem)
{
  #INPUT: theta_mas nad theta_fem are two grid parameters
  #OUTPUT: sigma_2 - the group variance
  var_between <- var(c(theta_mas,theta_fem))
  sigma_2 <- 1 - var_between
  if(sigma_2 < 0){
    return(NaN)
  }
  return(sigma_2)
}


partitionFeasibleParameterSet <- function(outer_parameter_grid,mixture_probs,sigma_2)
{
  #INPUT: sex parameter centers, mixture probability and sex variance
  #OUTPUT: data frame with feasible parameters
  n <- dim(outer_parameter_grid)[1]
  # validate the dimensions:
  if(!(dim(outer_parameter_grid)[1] == dim(mixture_probs)[1]))
  {
    mixture_probs <- t(mixture_probs)
  }
  parameter_set <- cbind(outer_parameter_grid,mixture_probs,sigma_2)
  stopifnot(all(dim(parameter_set) == c(n,5)))
  # Filter the non relevant rows:
  bad_ind <- apply(parameter_set, 1, function(x)any(is.nan(x)))
  working_parameters <- parameter_set[!bad_ind,]
  return(working_parameters)
}