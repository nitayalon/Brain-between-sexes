#' This script contains helper function for sampling
#' from a mixture model based on the new model (update 27/07/2018)
library(magrittr)
createParametersGrid <- function(default_values = T, xi = NULL, ep = NULL, de = NULL){
  # This function creates a grid of the 3 main paraemeters
  if(default_values){
    default_seq <- seq(0.1,1 - 1e-3, 0.1)
    xi <- default_seq
    epsilon <- expandGridForParameters((1/xi - xi))
    delta <- expandGridForParameters((1/(epsilon + xi) - xi))
  }
  else{
    xi <- xi
    epsilon <- epsilon
    delta <- delta
  }
  parameters <- cbind(xi,epsilon,delta)
  names(parameters) <- c("xi","epsilon","delta")
  stopifnot(validateParameters(parameters))
  parameters %<>% data.frame()
  return(parameters)
}

expandGridForParameters <- function(max_size, default_grid = T){
  default_seq <- seq(1e-3,1 - 1e-3, 0.01)
  grid <- expand.grid(max_size,default_seq)
  parameter_range <- apply(grid,1,function(x){x[1] * x[2]}) 
  return(parameter_range)
}

validateParameters <- function(parameters){
  apply(parameters,1,function(x){
    x[2] < 1/x[1] - x[1] &
      x[3] < 1/(x[1] + x[2]) - x[1]
  })
}

computeMixtureProbabilities <- function(xi, delta, epsilon, as_list = F){
  # This function computes the mixture probability
  denom <- 2 * xi + epsilon + delta
  p <- (2 * xi + delta) / denom
  q <- (delta) / denom
  if(as_list){
    res <- 
      list(
        p = p,
        q = q
      )
  }
  else{
    res <- c(p = p, q = q)
  }
  return(res)
}

generateMixtureProbabilitiesDataFrame <- function(parameters_data_frame){
  stopifnot(class(parameters_data_frame) == "data.frame" &
              dim(parameters_data_frame)[2] == 3)
  mixtue_probs <- apply(parameters_data_frame ,1, function(x){
    computeMixtureProbabilities(x[1],x[3],x[2],F)})
  mixtue_probs <- t(mixtue_probs)
  colnames(mixtue_probs) <- c("p","q")
  return(mixtue_probs)
}

computeGroupVariance <- function(xi,epsilon,delta) {
  # This function computes the group variance
  sigma_2 <- 1 - (xi + epsilon)*(xi + delta)
  stopifnot(sigma_2 > 0)
  return(sigma_2)
}

createParameters <- function(create_auto_grid = T, xi = NULL, ep = NULL, de = NULL){
  parameters <- createParametersGrid(create_auto_grid, xi, ep, de)
  mixtue_probs <- generateMixtureProbabilitiesDataFrame(parameters)
  combined_paremeters <- cbind(parameters,mixtue_probs)
  group_variance <- apply(combined_paremeters, 1, 
                          function(x){computeGroupVariance(x[1],x[2],x[3])})
  combined_paremeters <- cbind(combined_paremeters, group_variance)
  return(combined_paremeters)
}


# Generate samples from MM ------------------------------------------------

sampleFromMixtureModel <- function(xi, epsilon, delta , p, q, sigma_2, sample_size = 1e4){
  ind <- runif(sample_size)
  male_mean <- xi + epsilon
  female_mean <- -xi - delta
  stopifnot(male_mean > 0 & female_mean < 0)
  men <- rnorm(sample_size,male_mean,sqrt(sigma_2)) * (ind <= p) + 
    rnorm(sample_size,female_mean,sqrt(sigma_2)) * (ind > p)
  women <- rnorm(sample_size,male_mean,sqrt(sigma_2)) * (ind <= q) + 
    rnorm(sample_size,female_mean,sqrt(sigma_2)) * (ind > q)
  return(list(
    men = men,
    women = women
  ))
}

