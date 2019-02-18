# compute theta:
computeThetaNormal <- function(mu, p){
  theta <- mu / (2 * p - 1)
  return(theta)
}

computeThetaALD <- function(mu, p){
  theta <- sqrt((2 * p - 1) / mu - 1)
  return(theta)
}

computeMu <- function(theta, p){
  mu <- theta * (2 * p - 1)
  return(mu)
}


computeSexVariance <- function(mu, p){
  return(1 - mu^2 / (2 * p - 1)^2)
}


popVariance <- function(population){
  pop_var <- var(c(population$men, population$women))
  return(pop_var)
}

sampleAndComputeKldForPair <- function(n = 1e3, parameters, variance, distribution = "normal"){
  
  # remove infeasible parameters:
  feasible_index <- variance > 0 
  feasible_parameters <- parameters[feasible_index,] 
  if(distribution == "normal"){
    theta <- apply(parameters, 1, function(x){computeThetaNormal(x[1],x[2])})
  }
  else if(distribution == "laplace"){
    theta <- apply(parameters, 1, function(x){computeThetaALD(x[1],x[2])})
  }
  else{
    stop("Only normal or ADL distributions are supported!")
  }
  
  feasible_theta <- theta[feasible_index] 
  feasible_variance <- variance[feasible_index] 
  KLD <- c()
  pop_variance <- c()
  for(i in 1:length(feasible_theta)){
    print(i)
    Mu <- feasible_parameters$Var1[i]
    p <- feasible_parameters$Var2[i]
    Theta <- feasible_theta[i]
    sigma2 <- feasible_variance[i]
    samples <- sampleMockData(Theta, -Theta, p, 1-p, sqrt(sigma2),
                              n, T,distribution)
    pop_variance[i] <- popVariance(samples)
    KLD[i] <- simpleKLDComputation(samples, p, Theta, -Theta,
                                   sigma2, sigma2, Mu, -Mu)
  }
  return(list(
    KLD = KLD,
    pop_variance = pop_variance
  ))
}