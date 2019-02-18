createOuterLoopGrid <- function(mu, step_dir){
  theta_2 <- seq(mu, 1 / mu, 0.1 * (-1)^step_dir)
  return(theta_2 = theta_2)
}

createInnerLoopGrid <- function(theta_2, mu, step_dir){
  theta_1 <- seq(-mu,1 / -theta_2,  -0.1 * (-1)^step_dir)
  return(theta_1 = theta_1)
}


computeParameters <- function(theta_2,theta_1, mu){
  p = (theta_2 + mu) / (theta_2 - theta_1)
  q = (theta_2 - mu) / (theta_2 - theta_1)
  sigma_2 = 1 + theta_2 * theta_1
  return(list(
    p = p,
    q = q,
    sigma_2 = sigma_2
  ))
}

evaluateLogLikelihoodOFMixtureModel <- function(observations,
                                                masculine_prob, 
                                                theta_mas, theta_fem, 
                                                sigma_2_mas, sigma_2_fem,
                                                log = F,
                                                full_data = F, 
                                                distribution = "normal"){
  if(distribution == "normal"){
    masculine <- 
      dnorm(observations, theta_mas, sqrt(sigma_2_mas))
    feminine <- 
      dnorm(observations, theta_fem, sqrt(sigma_2_fem))
  }
  
  if(distribution == "laplace"){
    masculine <- 
      dlaplace(observations, theta_mas)
    feminine <- 
      dlaplace(observations, theta_fem)
  }
  
  if(log){
    llk <- tryCatch(
      {
        masculine_prob * masculine + (1 - masculine_prob) * feminine
      },
      error = function(e){
        message(e)
        return(NA)
      },
      warning = function(w){
        message(w)
        return(NULL)
      }
    )
  }
  else{
    llk <- tryCatch(
      {
        log(masculine_prob * masculine + (1 - masculine_prob) * feminine)
      },
      error = function(e){
        message(e)
        return(NA)
      },
      warning = function(w){
        message(w)
        return(NULL)
      }
    )
  }
  if(full_data){
    return(llk)
  }
  return(sum(llk))
}

