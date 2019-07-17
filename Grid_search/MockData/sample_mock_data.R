sampleMockData <- function(theta_masculine, 
                           theta_feminine, 
                           p, 
                           q,
                           sigma_2, 
                           sample_size,
                           export_full_data = F,
                           distribution = "normal"){
  
  if(!any(distribution == c("normal","laplace"))){
    stop("Only normal or laplace distribution")
  }
  men_avg <- p*theta_masculine + (1-p) * theta_feminine
  women_avg <- q*theta_masculine + (1-q) * theta_feminine
  
  men_mu <- theta_masculine + (theta_feminine - theta_masculine) *
    (runif(sample_size) > p)
  
  women_mu <- theta_feminine + (theta_masculine - theta_feminine) *
    (runif(sample_size) < q)
  
  if(distribution == "normal"){
    men_data <- rnorm(sample_size,0,sigma_2) + 1 * men_mu
    women_data <- rnorm(sample_size,0,sigma_2) + 1 * women_mu
  }
  if(distribution == "laplace"){
    men_data <- sampleDataFromDoubleExponentialDistribution(sample_size, men_mu)
    women_data <- sampleDataFromDoubleExponentialDistribution(sample_size, women_mu)
  }
  
  if(export_full_data){
    return(list(
      men = men_data,
      women = women_data,
      men_avg = men_avg,
      men_var = var(men_data),
      women_var = var(women_data),
      women_avg = women_avg,
      pop_mean = mean(c(men_data, women_data)),
      pop_var = var(c(men_data, women_data))
    ))
  }
  
  return(list(
    men = men_data,
    women = women_data
  ))
}

validateSample <- function(men, women){
  pop_mean <- mean(c(mean(men),mean(women)))
    ### Complete variance formula:
  pop_var <- var(c(mean(men),mean(women))) + mean(c(var(men), var(women)))
  return(list(
    pop_mean = pop_mean,
    pop_var = pop_var
  ))
}