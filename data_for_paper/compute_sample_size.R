nullHypothesisIsTrueLlk <- function(sample_size){
  alternative_llk <- NULL
  counter = 0
  while(is.null(alternative_llk))
  {
    data <- rnorm(sample_size, 0,1)  
    x_bar = mean(data)
    s_2 = sum((data - mean(data))^2) / sample_size
    null_hypothesis_llk <- sum(dnorm(data,mean(data),sqrt(s_2),log = T))
    theoretical_llk = -sample_size/2 - sample_size/2*log(s_2) - 
      sample_size/2*log(2*pi)
    alternative_llk <- tryCatch(
      {normalmixEM(data, lambda = 0.9,
                                   maxit = 10000,
                                   arbmean = TRUE,
                                   arbvar = FALSE,
                                   fast = TRUE)},
                                warning=function(e){
                                  NULL
                                },
      error=function(e){
        NULL
      })
    counter = counter + 1
    if(counter %% 10 == 0){
      cat(sprintf('Iteration number %i',counter))
    }
    if(counter >= 100)
    {
      break
    }
  }
  return(list(null_hypothesis_llk = null_hypothesis_llk,
              alternative_llk = alternative_llk$loglik,
              em_params = list(p = alternative_llk$lambda,
                               location = alternative_llk$mu,
                               scale = alternative_llk$sigma),
              theoretical_llk = theoretical_llk,
              llrt = 2 * (alternative_llk$loglik - null_hypothesis_llk)))
}

alternativeHypothesisIsTrueLlk <- function(alternative_parameters, 
                                           sample_size){
  alternative_llk <- NULL
  ind <- runif(sample_size) > alternative_parameters$p
  low_distribution <- rnorm(sum(!ind), alternative_parameters$mu_2, sqrt(alternative_parameters$sigma_2_men))
  high_distribution <- rnorm(sum(ind), alternative_parameters$mu_1, sqrt(alternative_parameters$sigma_2_men))
  data <- c(low_distribution,high_distribution)
  null_hypothesis_llk <- sum(dnorm(data,mean(data),sd(data),log = T))
  alternative_llk <- tryCatch(
    {normalmixEM(data, lambda = alternative_parameters$p,
                                   maxit = 10000,
                                   arbmean = TRUE,
                                   arbvar = TRUE,
                                   fast = TRUE)},
                                warning=function(e){
                                  NULL
                                },
      error=function(e){
        NULL
      })
  return(list(null_hypothesis_llk = null_hypothesis_llk,
              alternative_llk = alternative_llk$loglik,
              em_params = list(p = alternative_llk$lambda,
                               location = alternative_llk$mu,
                               scale = alternative_llk$sigma),
              llrt = 2 * (alternative_llk$loglik - null_hypothesis_llk)))
}