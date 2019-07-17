#' Computing KLD for the mixture model

# MC simulation -----------------------------------------------------------

computeKLD <- function(samples, parameter_row, method = "MC"){
  men <- samples$men
  women <- samples$women
  men_mean <- parameter_row$xi
  women_mean <- -parameter_row$xi
  men_sd <- women_sd <- sqrt(1 - parameter_row$xi^2)
  male_mean <- parameter_row$xi + parameter_row$epsilon
  female_mean <- -parameter_row$xi - parameter_row$delta
  p <- parameter_row$p
  q <- parameter_row$q
  sex_sd <- sqrt(parameter_row$group_variance)
  ## Compute likelihood
  null_llk <- computeNullLogLikelihood(men,men_mean,men_sd,women,women_mean,women_sd)
  alternative_llk <- computeAlternativeLogLikelihood(men,women,parameter_row)
  
  if(method == "MC")
  {
    KLD_estimate_men <- mean(log(alternative_llk$llk_men / null_llk$llk_men))
    KLD_estimate_women <- mean(log(alternative_llk$llk_women / null_llk$llk_women))
  }
  
  if(method == "Numeric_Integration")
  {
    KLD_estimate_men <- 
      numericKLD(men,p,male_mean, female_mean,sex_sd,men_mean,men_sd)$value
    KLD_estimate_women <- 
      numericKLD(women,q,male_mean, female_mean,sex_sd,men_mean,men_sd)$value
  }
  
  return(list(
    KLD_estimate = KLD_estimate_men + KLD_estimate_women,
    KLD_estimate_men = KLD_estimate_men,
    KLD_estimate_women = KLD_estimate_women
    ))
}


# Numeric integral --------------------------------------------------------
numericKLD <- function(sample,prop,theta_male, theta_female,sigma,x_bar,sigma_MLE){
  from <- min(sample)
  to <- max(sample)
  kld <- tryCatch(
    {
    integrate(KLD,from,to,prop,theta_male, theta_female,sigma,x_bar,sigma_MLE,
                   rel.tol = 1e-10,subdivisions = 100)
    },
    error=function(cond)
    {
      message("There is a problem with numeric integration")
      message(paste("The parameter set is ",
              prop,",",theta_male,",",theta_female,",",
              sigma,",",x_bar,",",sigma_MLE))
      return(NA)
    }
  )  
  return(kld)
}

KLD <- function(x,prop,theta_male,theta_female,sigma,theta_MLE ,sigma_MLE){
  log(f1(x,prop,theta_male,theta_female,sigma) / f0(x,theta_MLE ,sigma_MLE)) * 
    f1(x,prop, theta_male,theta_female,sigma)
}

f1 <- function(x,prop,theta_male,theta_female,sigma){
  f1 <- prop * computeLLK(x,theta_male ,sigma) + 
    (1 - prop) * computeLLK(x,theta_female ,sigma) + 1e-6
  stopifnot(all(f1 > 0))
  return(f1)
}

f0 <- function(x,theta_MLE ,sigma_MLE){
  f0 <- computeLLK(x,theta_MLE ,sigma_MLE) + 1e-6
  stopifnot(all(f0 > 0))
  return(f0)
}

# Likelihood computation --------------------------------------------------

computeNullLogLikelihood <- function(men, men_mean,men_sd, women, women_mean,women_sd){
  llk_men <- computeLLK(men, men_mean,men_sd)
  llk_women <- computeLLK(women, women_mean,women_sd)
  return(list(
    llk_men = llk_men ,
    llk_women = llk_women
    ))
}

computeAlternativeLogLikelihood <- function(men, women, parameters){
  
  male_theta <- parameters$xi + parameters$epsilon
  female_theta <- -parameters$xi - parameters$delta
  sigma <- sqrt(parameters$group_variance)
  llk_men <- parameters$p * computeLLK(men,male_theta ,sigma) + 
                       (1 - parameters$p) * computeLLK(men,female_theta ,sigma)
  llk_women <- parameters$q * computeLLK(women,male_theta ,sigma) + 
                         (1 - parameters$q) * computeLLK(women,female_theta ,sigma)
  return(list(
    llk_men = llk_men ,
    llk_women = llk_women
  ))
}

computeLLK <- function(data,mu,sigma){
  llk <- dnorm(data,mu,sigma)
  return(llk)
}















# Numeric intergration ----------------------------------------------------

#' This script computes KLD by numeric integration under mixture model
computeKLDIntegral <- function(sample,parameters,null_hypothesis = F){

  ep <- parameters$ep
  de <- parameters$de
  
  if(null_hypothesis){
    llk0 <- function(y){
      wy(y,ep,de) * dnorm(y)
    }
    llk <- integrate(llk0,lower = min(sample) - 1 
                     , upper = max(sample) + 1,subdivisions = 10000)$value
  }
  else{
    llk1 <- function(y){
      wy(y,ep,de) * dnorm(y - de)
    }
    llk2 <- function(y){
      wy(y,ep,de) * dnorm(y + ep)
    }
    llk <- 
      ep/(ep + de) * integrate(llk1,lower = min(sample) - 1 
                     , upper = max(sample) + 1,subdivisions = 10000)$value + 
      de/(ep + de) * integrate(llk2,lower = min(sample) - 1 
                               , upper = max(sample) + 1,subdivisions = 10000)$value  
  }
  return(llk) 
}

wy <- function(y,ep,de){
  res <- log(
    ep/(ep + de) * exp(de * y - de^2/2) + 
      de/(ep + de) * exp(-ep * y - ep^2/2))
  return(res)
}

