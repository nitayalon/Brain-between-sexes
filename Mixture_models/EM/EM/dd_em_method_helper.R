validateInitEMParameters <- function(init_parameters)
{
  if(names(init_parameters) == c("p","q","mu_1","mu_2","sigma_2"))
  {
    if(init_parameters$q < 0 || init_parameters$q > 1)
    {
      return(F)
    }
    if(init_parameters$p < 0 || init_parameters$p > 1)
    {
      return(F)
    }
    if(init_parameters$sigma_2 < 0)
    {
      return(F)
    }
    return(T)
  }
  return(F)
}

computeInitParameters <- function(p,q,men_bar,women_bar)
{
  if((p == q) & (p == 0.5))
  {
    warning("Cannot start for 50-50 mixture model")
    p = 0.51
    q = 0.49
  }
  mu_1 = ((1-q) * men_bar - (1-p) * women_bar) / (p*(1-q) - q*(1-p)) 
  mu_2 = (q * men_bar - p * women_bar) / (q*(1-p) - p*(1-q))
  sigma_2 = max((mu_1 - mu_2)^2*(p+q)/2*(2-p-q)/2,2)
  init_paramters <- list(mu_1 = mu_1, mu_2 = mu_2, sigma_2 = sigma_2)
  return(init_paramters)
}

prepareData <- function(observations)
{
  men <- observations$value[observations$bio_sex == 1]
  women <- observations$value[observations$bio_sex == 2]
  obs <- list(men = men, women = women)
  return(obs)
}


prepareDataBioBank <- function(observations)
{
  men <- observations$value[observations$sex == 1]
  women <- observations$value[observations$sex == 0]
  obs <- list(men = men, women = women)
  return(obs)
}