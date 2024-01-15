#' Convert the EM probabilities to nomenclature where p,q denote the probability to select the higher mean
#' @param EM results of EM algorithm
#' @return c(p,q) a vector of unified probabilities

convertEMProbabiltiesToPQSpace <- function(em_results)
{
  stopifnot(length(em_results) > 0)
  m_parameters <- lapply(em_results, function(x){x$hypothesis_results$mixture_model$m_parameters})
  unified_m_parameters <- lapply(m_parameters, function(x){adaptProbabilitiesToSize(x)})
  return(unified_m_parameters)
}

adaptProbabilitiesToSize <- function(m_parameters)
{
  mu_1_positive <- m_parameters$mu_1 > 0
  if(mu_1_positive)
  {
    return(m_parameters)
  }
  else
  {
    m_parameters$p <- 1 - m_parameters$p
    m_parameters$q <- 1 - m_parameters$q
    return(m_parameters)
  }
}

adjustResponsebilitiesToHighMean <- function(em_results)
{
  m_parameters <- em_results$hypothesis_results$mixture_model$m_parameters
  mu_1_negative <- m_parameters$mu_1 > 0
  men_responsebilities <- em_results$hypothesis_results$mixture_model$men_responsebilities
  women_responsebilities <- em_results$hypothesis_results$mixture_model$women_responsebilities
  if(mu_1_negative)
  {
    men_responsebilities$responsebility <- 1 - men_responsebilities$responsebility
    women_responsebilities$responsebility <- 1 - women_responsebilities$responsebility
  }  
  return(list(men_responsebilities = men_responsebilities,
              women_responsebilities = women_responsebilities))
}


pullPvaluePerHypothesis <- function(em_results)
{
  sapply(em_results, function(x){
    max(1e-6, 1 - pchisq(2 * x$hypothesis_results$pure_types_vs_mixture_model_llr, 1))})
}

equalProbabilityMixturePV <- function(em_results)
{
  sapply(em_results, function(x){
    max(1e-6, 1 - pchisq(2 * x$hypothesis_results$equal_proportions_vs_mixture_model_llr,2))})
}


# str(biobank_feature_residual_analysis$`Mean FA in middle cerebellar peduncle on FA skeleton`$hypothesis_results)