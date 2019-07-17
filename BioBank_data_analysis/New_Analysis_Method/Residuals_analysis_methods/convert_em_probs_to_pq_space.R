#' Convert the EM probabilities to nomenclature where p,q denote the probability to select the higher mean
#' @param EM results of EM algorithm
#' @return c(p,q) a vector of unified probabilities

convertEMProbabiltiesToPQSpace <- function(em_results)
{
  stopifnot(length(em_results) > 0)
  m_parameters <- lapply(em_results, function(x){x$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters})
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

pullPvaluePerHypothesis <- function(em_results)
{
  sapply(em_results, function(x){
    max(1e-6, 1 - pchisq(x$hypothesis_results$pure_type_vs_mixed_gender_em_results$wilks_statistic,2))})
}


