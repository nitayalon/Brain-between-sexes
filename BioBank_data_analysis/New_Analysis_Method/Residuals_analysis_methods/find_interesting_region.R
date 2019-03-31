#' Finds interesting regions for plotting
#' @param list_of_features - list of brain features 
#' @return list of top k interesting regions

findInterestingRegion <- function(list_of_features, k = 5)
{
  if(length(list_of_features) <= 1)
  {
    return(list_of_features[[1]])
  }
  means_distance <- sapply(list_of_features,
                           function(x){x$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters$mu_1 - 
                               x$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters$mu_2})
  return(sort(means_distance, decreasing = T)[1:k])
}