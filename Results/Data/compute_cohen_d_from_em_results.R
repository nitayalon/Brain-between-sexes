computeCohensDFromStatistic <- function(mu_1,mu_2,sigma_2_1, sigma_2_2,
                                        group_1_size, group_2_size)
{
  cohen_d_stat = (mu_1 - mu_2) / 
    sqrt(((group_1_size -1) * sigma_2_1 + 
            (group_2_size -1) * sigma_2_2)/
           (group_1_size + group_2_size - 2))
  return(cohen_d_stat)
}

extractStatisticsForCohensD <- function(feature_data)
{
  sufficient_statistics <- feature_data$`hypothesis_results`$`pure_type_vs_mixed_gender_em_results`$`alternative_hypothesis`$`m_parameters`
  group_size <- lapply(feature_data$`hypothesis_results`$`pure_type_vs_mixed_gender_em_results`$`alternative_hypothesis`$`e_parameters`, length)  
  cohens_d <- computeCohensDFromStatistic(sufficient_statistics$mu_1, sufficient_statistics$mu_2,
                              sufficient_statistics$sigma_2_men, sufficient_statistics$sigma_2_women,
                              group_size$I, group_size$J)
  return(cohens_d)
}