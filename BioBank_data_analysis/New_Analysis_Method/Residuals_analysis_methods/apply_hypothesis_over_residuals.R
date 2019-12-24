#' Apply a set of hypothesis test over each brain feature
#' @param feature_data a tibble of scaled residuals for brain feature 
#' @return test results 
applyHypothesisOverResiduals <- function(feature_data) {
  
  feature_data_trimmed <- feature_data %>% 
    filter(value < mean(value) + 4 * sd(value) &
             value > mean(value) - 4 * sd(value))
  
  simple_vs_compostie_hypothesis_test <- 
    simpleDistributionVsCompositeDistribution(
      feature_data$value,
      feature_data$sex,
      feature_data$eid,
        "L")
  
  simple_vs_compostie_llrt <- simple_vs_compostie_hypothesis_test
  
  t_test_for_difference_between_genders <- 
    t.test(feature_data_trimmed$value[feature_data$sex == 0],
           feature_data$value[feature_data$sex == 1])
  
  cohen_d_test <- cohen.d(feature_data$value[feature_data$sex == 0],
                          feature_data$value[feature_data$sex == 1])
  
  pure_types_vs_mixed_gender_hypothesis <- 
    pureTypeMixtureVsCompositeMixture(feature_data$value,
                                      feature_data$sex,
                                      feature_data$eid,
                                      distribution_model = "L",
                                      return_full_data = T, 
                                      data_needs_preparation = F)
  return(list(
    simple_vs_compostie_llrt = simple_vs_compostie_llrt,
    t_test_for_difference_between_genders = 
      t_test_for_difference_between_genders,
    cohen_d_test = cohen_d_test,
    pure_type_vs_mixed_gender_em_results = pure_types_vs_mixed_gender_hypothesis,
    pure_vs_mixed_llrt = -2*pure_types_vs_mixed_gender_hypothesis$llrt
  ))
}