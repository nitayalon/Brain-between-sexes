#' Apply a set of hypothesis test over each brain feature
#' @param residual_data a tibble of scaled residuals for brain feature 
#' @return test results 
applyHypothesisOverResiduals <- function(residual_data) {
  
  simple_vs_compostie_hypothesis_test <- 
    simpleDistributionVsCompositeDistribution(
        residual_data$value,
        residual_data$sex,
        "L")
  simple_vs_compostie_llrt <- simple_vs_compostie_hypothesis_test
  
  t_test_for_difference_between_genders <- 
    t.test(residual_data$value[residual_data$sex == 0],
           residual_data$value[residual_data$sex == 1])
  
  pure_types_vs_mixed_gender_hypothesis <- 
    pureTypeMixtureVsCompositeMixture(residual_data$value,
                                      residual_data$sex,
                                      distribution_model = "L",
                                      return_full_data = T, 
                                      data_needs_preparation = F)
  return(list(
    simple_vs_compostie_llrt = simple_vs_compostie_llrt,
    t_test_for_difference_between_genders = 
      t_test_for_difference_between_genders,
    pure_type_vs_mixed_gender_em_results = pure_types_vs_mixed_gender_hypothesis,
    pure_vs_mixed_llrt = -2*pure_types_vs_mixed_gender_hypothesis$llrt
  ))
}