#' Apply a set of hypothesis test over each brain feature
#' @param feature_data a tibble of scaled residuals for brain feature 
#' @return test results 
applyHypothesisOverResiduals <- function(feature_data,
                                         including_equal_mixture_model = T) {
  
  t_test_for_difference_between_genders <- 
    t.test(feature_data$value[feature_data$sex == 0],
           feature_data$value[feature_data$sex == 1])
  
  cohen_d_test <- cohen.d(feature_data$value[feature_data$sex == 0],
                          feature_data$value[feature_data$sex == 1])
  
  single_population_model <- singleDistributionHypothesisLoglikelihood(feature_data$value)
  pure_types_model <- pureTypeMixtureModelHypothesis(feature_data)
  mixture_model <- tryCatch(
    {
      doubleDoubleEM(feature_data,T)
    },
    error=function(cond)
    {
      return(data.frame(llk = -Inf))
    }
  )
  if(including_equal_mixture_model){
    equal_proportions_model <- computeEqualProprtionsEM(feature_data)
    equal_proportions_vs_mixture_model_llr <- mixture_model$llk - equal_proportions_model$loglik
  }
  else{
    equal_proportions_model <- NULL
    equal_proportions_vs_mixture_model_llr <- NULL
  }
  single_population_vs_mixture_model_llr <- mixture_model$llk - single_population_model
  pure_types_vs_mixture_model_llr <- mixture_model$llk - pure_types_model$llk
  
  return(list(
    t_test_for_difference_between_genders = 
      t_test_for_difference_between_genders,
    cohen_d_test = cohen_d_test,
    simple_vs_compostie_llrt = single_population_vs_mixture_model_llr,
    pure_types_vs_mixture_model_llr = pure_types_vs_mixture_model_llr,
    equal_proportions_vs_mixture_model_llr = equal_proportions_vs_mixture_model_llr,
    pure_types_model = pure_types_model,
    equal_proportions_model = equal_proportions_model,
    mixture_model = mixture_model
  ))
}