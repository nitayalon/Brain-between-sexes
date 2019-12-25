BrainFeatureAnalysis <- function(feature_data, including_equal_mixture_model = T)                                 
{
  hypothesis_results <- applyHypothesisOverResiduals(feature_data,
                                                     including_equal_mixture_model)
  return(list(
    hypothesis_results = hypothesis_results
  ))
}
