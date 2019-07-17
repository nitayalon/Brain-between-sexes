BrainFeatureAnalysis <- function(feature_data)                                 
{
  hypothesis_results <- applyHypothesisOverResiduals(feature_data)
  return(list(
    hypothesis_results = hypothesis_results
  ))
}
