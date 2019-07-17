#' Apply full analysis over brain feature as described in the documentation
#' section (See ne analysis method)
#' @param feature_name - brain feature name (As set in BioBank data)
#' @param trimming_limit - number indicating the trimming threshold after scoring 
#' @return test results 
fullBrainFeatureAnalysis <- function(feature_name,
                                     list_of_brain_feature_names,
                                     subject_data)
{
  stopifnot(feature_name %in% list_of_brain_feature_names)
  # LM + Z_score + Scale
  feature_residuals <- applyLinearModelOverBrainFeature(feature_name,
                                                        subject_data = subject_data)
  # EM
  hypothesis_results <- applyHypothesisOverResiduals(feature_residuals)
  return(list(
    feature_residuals = feature_residuals,
    hypothesis_results = hypothesis_results
  ))
}
