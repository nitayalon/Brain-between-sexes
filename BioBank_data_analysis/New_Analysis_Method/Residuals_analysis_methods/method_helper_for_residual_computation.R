methodHelperOfResidualComputation <- function(feature_name, subject_data, brain_volume,
                                              winsorized = T,normalized = F)
{
  feature_residuals <- applyLinearModelOverBrainFeature(feature_name = feature_name,
                                                        subject_data = subject_data,
                                                        total_brain_volume_data = brain_volume,
                                                        winsorized = winsorized,
                                                        normalized = normalized)  
  return(feature_residuals)
}
