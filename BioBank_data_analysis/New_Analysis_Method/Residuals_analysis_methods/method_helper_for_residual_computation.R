methodHelperOfResidualComputation <- function(feature_name, subject_data, brain_volume)
{
  feature_residuals <- applyLinearModelOverBrainFeature(feature_name,
                                                        subject_data = subject_data,
                                                        total_brain_volume_data = brain_volume)  
  return(feature_residuals)
}
