methodHelperOfResidualComputation <- function(feature_name, subject_data)
{
  feature_residuals <- applyLinearModelOverBrainFeature(feature_name,
                                                        subject_data = subject_data)  
  return(feature_residuals)
}
