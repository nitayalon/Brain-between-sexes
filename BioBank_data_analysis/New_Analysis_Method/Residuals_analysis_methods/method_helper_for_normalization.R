methodHelperFeatureNormalization <- function(feature_name, subject_data)
{
  feature_residuals <- applyStandartizationForFeature(feature_name,
                                                      subject_data = subject_data)  
  return(feature_residuals)
}
