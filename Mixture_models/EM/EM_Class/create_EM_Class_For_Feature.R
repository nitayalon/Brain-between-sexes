createEMClassForFeature <- function(feature_name,log_applied = T, feature_data)
{
  em_over_brain_feature <- new("EM")
  em_over_brain_feature <- setEMName(em_over_brain_feature,feature_name,log_applied)
  em_over_brain_feature <- loadBrainDataForEM(em_over_brain_feature,
                                                       feature_data)
  em_over_brain_feature <- applyEMAlgorithmOnBrainData(em_over_brain_feature)
  return(em_over_brain_feature)
}