#' Computing llk for pure types mixture model
pureTypeMixtureModelHypothesis <- function(feature_data)
{
  split_to_gender <- prepareDataBioBank(feature_data)
  pure_type_model <- logNormalNullLlkComputation(split_to_gender,T)
  return(pure_type_model)
}
