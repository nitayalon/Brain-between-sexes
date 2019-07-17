#' Computing llk for pure types mixture model
pureTypeMixtureModelHypothesis <- function(feature_data,
                                           is_data_processed = F)
{
  if(is_data_processed)
  {
    split_to_gender <- prepareDataBioBank(feature_data)
  }
  else
  {
    split_to_gender <- feature_data
    names(split_to_gender) <- c("value","bio_sex", "eid")
  }
  pure_type_model <- logNormalNullLlkComputation(split_to_gender,T)
  return(pure_type_model)
}
