# This is the main function for computing log-likelihood ratio test for
# mixture of mixture model as part of the brain research 
# the function takes as input a data file and returns a table with all the statistics
library(dplyr)

EMForGenderMixtureModel <- function(brain_file_data_with_data)
{
  data <- getRawBrainFileData(brain_file_data_with_data)
  
  full_data_results <- list()
  llr_per_feature <- list()
  
  for(i in 1:length(names(data[-1])))
  {
    # Select feature, apply log and standardization
    brain_feature_name <- names(data)[i]
    if(brain_feature_name %in% c("bio_sex","sex"))
    {
      next
    }
    brain_feature <- new("brainFeatureData")
    brain_feature <- loadBrainFeatureData(brain_feature,brain_file_data_with_data,brain_feature_name)
    brain_feature_log <- applyLogTransformationOnBrainFeatureData(brain_feature)
    brain_feature_scaled <- applyScalingOnBrainFeatureData(brain_feature_log)
    
    # MLE - EM/Grid search (per feature)
    log_normal <- createEMClassForFeature(brain_feature_name, T, brain_feature_scaled@scaled_log_value)
    normal <- createEMClassForFeature(brain_feature_name, F, brain_feature_scaled@scaled_value)
    full_data_results[[i]] = 
        list(
          feature_name = brain_feature_name,
          MLE_log_normal = getEMResults(log_normal),
          MLE_normal = getEMResults(normal)
        )
    
    llr_per_feature[[i]] <- 
      c(brain_feature_name,
        getLlrt(log_normal), getLlrt(normal))
  }
  return(
    list(
      full_data_results = full_data_results,
      llr_per_feature = llr_per_feature
    )
  )
}


