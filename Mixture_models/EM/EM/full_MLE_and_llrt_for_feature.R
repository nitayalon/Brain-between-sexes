fullMLEAndLlrtForFeature <- function(feature_data,...)
{
  # Step 1 - Run the EM algorithm to find MLE set
  EM_results <- doubleDoubleEM(feature_data)
  # Step 2 - compute the llk for the null hypothesis
  null_data <- logNormalNullLlkComputation(feature_data)
  # Step 3 - compute the llrt for the feature
  llrt <- tail(EM_results$llk, n=1) - null_data$llk
  # Step 4 - return the results
  
  feature_analysis_results <- list(
    EM_results = EM_results,
    null_data = null_data,
    llrt = llrt
  )
  
  return(feature_analysis_results)
}