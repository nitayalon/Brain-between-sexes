# This is first draft of the single distribution vs composite 
# distribution hypothesis test
simpleDistributionVsCompositeDistribution <- function(feature_data
                                                      ,gender_data
                                                      ,distribution_model=c("LogNormal","Normal"),
                                                      is_data_scaled_and_logged = T)
{
  distribution_model=match.arg(distribution_model)
  
  full_data <- cbind(feature_data, gender_data)
  full_data <- na.omit(full_data) %>% as.data.frame()
  names(full_data) <- c("value","bio_sex")
  
  if(!is_data_scaled_and_logged)
  {
    processed_data <- switch (
      distribution_model,
      LogNormal = logNormalDataPreparation(full_data$value[full_data$value > 0]),
      Normal = normalDataPreparation(full_data$value)
                          )
    
    data_for_EM <- switch (
      distribution_model,
      LogNormal = full_data[full_data$value > 0,],
      Normal = full_data
                          )
      
    data_for_EM$value <- processed_data
  }
  else
  {
    processed_data <- full_data$value
    data_for_EM <- full_data
    names(data_for_EM) <- c("bio_sex","value")
  }
  # null hypothesis llk
  null_hypothesis_llk <- singleDistributionHypothesisLoglikelihood(processed_data)
  # composite hypothesis llk
  composite_hypothesis_llk <- 
    tryCatch(
    {doubleDoubleEM(data_for_EM,T)},
    error=function(cond)
    {
      return(data.frame(llk = -Inf))
    }
    )
  # llrt
  llrt <- tail(composite_hypothesis_llk$llk, n=1) - null_hypothesis_llk
}