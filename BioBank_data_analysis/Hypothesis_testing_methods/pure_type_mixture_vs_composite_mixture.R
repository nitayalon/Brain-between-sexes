#' Testing pure types mixture model vs
#' mixture of mixtures model
pureTypeMixtureVsCompositeMixture <- function(feature_data
                                              ,gender_data
                                              ,user_id
                                              ,distribution_model=c("LogNormal","Normal")
                                              ,return_full_data = F,
                                              data_needs_preparation = T)
{
  distribution_model=match.arg(distribution_model)
  
  full_data <- cbind(feature_data,gender_data,user_id)
  full_data <- na.omit(full_data) %>% as.data.frame()
  names(full_data) <- c("value","bio_sex","eid")
  
  if(data_needs_preparation)
  {
    processed_data <- switch (distribution_model,
                              LogNormal = logNormalDataPreparation(full_data$value[full_data$value > 0]),
                              Normal = normalDataPreparation(full_data$value)
    )
    data_for_EM <- switch (distribution_model,
                           LogNormal = full_data[full_data$value > 0,],
                           Normal = full_data
    )
    
    data_for_EM$value <- processed_data
  }
  else
  {
    data_for_EM <- full_data
  }
  # null hypothesis llk
  null_hypothesis_llk <- 
    pureTypeMixtureModelHypothesis(data_for_EM)
  # composite hypothesis llk
  composite_hypothesis_llk <- 
    tryCatch(
      {
        doubleDoubleEM(data_for_EM,T)
      },
      error=function(cond)
      {
        return(data.frame(llk = -Inf))
      }
    )
  # llrt
  llrt <- tail(composite_hypothesis_llk$llk, n=1) - null_hypothesis_llk$llk
  composite_results <- list(alternative_hypothesis = composite_hypothesis_llk,
                            null_hypothesis = null_hypothesis_llk,
                            llrt = llrt,
                            wilks_statistic = 
                              tail(composite_hypothesis_llk$llk, n=1) - null_hypothesis_llk$llk)
  if(return_full_data)
  {
    return(composite_results)
  }
  return(llrt)
}