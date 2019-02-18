applyKSTestForDataFrame <- function(relevant_data,
                                    model_assumed)
{
  ks_results <- 
  sapply(relevant_data, function(x){applyFullKSTestPerFeature(x,model_assumed)})
  rownames(ks_results) <- c("KS_Statistics","P_Value")
  ks_results %<>% t() %>% data.frame()
  return(ks_results)
}