#' @param biobank_feature_data: list, one entry per feature
#' @return data.frame: results of FDR correction over the hypotheses.  

fullFdrAnalysis <- function(biobank_feature_data)
{
  data_m_parameters <- 
    convertEMProbabiltiesToPQSpace(biobank_feature_data)
  p_and_q <- 
    sapply(data_m_parameters, function(x){c(x$p, x$q)})
  
  pvalue_per_feature <- pullPvaluePerHypothesis(biobank_feature_data)
  
  pvalue_per_feature_df <- t(pvalue_per_feature) %>% as.data.frame() 
  
  p_and_q_df <- p_and_q %>% as.data.frame()
  
  cohens_d_per_feature <- sapply(biobank_feature_data, function(x){x$cohen_d_test$estimate})
  
  combined_df <- mergeDataFramesByColumnNames(pvalue_per_feature_df, p_and_q_df)
  
  combined_df <- rbind(combined_df, cohens_d_per_feature)
  rownames(combined_df) <- c("p_values","p","q","Cohen_D")
  combined_df_long <- combined_df %>% t() %>% as.data.frame()
  names(combined_df_long) =  c("p_values","p","q","Cohen_D")
  combined_df_long$bins_of_pv <- cut(combined_df_long$p_values, c(1e-9,1e-8,1e-3,1e-2,1))
  
  N <- nrow(combined_df_long)
  combined_df_long_sorted <- combined_df_long[order(combined_df_long$p_values,decreasing = F),]
  combined_df_long_sorted$i <- seq(1:N)
  combined_df_long_sorted$bh_pvalue <- with(combined_df_long_sorted, p_values * i / N)
  combined_df_long_sorted$region <- apply(combined_df_long_sorted, 1, function(x){fitQuadrant(x[2],x[3])})
  
  combined_df_long_sorted$fdr_pv <- 
    sapply(c(1:nrow(combined_df_long_sorted)), function(i){
      max(combined_df_long_sorted$bh_pvalue[1:i])
    })
  
  combined_df_long_sorted$bins_for_fdr <- cut(combined_df_long_sorted$fdr_pv, c(0,1e-2,5e-2,1))
  
  return(combined_df_long_sorted)
  
}