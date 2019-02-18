## KS Test for brain feature - BioBank data
applyKSTestPerFeature <- function(feature_data) {
  
  log_clean_feature_data <- log(unique(na.omit(feature_data)))
  log_clean_feature_data <- 
    log_clean_feature_data[log_clean_feature_data > -Inf]
  h0_log_mean <- mean(log_clean_feature_data)
  h0_log_standard_diviation <- sd(log_clean_feature_data)
  lnormal_ks <- ks.test(log_clean_feature_data,"pnorm",h0_log_mean
                        ,h0_log_standard_diviation)
  return(lnormal_ks)  
}