createCorrelationDataFrameForSexComparison = function(residual_data, features){
  sex_wise_correlations = data.frame()
  groups = c('Volume','Mean FA','Mean MD','Weighted-mean FA','Weighted-mean MD')
  for(group in groups){
    feature_index = grep(group, features)
    men_residual_data_for_corr_plot = lapply(residual_data[features[feature_index]],function(x){x$hypothesis_results$mixture_model$men_responsebilities})
    women_residual_data_for_corr_plot = lapply(residual_data[features[feature_index]],function(x){x$hypothesis_results$mixture_model$women_responsebilities})
    men_feature_data <- men_residual_data_for_corr_plot[[1]]
    for(i in 2:length(men_residual_data_for_corr_plot))
    {
      men_feature_data <- base::merge(men_feature_data, men_residual_data_for_corr_plot[[i]], by.x = "eid", by.y = "eid")
    }
    names(men_feature_data) <- c("eid", features[feature_index])
    men_features_no_id <- men_feature_data[,-1]
    
    women_feature_data <- women_residual_data_for_corr_plot[[1]]
    for(i in 2:length(women_residual_data_for_corr_plot))
    {
      women_feature_data <- base::merge(women_feature_data, women_residual_data_for_corr_plot[[i]], by.x = "eid", by.y = "eid")
    }
    names(women_feature_data) <- c("eid", features[feature_index])
    women_features_no_id <- women_feature_data[,-1]
    men_cor <- cor(men_features_no_id, use = "pairwise.complete.obs")
    women_cor <- cor(women_features_no_id,use = "pairwise.complete.obs")
    tmp = data.frame(correlation = c(men_cor[upper.tri(men_cor)], women = women_cor[upper.tri(women_cor)]),sex = c(rep('Men', length(men_cor[upper.tri(men_cor)])), 
                                                                                                                   rep('Women', length(women_cor[upper.tri(women_cor)]))))
    sex_wise_correlations = rbind(sex_wise_correlations,tmp)
  }
  return(sex_wise_correlations)
}