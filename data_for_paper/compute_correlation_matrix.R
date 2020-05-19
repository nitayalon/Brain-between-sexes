computeCorrelationMatrix <- function(features_names){
  
  # features <- biobank_feature_residual_analysis[features_names]
  modified_features <- ComputeResponsibilities(features_names)
  
  number_of_revesed_features <- sapply(modified_features, function(x){x[[1]]})
  
  features <- lapply(modified_features, function(x){x[[2]]})
  
  men_data <- lapply(features, function(x){x$hypothesis_results$mixture_model$men_responsebilities})
  
  women_data <- lapply(features, function(x){x$hypothesis_results$mixture_model$women_responsebilities})
  men_feature_data <- men_data[[1]]
  for(i in 2:length(men_data))
  {
    men_feature_data <- base::merge(men_feature_data, men_data[[i]], by.x = "eid", by.y = "eid")
  }
  names(men_feature_data) <- c("eid", features_names)
  men_features_no_id <- men_feature_data[,-1]
  women_feature_data <- women_data[[1]]
  for(i in 2:length(women_data))
  {
    women_feature_data <- base::merge(women_feature_data, women_data[[i]], by.x = "eid", by.y = "eid")
  }
  names(women_feature_data) <- c("eid", features_names)
  women_features_no_id <- women_feature_data[,-1]
  
  men_features_no_id <- men_features_no_id[order(names(women_features_no_id))]
  women_features_no_id <- women_features_no_id[order(names(women_features_no_id))]
  
  men_cor <- cor(men_features_no_id, use = "pairwise.complete.obs")
  women_cor <- cor(women_features_no_id,use = "pairwise.complete.obs")
  combined_correaltion <- men_cor
  combined_correaltion[lower.tri(combined_correaltion)] <- women_cor[lower.tri(women_cor)]
  
  correlation_legend <- data.frame(feature_name = colnames(men_cor), 
                                                            number = 1:ncol(combined_correaltion))
  colnames(combined_correaltion) <- rownames(combined_correaltion) <- 1:ncol(combined_correaltion)
  return(list(correlation_matrix = combined_correaltion,
              legend = correlation_legend,
              number_of_revesed_features = number_of_revesed_features))
}