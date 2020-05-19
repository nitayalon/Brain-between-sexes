ComputeResponsibilities <- function(features_names){
  features <- biobank_feature_residual_analysis[features_names]
  modified_features <- lapply(features, function(feature){ChangeResponsibilityDirection(feature)})
  return(modified_features)
}

ChangeResponsibilityDirection <- function(feature){
  indicator <- feature$hypothesis_results$mixture_model$m_parameters$mu_1 < 
     feature$hypothesis_results$mixture_model$m_parameters$mu_2
  if(indicator){
    feature$hypothesis_results$mixture_model$men_responsebilities$responsebility =  
      (1 - feature$hypothesis_results$mixture_model$men_responsebilities$responsebility)
    feature$hypothesis_results$mixture_model$women_responsebilities$responsebility =  
      (1 - feature$hypothesis_results$mixture_model$women_responsebilities$responsebility)
  }
  return(list(indicator = indicator,
              feature = feature))
}