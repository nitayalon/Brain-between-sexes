computeFisherMatrixForFeature <- function(biobank_feature_residual_analysis, 
                                          biobank_residuals_data,
                                          feature_name)
{
  mle_parameters_list <- biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters
  
  observations <- biobank_residuals_data[[feature_name]]
  
  h <- computeHessianForMixtureModel(observations, mle_parameters_list)
  fisher <- -h
  eigen_values = eigen(fisher)
  return(list(
    hessian = h,
    fisher = fisher,
    eigen_values = eigen_values
  ))
}