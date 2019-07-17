# computing the llk of the feature set

logLikelihoodComputationForFeatureSet <- function(feature_data, 
                                                  parameter_row, 
                                                  is_bioBank_data = F,
                                                  ...)
{
  # stopifnot(length(parameter_row) == 5)
  if(is_bioBank_data)
  {
    # men_data <- feature_data$men
    # women_data <- feature_data$women
    men_data <- feature_data$value[feature_data$bio_sex == 1]
    women_data <- feature_data$value[feature_data$bio_sex == 0]
  }
  else
  {
    men_data <- feature_data$value[feature_data$bio_sex == 1]
    women_data <- feature_data$value[feature_data$bio_sex == 2]
  }
  theta_mas <- parameter_row[1]
  theta_fem <- parameter_row[2]
  p <- parameter_row[3]
  q <- parameter_row[4]
  sigma_2_men <- parameter_row[5]
  sigma_2_women <- parameter_row[6]
  llk <- computeLogLikelihood(men_data,
                              women_data,
                              theta_mas,
                              theta_fem,
                              p,
                              q,
                              sigma_2_men,
                              sigma_2_women)
  return(llk)
}