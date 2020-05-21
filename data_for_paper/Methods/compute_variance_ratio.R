q2_q4_features <- read.csv('q2_q4_mixture_model_residual_data_with_t_test.csv')
q1_q3_features <- read.csv('q1_q3_mixture_model_residual_data_with_t_test.csv')
p_equals_q <- read.csv('single_mixture_model_residual_data_with_t_test.csv')

computeStandardDeviationRation <- function(m_parameters){
  variance_ratio <- m_parameters$sigma_2_men /m_parameters$sigma_2_women
  return(variance_ratio)
}

computeWideDistributionPreference <- function(m_parameters){
  if(m_parameters$sigma_2_men > m_parameters$sigma_2_women){
    return(c(m_parameters$p, m_parameters$q))
  }
  else{
    return(c(1 - m_parameters$p, 1 - m_parameters$q))
  }
}

biobank_feature_residual_analysis[['']]

variance_ratio <- sapply(biobank_feature_residual_analysis[c(q2_q4_features$feature, q1_q3_features$feature, p_equals_q$feature)], 
                        function(x){computeStandardDeviationRation(x$hypothesis_results$mixture_model$m_parameters)})
plot(sort(variance_ratio))
extreme_variance_ratio <- variance_ratio[variance_ratio > 2 | variance_ratio < 0.5]
str(biobank_feature_residual_analysis[[names(variance_ratio)[which.min(variance_ratio)]]])

wide_distribution_preferences <- sapply(biobank_feature_residual_analysis[names(extreme_variance_ratio)], function(x){computeWideDistributionPreference(x$hypothesis_results$mixture_model$m_parameters)})
names(wide_distribution_preferences) <- names(extreme_variance_ratio)
wide_distribution_preferences_df <- data.frame(W_m = wide_distribution_preferences[1,],
                                               W_w = wide_distribution_preferences[2,])

which(names(wide_distribution_preferences) %in% c('Weighted-mean MD in tract superior thalamic radiation (left)',
'Weighted-mean MD in tract superior thalamic radiation (right)'))

biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters
biobank_feature_residual_analysis[[q1_q3_features_names[156]]]$hypothesis_results$mixture_model$m_parameters

library(ggplot2)
ggplot(wide_distribution_preferences_df, aes(W_m, W_w)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  ggtitle('Men and women wide distribution preferences') + 
  coord_fixed()
  
  plot(wide_distribution_preferences[1,],
     wide_distribution_preferences[2,],
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = 'W_m',
     ylab = 'W_w')
abline(a = 0, b = 1, col = 'red')

