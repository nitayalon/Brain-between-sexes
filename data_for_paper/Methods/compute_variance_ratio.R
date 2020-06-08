library(ggplot2)
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

isTailFeature <- function(m_parameters){
  high_tail_indicator <- m_parameters$p < 0.15 && m_parameters$q < 0.15
  lower_tail_indicator <- (1 - m_parameters$p) < 0.15 && (1- m_parameters$q) < 0.15
  if(high_tail_indicator || lower_tail_indicator){
    return(T)
  }
  else{
    return(F)
  }
}

computeTailDistributionPreference <- function(m_parameters){
  if(m_parameters$p > 0.85 &&  m_parameters$q > 0.58){
    return(c(1 - m_parameters$p, 1 - m_parameters$q))
  }
  else{
    return(c(m_parameters$p, m_parameters$q))
  }
}

variance_ratio <- sapply(biobank_feature_residual_analysis[c(q2_q4_features$feature, q1_q3_features$feature, p_equals_q$feature)], 
                        function(x){computeStandardDeviationRation(x$hypothesis_results$mixture_model$m_parameters)})
tail_features <- sapply(biobank_feature_residual_analysis[c(q2_q4_features$feature, q1_q3_features$feature, p_equals_q$feature)], 
                        function(x){isTailFeature(x$hypothesis_results$mixture_model$m_parameters)})
sum(sort(variance_ratio) > 3)
extreme_variance_ratio <- variance_ratio[variance_ratio > 2 | variance_ratio < 0.5]
str(biobank_feature_residual_analysis[[names(variance_ratio)[which.min(variance_ratio)]]])


wide_distribution_preferences <- sapply(biobank_feature_residual_analysis[names(extreme_variance_ratio)], function(x){computeWideDistributionPreference(x$hypothesis_results$mixture_model$m_parameters)})
names(wide_distribution_preferences) <- names(extreme_variance_ratio)
tail_distribution_preferences <-sapply(biobank_feature_residual_analysis[names(tail_features[which(tail_features)])], function(x){computeTailDistributionPreference(x$hypothesis_results$mixture_model$m_parameters)})
names(tail_distribution_preferences) <- names(tail_features[which(tail_features)])

plotGenderHistogram(biobank_residuals_data[['Weighted-mean MD in tract acoustic radiation (left)']],
                    biobank_feature_residual_analysis[['Weighted-mean MD in tract acoustic radiation (left)']], 
                    'Weighted-mean MD in tract acoustic radiation (left)')

plotGenderHistogram(biobank_residuals_data[['Mean FA in posterior corona radiata on FA skeleton (left)']],
                    biobank_feature_residual_analysis[['Mean FA in posterior corona radiata on FA skeleton (left)']], 
                    'Mean FA in posterior corona radiata on FA skeleton (left)')

wide_distribution_preferences_df <- data.frame(W_m = wide_distribution_preferences[1,],
                                               W_w = wide_distribution_preferences[2,])
wide_distribution_preferences_df$Feature <- row.names(wide_distribution_preferences_df)

tail_distribution_preferences_df <- data.frame(Feature = names(tail_features[which(tail_features)]),
  W_m = tail_distribution_preferences[1,],
                                               W_w = tail_distribution_preferences[2,])

combined_data_frame_for_tail_distributions <- merge(wide_distribution_preferences_df, tail_distribution_preferences_df,
                                                    by = 'Feature', suffixes = c('Variance','Tail'),
                                                    all = TRUE)
# 24/05/2020 addition -----------------------------------------------------

tail_features <- 

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

