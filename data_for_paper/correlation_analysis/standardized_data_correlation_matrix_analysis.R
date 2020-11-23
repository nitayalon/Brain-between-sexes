# Extract only FDR significant features
log_feature_fdr_significant_features_names = rownames(log_feature_fdr_analysis)[log_feature_fdr_analysis$bh_pvalue < 0.05]
# partition to groups
feature_groups_suffix = c('Volume','Mean FA', 'Mean MD', 'Weighted-mean FA', 'Weighted-mean MD')
log_feature_fdr_significant_features_names[grep(feature_groups_suffix[1], log_feature_fdr_significant_features_names)]
# For each feature, compute the responsebilities of all the valid data (wrt to the high-mean):
feature_name = log_feature_fdr_significant_features_names[grep(feature_groups_suffix[1], log_feature_fdr_significant_features_names)][1]
men_cor_data = c()
women_cor_data = c()
for(group in feature_groups_suffix)
{
  results = compute_correlation_matrix_per_region(group)
  men_cor_data = c(men_cor_data, results$men_cor_matrix)
  women_cor_data = c(women_cor_data, results$women_cor_matrix)
}

## Plot for Dafi
both_genders_log_feature_correlation_vector_df = data.frame(index = seq(1,length(sort(men_cor_data))),
                                                          men_residual_correlation = sort(men_cor_data),
                                                          women_residual_correlation = sort(women_cor_data))
write_csv(both_genders_log_feature_correlation_vector_df, "sex_correlation_df.csv")
ggplot(both_genders_log_feature_correlation_vector_df, aes(x = index)) + 
  geom_line(aes(y = men_residual_correlation, color = 'men')) + 
  geom_line(aes(y = women_residual_correlation, color = 'women')) + 
  ggtitle("Men and women sorted residual correlation") + 
  xlab("Index") + 
  ylab('Correlation')


compute_correlation_matrix_per_region = function(region, ...)
{
  men_responsebilities_df = bio_bank_log_feature_data %>% 
    filter(sex == 1) %>% 
    select(eid)
  women_responsebilities_df = bio_bank_log_feature_data %>% 
    filter(sex == 0) %>% 
    select(eid)
  # Filter features
  feature_names = log_feature_fdr_significant_features_names[grep(region, log_feature_fdr_significant_features_names)]
  feature_names = sort(feature_names)
  for(feature in feature_names)
  {
    responsibilities = compute_responsebilities_vector(feature)
    men_responsebilities_df = full_join(men_responsebilities_df, 
                                        responsibilities$men_measurements %>% select(eid, men_responsebilities), by = c('eid','eid'))
    women_responsebilities_df = full_join(women_responsebilities_df, 
                                          responsibilities$women_measurements %>% select(eid, women_responsebilities), by = c('eid','eid') )
  }
  names(men_responsebilities_df) = c('eid', feature_names)
  names(women_responsebilities_df) = c('eid', feature_names)
  men_cor_matrix = cor(men_responsebilities_df, use = 'pairwise.complete.obs')
  women_cor_matrix = cor(women_responsebilities_df, use = 'pairwise.complete.obs')
  return(list(men_cor_matrix = as.vector(men_cor_matrix[upper.tri(men_cor_matrix)]),
              women_cor_matrix = as.vector(women_cor_matrix[upper.tri(women_cor_matrix)])))
  
}

compute_responsebilities_vector = function(feature_name)
{
  # extract EM parameters
  em_parameters = log_feature_em[[feature_name]]$mixture_model$m_parameters
  # Extract individual measurements and sex
  men_measurements = bio_bank_log_feature_data %>% 
    filter(sex == 1) %>% 
    filter(!!sym(feature_name) > -Inf) %>% 
    select(eid, feature_name) %>% 
    drop_na() %>% 
    mutate(scale = (!!sym(feature_name) - mean(!!sym(feature_name))) / sd(!!sym(feature_name)))
  
  women_measurements = bio_bank_log_feature_data %>% 
    filter(sex == 0) %>% 
    filter(!!sym(feature_name) > -Inf) %>% 
    select(eid, feature_name) %>% 
    drop_na() %>% 
    mutate(scale = (!!sym(feature_name) - mean(!!sym(feature_name))) / sd(!!sym(feature_name)))
  
  if(em_parameters$mu_1 >= 0)
  {
    men_measurements$men_responsebilities = (em_parameters$p * dnorm(men_measurements$scale, em_parameters$mu_1, sqrt(em_parameters$sigma_2_men))) /
      (
        (em_parameters$p * dnorm(men_measurements$scale, em_parameters$mu_1, sqrt(em_parameters$sigma_2_men))) + 
          ((1 - em_parameters$p) * dnorm(men_measurements$scale, em_parameters$mu_2, sqrt(em_parameters$sigma_2_women)))
      )
    women_measurements$women_responsebilities = (em_parameters$q * dnorm(women_measurements$scale, em_parameters$mu_1, sqrt(em_parameters$sigma_2_men))) /
      (
        (em_parameters$q * dnorm(women_measurements$scale, em_parameters$mu_1, sqrt(em_parameters$sigma_2_men))) + 
          ((1 - em_parameters$q) * dnorm(women_measurements$scale, em_parameters$mu_2, sqrt(em_parameters$sigma_2_women)))
      )
  }else
  {
    men_measurements$men_responsebilities = (1-em_parameters$p) * dnorm(men_measurements$scale, em_parameters$mu_2, sqrt(em_parameters$sigma_2_women)) /
        (em_parameters$p * dnorm(men_measurements$scale, em_parameters$mu_1, sqrt(em_parameters$sigma_2_men)) + 
          (1 - em_parameters$p) * dnorm(men_measurements$scale, em_parameters$mu_2, sqrt(em_parameters$sigma_2_women)))
      
    women_measurements$women_responsebilities = (1-em_parameters$q) * dnorm(women_measurements$scale, em_parameters$mu_2, sqrt(em_parameters$sigma_2_women)) /
        (em_parameters$q * dnorm(women_measurements$scale, em_parameters$mu_1, sqrt(em_parameters$sigma_2_men)) + 
          (1 - em_parameters$q) * dnorm(women_measurements$scale, em_parameters$mu_2, sqrt(em_parameters$sigma_2_women)))
      
  }
  return(list(men_measurements = men_measurements,
              women_measurements = women_measurements))  
}
