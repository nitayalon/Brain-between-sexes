all_non_pure_type_features = c(q2_q4_features_names, q1_q3_features_names, p_equals_q$feature)
feature_name = all_non_pure_type_features[1]
men_responsebilities_matrix = biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$men_responsebilities
women_responsebilities_matrix = biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$women_responsebilities

for(feature_name in all_non_pure_type_features[-1]){
  men_res = biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$men_responsebilities
  women_res = biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$women_responsebilities
  men_responsebilities_matrix = merge(men_responsebilities_matrix,men_res, by.x = 'eid', by.y = 'eid')
  women_responsebilities_matrix = merge(women_responsebilities_matrix,women_res, by.x = 'eid', by.y = 'eid')
}

colnames(men_responsebilities_matrix) = c('eid', all_non_pure_type_features)
colnames(women_responsebilities_matrix) = c('eid', all_non_pure_type_features)

write.csv(as.data.frame(men_responsebilities_matrix), file = '~/Human_brain_research/data_for_paper/Tables/men_responsebilities.csv')
write.csv(as.data.frame(women_responsebilities_matrix), file = '~/Human_brain_research/data_for_paper/Tables/women_responsebilities.csv')
