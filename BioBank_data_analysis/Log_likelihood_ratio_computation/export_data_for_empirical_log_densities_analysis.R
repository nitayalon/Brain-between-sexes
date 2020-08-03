library(dplyr)
features_for_export = c('Volume of grey matter in Planum Polare (right)',                            
'Volume of grey matter in Postcentral Gyrus (left)',
'Volume of grey matter in Cingulate Gyrus, anterior division (right)',
'Volume of grey matter in Lateral Occipital Cortex, superior division (left)',
'Volume of grey matter in IX Cerebellum (right)',
'Volume of grey matter in Vermis X Cerebellum',
'Volume of grey matter in Planum Polare (left)',
'Mean FA in fornix cres+stria terminalis on FA skeleton (left)',
'Volume of grey matter in Cingulate Gyrus, anterior division (left)',
'Volume of grey matter in IX Cerebellum (left)',
'Mean MD in inferior cerebellar peduncle on FA skeleton (right)',
'Volume of grey matter in Caudate (left)',
'Weighted-mean MD in tract acoustic radiation (right)')
set.seed(6431)
features_for_export_new = names(biobank_feature_residual_analysis)

em_parameters_for_llk_analysis = lapply(features_for_export, function(x){biobank_feature_residual_analysis[[x]]$hypothesis_results$mixture_model$m_parameters})

em_parameters_for_llk_analysis[1]
i = 13
round(1 - em_parameters_for_llk_analysis[[i]]$p,4)
round(1 - em_parameters_for_llk_analysis[[i]]$q,4)

data_fo_isaco = lapply(features_for_export_new, function(x){biobank_residuals_data[[x]]})
  
table_for_isaco = biobank_residuals_data[[1]]
for(i in 2:length(names(biobank_residuals_data))){
  feat_data = biobank_residuals_data[[i]]
  table_for_isaco = merge(table_for_isaco, feat_data, by.x = 'eid', by.y = 'eid', all = T)
}
dim(table_for_isaco)
table_for_isaco[is.na(table_for_isaco)] <- -10
write.csv(table_for_isaco, 'BioBank_data_analysis/Log_likelihood_ratio_computation/data_for_isaco_new.csv')

features_for_export_new = c(
'Volume of grey matter in Vermis X Cerebellum',
'Volume of grey matter in Planum Polare (left)')
data_fo_isaco = lapply(features_for_export_new, function(x){biobank_residuals_data[[x]]})
table_for_isaco = data_fo_isaco[[1]]
for(i in 2:length(data_fo_isaco)){
  feat_data = data_fo_isaco[[i]] %>% select(eid, value)
  table_for_isaco = merge(table_for_isaco, feat_data, by.x = 'eid', by.y = 'eid')
}
write.csv(table_for_isaco, 'BioBank_data_analysis/Log_likelihood_ratio_computation/data_for_isaco_new.csv')


logistic_cdf = read.csv('BioBank_data_analysis/Log_likelihood_ratio_computation/jul19.csv')
dim(logistic_cdf)
cut_off = 13
par(mfrow=c(1,2))
plot(logistic_cdf[,1] , col = 1, ylim = c(-1,1))
for(i in 2:13){
  points(logistic_cdf[,i], col = i)
}
plot(-logistic_cdf[,14], col = 1)
for(i in 15:26){
  points(-logistic_cdf[,i], col = i)
}

par(mfrow=c(1,1))
plot(logistic_cdf[,4] , col = 1)
points(logistic_cdf[,5] , col = 2)
points(logistic_cdf[,6] , col = 3)
points(logistic_cdf[,7] , col = 4)
i = 4
plot(logistic_cdf[,i] , col = 1, ylim = c(-2,2))
points(logistic_cdf[,i+1] , col = 2)
points(logistic_cdf[,i+2] , col = 3)
points(logistic_cdf[,i+3] , col = 4)

