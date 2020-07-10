library(dplyr)
features_for_export = c('Volume of grey matter in Planum Polare (right)'                               ,                            
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

data_fo_isaco = lapply(features_for_export, function(x){biobank_residuals_data[[x]]})
table_for_isaco = data_fo_isaco[[1]]
for(i in 2:length(data_fo_isaco)){
  feat_data = data_fo_isaco[[i]] %>% select(eid, value)
  table_for_isaco = merge(table_for_isaco, feat_data, by.x = 'eid', by.y = 'eid')
}
str(table_for_isaco)
write.csv(table_for_isaco, 'BioBank_data_analysis/Log_likelihood_ratio_computation/data_for_isaco.csv')
