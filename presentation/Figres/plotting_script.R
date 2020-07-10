feature_name_for_presentation <- 'Volume of grey matter in Lateral Occipital Cortex, superior division (left)'

feature_name_for_presentation <- 'Mean FA in retrolenticular part of internal capsule on FA skeleton (left)'

feature_name_for_presentation <- 'Mean FA in middle cerebellar peduncle on FA skeleton'

feature_name_for_presentation <- 'Volume of grey matter in Intracalcarine Cortex (right)'

feature_name_for_presentation <- 'Weighted-mean MD in tract parahippocampal part of cingulum (right)'

feature_name_for_presentation <- 'Mean FA in cingulum cingulate gyrus on FA skeleton (left)' # Q2-Q4 Mean MD 1

feature_name_for_presentation <- 'Mean FA in retrolenticular part of internal capsule on FA skeleton (left)'


feature_name_for_presentation <- 'Volume of grey matter in Postcentral Gyrus (left)'

feature_name_for_presentation <- 'Mean FA in medial lemniscus on FA skeleton (right)'


plotGenderHistogram(biobank_residuals_data[[pure_type_model_feature_name]],
                    biobank_feature_residual_analysis[[pure_type_model_feature_name]],
                    pure_type_model_feature_name)

plotGenderHistogram(biobank_residuals_data[[feature_name_for_presentation]],
                    biobank_feature_residual_analysis[[feature_name_for_presentation]],
                    feature_name_for_presentation,
                    two_mixtures = T)

mock_data = data.frame(value = c(rnorm(10000, -0.1, 0.2), rnorm(10000, 0.1, 0.2)),
                       sex = c(rep(0,10000), rep(1,10000)))
plotGenderHistogram(mock_data,
                    two_mixtures = F,single_gaussian = T)
