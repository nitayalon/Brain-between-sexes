feature_name_for_presentation <- 'Volume of grey matter in Lateral Occipital Cortex, superior division (left)'
plotGenderHistogram(biobank_residuals_data[[feature_name_for_presentation]],
                    biobank_feature_residual_analysis[[feature_name_for_presentation]],
                    feature_name_for_presentation,
                    two_mixtures = F,
                    single_gaussian = T)
plotGenderHistogram(biobank_residuals_data[[feature_name_for_presentation]],
                    biobank_feature_residual_analysis[[feature_name_for_presentation]],
                    feature_name_for_presentation,
                    two_mixtures = T)
