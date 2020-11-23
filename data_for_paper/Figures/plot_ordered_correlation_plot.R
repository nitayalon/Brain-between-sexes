plotCorrelationPlot(q2_q4_correlation_matrix, 'Mean FA', type = 'number')
plotCorrelationPlot(q2_q4_correlation_matrix, 'Mean MD', type = 'number')
plotCorrelationPlot(q2_q4_correlation_matrix, 'Volume', type = 'number')


# sorted correaltion plot -------------------------------------------------
features_for_correlation_comparision = c(q2_q4_features_names, q1_q3_features_names, p_equals_q$feature)
sex_correlations_for_plot = createCorrelationDataFrameForSexComparison(biobank_feature_residual_analysis, features_for_correlation_comparision)

mens_data = sex_correlations_for_plot$correlation[sex_correlations_for_plot$sex == 'Men']
womens_data = sex_correlations_for_plot$correlation[sex_correlations_for_plot$sex == 'Women']
plot(mens_data[order(mens_data)], type = 'l', col = 'navyblue', cex.axis=1.5, cex.lab=1.5, ylab = 'Correlation coefficient', lwd = 5)
lines(womens_data[order(womens_data)], type = 'l', col = 'tomato', lwd = 5)
legend(0, y=1, legend = c('Men','Women'), fill = c('navyblue','tomato'), cex = 1.5)

sex_correlations_df = read.csv('sex_correlation_df.csv')
mens_data = sex_correlations_df$men_residual_correlation
womens_data = sex_correlations_df$women_residual_correlation
plot(mens_data[order(mens_data)], type = 'l', col = 'navyblue', cex.axis=1.5, cex.lab=1.5, ylab = 'Correlation coefficient', lwd = 5)
lines(womens_data[order(womens_data)], type = 'l', col = 'tomato', lwd = 5)
legend(0, y=1, legend = c('Men','Women'), fill = c('navyblue','tomato'), cex = 1.5)