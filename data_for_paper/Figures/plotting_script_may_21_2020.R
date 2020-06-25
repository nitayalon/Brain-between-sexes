plotGenderHistogram(biobank_residuals_data[[q1_q3_features_names[155]]],
                    biobank_feature_residual_analysis[[q1_q3_features_names[155]]], q1_q3_features_names[155])
plotGenderHistogram(biobank_residuals_data[[q1_q3_features_names[156]]],
                    biobank_feature_residual_analysis[[q1_q3_features_names[156]]], q1_q3_features_names[156])


hist(biobank_residuals_data[[q1_q3_features_names[155]]]$value)
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters$mu_1
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters$mu_2
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters$sigma_2_men
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters$sigma_2_women
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters$p
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters$q

df <- merge(biobank_residuals_data[[q1_q3_features_names[156]]] %>%
  filter(sex == 1) %>%
  select(eid, value), 
biobank_residuals_data[[q1_q3_features_names[155]]] %>%
  filter(sex == 1) %>%
  select(eid, value),by.x = 'eid', by.y = 'eid') 

cor(df$value.x, df$value.y)

df2 <- merge(biobank_residuals_data[[q1_q3_features_names[155]]] %>%
               filter(sex == 1) %>%
               select(eid, value), biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$men_responsebilities,by.x = 'eid', by.y = 'eid')

plot(df1$value, df1$responsebility, main = '156')
plot(df2$value, df2$responsebility, main = '155')

cor(biobank_feature_residual_analysis[[q1_q3_features_names[156]]]$hypothesis_results$mixture_model$men_responsebilities,
    biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$e_parameters$I[-1])

cor(biobank_feature_residual_analysis[[q1_q3_features_names[156]]]$hypothesis_results$mixture_model$men_responsebilities$responsebility,
    biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$men_responsebilities$responsebility[-1])

cor(q1_q3_correlation_matrix$men_features_no_id[,155],
    q1_q3_correlation_matrix$men_features_no_id[,156])

biobank_feature_residual_analysis$`Weighted-mean MD in tract superior thalamic radiation (left)`$hypothesis_results$mixture_model$m_parameters
biobank_feature_residual_analysis[[q1_q3_features_names[155]]]$hypothesis_results$mixture_model$m_parameters

feature_name <- 'Volume of grey matter in Postcentral Gyrus (left)'
plotGenderHistogram(biobank_residuals_data[[feature_name]],
                    biobank_feature_residual_analysis[[feature_name]], feature_name,two_mixtures = F)




# Nice plot ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
load('df_p_vs_q.RData')
ggplot(df_p_vs_q, aes(p,q)) +
  geom_point(aes(colour = bins_for_cohen_d,shape = p_equal_q)) +
  scale_shape_manual(values=c(3,19))+
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_manual(name = "Cohen D",
                     values = c("(-0.3,-0.2]" = "red4",
                                "(-0.2,-0.1]" = "red1",
                                "(-0.1,0.1]" = "orange1",
                                "(0.1,0.2]" = "deepskyblue2",
                                "(0.2,0.3]" = "dodgerblue1",
                                "(0.3,0.4]" = "blue4"),
                     labels = c("<-0.2","0.2<=d<-0.1","-0.1<=d<0.1","0.1<=d<0.2","0.2<=d<3","0.3<=d<0.4")) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10)) +
  coord_fixed()


# Ordered correlation plot ------------------------------------------------

ggplot(volume_ordered_correlations_melted, aes(x = index, y = Correlation, color = Sex)) + 
  geom_line() + 
  scale_color_manual(values = alpha(c('tomato','dodgerblue'),1)) + 
  ylab('Correlation') + 
  ggtitle('Sotred correlations, all regions')

# Adding nice plot for tails ----------------------------------------------

findEqualTailedFeatures <- function(feature_data){
  cohen_d_indicator <- abs(feature_data$hypothesis_results$mixture_model$m_parameters$mu_1 - 
                             feature_data$hypothesis_results$mixture_model$m_parameters$mu_2) < 0.1
  p_equals_q_indiciator <- feature_data$hypothesis_results$mixture_model$m_parameters$sigma_2_men / feature_data$hypothesis_results$mixture_model$m_parameters$sigma_2_women > 2
  if(cohen_d_indicator && p_equals_q_indiciator)
  {
    return(T)  
  }
  else{
    return(F)
  }
}


features_for_two_sides_tail_plot <- sapply(biobank_feature_residual_analysis, function(x){findEqualTailedFeatures(x)})
which(features_for_two_sides_tail_plot)

plotGenderHistogram(biobank_residuals_data[[names(features_for_two_sides_tail_plot)[which(features_for_two_sides_tail_plot)]]],
                    biobank_feature_residual_analysis[[names(features_for_two_sides_tail_plot)[which(features_for_two_sides_tail_plot)]]], 
                    names(features_for_two_sides_tail_plot)[which(features_for_two_sides_tail_plot)])

# Variance ratio plot with tailed features marked -------------------------

str(variance_ratio)
