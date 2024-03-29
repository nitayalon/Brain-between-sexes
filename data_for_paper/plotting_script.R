library(tidyverse)
library(ggplot2)


# Plotting Cohen's and t-statistics ---------------------------------------

sapply(biobank_full_analysis_list, function(x){x$hypothesis_results$cohen_d_test$estimate}) %>% 
  data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(Cohens_D = ".") %>%
  ggplot(aes(x = Cohens_D)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="grey69",
                 bins=50,breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) + 
  theme_bw() + 
  xlim(c(-0.5, 0.5)) + 
  ylab("Density") + xlab("Cohen's D") + 
  theme(text = element_text(size = 18),
        panel.border = element_blank())

sapply(biobank_feature_residual_analysis, function(x){x$hypothesis_results$t_test_for_difference_between_genders$statistic}) %>% 
  data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(T_statistics = ".") %>% 
  ggplot(aes(x = T_statistics)) + 
  geom_histogram(aes(y=..density..), bins=15) + 
  theme_bw() + 
  ylab("Density") + xlab("T Statistic") + 
  theme(text = element_text(size = 18),
        panel.border = element_blank())


# Main plots --------------------------------------------------------------

figure_2 <- combined_df_resid_equal_probs_sorted %>% 
  mutate(Cohen_D_bins = cut(Cohen_D, c(-0.4,-0.3,-0.2,-0.1,0.1,0.2,0.3,0.4))) %>% 
  ggplot(aes(p,q, colour = Cohen_D_bins)) +
  geom_point(aes(shape = p_equal_q)) +
  scale_shape_manual(values=c(3, 19), guide = "none")+
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  #ggtitle("Values of p and q", subtitle = "shape is p=q hypothesis") + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_manual(name = "Cohen's d",
                     values = c("(-0.3,-0.2]" = "red4",
                                "(-0.2,-0.1]" = "red1",
                                "(-0.1,0.1]" = "orange1",
                                "(0.1,0.2]" = "deepskyblue2",
                                "(0.2,0.3]" = "dodgerblue1",
                                "(0.3,0.4]" = "blue4"),
                     labels = c("-0.3<=d<-0.2","0.2<=d<-0.1","-0.1<=d<0.1","0.1<=d<0.2","0.2<=d<3","0.3<=d<0.4")) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10),
        panel.border = element_blank()) +
  coord_fixed()

ggsave(file = file.path("figure_2.png"),
         plot = figure_2,
         width = 10,
         height = 7,
         units = "in",
         dpi = 300)  
# Old script --------------------------------------------------------------

jpeg('p_vs_q_residuals_plot_color_indicates_cohen_d_shape_indicates_p_equal_q_or_not.jpeg')
ggplot(df_p_vs_q, aes(p,q)) +
  geom_point(aes(colour = bins_for_cohen_d,shape = p_equal_q)) +
  scale_shape_manual(values=c(3, 19))+
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  ggtitle("Values of p and q", subtitle = "shape is p=q hypothesis") + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_manual(name = "Cohen's d",
                     values = c("(-0.3,-0.2]" = "red4",
                                "(-0.2,-0.1]" = "red1",
                                "(-0.1,0.1]" = "orange1",
                                "(0.1,0.2]" = "deepskyblue2",
                                "(0.2,0.3]" = "dodgerblue1",
                                "(0.3,0.4]" = "blue4"),
                     labels = c("-0.3<=d<-0.2","0.2<=d<-0.1","-0.1<=d<0.1","0.1<=d<0.2","0.2<=d<3","0.3<=d<0.4")) +
  theme_bw() + 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=7),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=7)) +
  coord_fixed()
dev.off()


# Plot gender histograms --------------------------------------------------

# Odd feature
which.max(combined_df_resid_equal_probs$q)
combined_df_resid_equal_probs[58, ]
odd_feature_name <- "Mean MD in cingulum hippocampus on FA skeleton (right)"

plotGenderHistogram(biobank_full_analysis_list[[odd_feature_name]]$feature_residuals,
                    biobank_full_analysis_list[[odd_feature_name]],
                    odd_feature_name)
                    
# Mixture model example
mixture_model_feature_name <- 'Volume of grey matter in Lingual Gyrus (left)'

jpeg(sprintf('histogram of %s log volume.jpeg', mixture_model_feature_name))

plotGenderHistogram(biobank_standardized_data[[mixture_model_feature_name]],
                    biobank_feature_standard_analysis[[mixture_model_feature_name]],
mixture_model_feature_name)
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', mixture_model_feature_name))
plotGenderHistogram(biobank_residuals_data[[mixture_model_feature_name]],
                    biobank_feature_residual_analysis[[mixture_model_feature_name]], mixture_model_feature_name)
dev.off()

# Equal proportions
single_mixture_model_feature_name <- 'Volume of grey matter in Occipital Fusiform Gyrus (left)'
jpeg(sprintf('histogram of %s log volume.jpeg', single_mixture_model_feature_name))
plotGenderHistogram(biobank_standardized_data[[single_mixture_model_feature_name]],
                    biobank_feature_standard_analysis[[single_mixture_model_feature_name]],
                    single_mixture_model_feature_name)
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', single_mixture_model_feature_name))
plotGenderHistogram(biobank_residuals_data[[single_mixture_model_feature_name]],
                    biobank_feature_residual_analysis[[single_mixture_model_feature_name]], single_mixture_model_feature_name)
dev.off()

# Pure types
# Plot only the underline puretype
pure_type_model_feature_name <- 'Volume of grey matter in Postcentral Gyrus (left)'
jpeg(sprintf('histogram of %s log volume.jpeg', pure_type_model_feature_name))
plotGenderHistogram(biobank_standardized_data[[pure_type_model_feature_name]],
                    biobank_feature_standard_analysis[[pure_type_model_feature_name]],
                    pure_type_model_feature_name)
dev.off()

jpeg(sprintf('histogram of %s residuals.jpeg', pure_type_model_feature_name))
plotGenderHistogram(biobank_residuals_data[[pure_type_model_feature_name]],
                    biobank_feature_residual_analysis[[pure_type_model_feature_name]], pure_type_model_feature_name)
dev.off()

#### Scatter plots
i <- 2
j <- 5
x_feature <- strsplit(volume_features_for_plot[i], split="in")[[1]][2] %>% trimws()
y_feature <- strsplit(volume_features_for_plot[j], split="in")[[1]][2] %>% trimws()
jpeg(sprintf('scatter_plot_of_%s_vs_%s_volume.jpeg',volume_features_for_plot[i],
             volume_features_for_plot[j]))
  inner_join(biobank_standardized_data[[volume_features_for_plot[i]]],
           biobank_standardized_data[[volume_features_for_plot[j]]],
           by = 'eid') %>% 
  select(`Planum Polare (right)` = value.x,
         `Insular Cortex (right)` = value.y,
         sex = sex.x) %>% 
  mutate(sex = factor(sex)) %>% 
  ggplot(aes(`Planum Polare (right)`, `Insular Cortex (right)`,
             color = sex)) + 
  geom_point(shape=20, size = 1) + 
  labs(title = sprintf('%s vs %s',
                       x_feature, y_feature
                       ),
         subtitle = "Log volume",
         caption = "Measure:Volume of gery matter") + 
  coord_fixed() 
dev.off()

jpeg(sprintf('scatter_plot_of_%s_vs_%s_residuals.jpeg',volume_features_for_plot[i],volume_features_for_plot[j]))
  inner_join(biobank_residuals_data[[volume_features_for_plot[i]]],
           biobank_residuals_data[[volume_features_for_plot[j]]],
           by = 'eid') %>% 
    select(`Planum Polare (right)` = value.x,
           `Insular Cortex (right)` = value.y,
           sex = sex.x) %>% 
    mutate(sex = factor(sex)) %>% 
    ggplot(aes(`Planum Polare (right)`, `Insular Cortex (right)`,
               color = sex)) + 
    geom_point(shape=20, size = 1) + 
    labs(title = sprintf('%s vs %s',
                         x_feature, y_feature
    ),
    subtitle = "Residuals",
    caption = "Measure:Volume of gery matter") + 
  coord_fixed() 
dev.off()


# Corrplots ---------------------------------------------------------------

## Pure types
write.csv(pure_type_correlation_plot_legend, "pure_type_correlation_plot_legend.csv")
jpeg('pure_types_probability_correlation_matrix.jpeg')
corrplot(pure_type_combined_correaltion_residuals, order = "alphabet", 
         method = 'circle',tl.cex = 1.0,tl.srt = 45,tl.col="black")
dev.off()

## Single mixture model
write.csv(single_mixture_model_corrplot_legend, "single_mixture_model_corrplot_legend.csv")

jpeg('single_mixture_model_probability_correlation_matrix_volume_features.jpeg')
corrplot(combined_correaltion_residuals[volume_features_single_mixture_model,volume_features_single_mixture_model], 
         order = "alphabet",tl.cex = 1.5,tl.srt = 90,tl.col="black")
dev.off()

jpeg('single_mixture_model_probability_correlation_matrix_md_features.jpeg')
corrplot(combined_correaltion_residuals[MD_features_single_mixture_model,MD_features_single_mixture_model], order = "alphabet",
         tl.cex = 1.5,tl.srt = 90,tl.col="black")
dev.off()

jpeg('single_mixture_model_probability_correlation_matrix_fa_features.jpeg')
corrplot(combined_correaltion_residuals[FA_features_single_mixture_model,FA_features_single_mixture_model], order = "alphabet",
         tl.cex = 1.5,tl.srt = 90,tl.col="black")
dev.off()

## Two mixture model
write.csv(two_mixture_model_corrplot_legend, "two_mixture_model_corrplot_legend.csv")

jpeg('two_mixture_model_probability_correlation_matrix_volume_features.jpeg')
corrplot(combined_correaltion_residuals[volume_features_two_mixture_model,volume_features_two_mixture_model], 
         order = "alphabet",tl.cex = 0.6,tl.col="black")
dev.off()

jpeg('two_mixture_model_probability_correlation_matrix_md_features.jpeg')
corrplot(combined_correaltion_residuals[MD_features_two_mixture_model,MD_features_two_mixture_model], order = "alphabet",
         tl.cex = 1.0,tl.srt = 45,tl.col="black")
dev.off()

jpeg('two_mixture_model_probability_correlation_matrix_fa_features.jpeg')
corrplot(combined_correaltion_residuals[FA_features_two_mixture_model,FA_features_two_mixture_model], order = "alphabet",
         tl.cex = 0.8,tl.col="black")
dev.off()