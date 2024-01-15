library(corrplot) 
library(tidyverse) 
library(tidyr) 


# Q2-Q4 correlation plots -------------------------------------------------

two_mixture_model_features_residuals <- biobank_feature_residual_analysis[q2_q4_features_names$feature]

two_mixture_model_men_data_residuals <- lapply(two_mixture_model_features_residuals, function(x){x$hypothesis_results$mixture_model$men_responsebilities})

two_mixture_model_women_data_residuals <- lapply(two_mixture_model_features_residuals, function(x){x$hypothesis_results$mixture_model$women_responsebilities})


two_mixture_model_men_feature_data_residuals <- two_mixture_model_men_data_residuals[[1]]

for(i in 2:length(two_mixture_model_men_data_residuals))
{
  two_mixture_model_men_feature_data_residuals <- base::merge(two_mixture_model_men_feature_data_residuals, two_mixture_model_men_data_residuals[[i]], by.x = "eid", by.y = "eid")
}
names(two_mixture_model_men_feature_data_residuals) <- c("eid", q2_q4_features_names$feature)
two_mixture_model_men_features_no_id_residuals <- two_mixture_model_men_feature_data_residuals[,-1]

two_mixture_model_women_feature_data_residuals <- two_mixture_model_women_data_residuals[[1]]
for(i in 2:length(two_mixture_model_women_data_residuals))
{
  two_mixture_model_women_feature_data_residuals <- base::merge(two_mixture_model_women_feature_data_residuals, two_mixture_model_women_data_residuals[[i]], by.x = "eid", by.y = "eid")
}
names(two_mixture_model_women_feature_data_residuals) <- c("eid", q2_q4_features_names$feature)
two_mixture_model_women_features_no_id_residuals <- two_mixture_model_women_feature_data_residuals[,-1]

volume_features_mosaic_features <- grep('Volume',q2_q4_features_names$feature)

weighted_mean_MD_features_mosaic_features <- grep('Weighted-mean MD',q2_q4_features_names$feature)
MD_features_mosaic_features <- grep('Mean MD',q2_q4_features_names$feature)[!grep('MD',q2_q4_features_names$feature) %in%  weighted_mean_MD_features_mosaic_features]

weighted_mean_FA_features_mosaic_features <- grep('Weighted-mean FA',q2_q4_features_names$feature)
FA_features_mosaic_features <- grep('Mean FA',q2_q4_features_names$feature)[!grep('FA',q2_q4_features_names$feature) %in% weighted_mean_FA_features_mosaic_features]


men_cor_residuals <- cor(two_mixture_model_men_features_no_id_residuals, use = "pairwise.complete.obs")
women_cor_residuals <- cor(two_mixture_model_women_features_no_id_residuals,use = "pairwise.complete.obs")
combined_correaltion_residuals <- men_cor_residuals
combined_correaltion_residuals[lower.tri(combined_correaltion_residuals)] <- women_cor_residuals[lower.tri(women_cor_residuals)]



two_mixture_model_corrplot_legend <- data.frame(feature_name = colnames(combined_correaltion_residuals), 
                                                number = 1:ncol(combined_correaltion_residuals))
colnames(combined_correaltion_residuals) <- rownames(combined_correaltion_residuals) <- 1:ncol(combined_correaltion_residuals)

corrplot(combined_correaltion_residuals[volume_features_mosaic_features,volume_features_mosaic_features], order = "alphabet",tl.cex = 0.4)

corrplot(combined_correaltion_residuals[MD_features_mosaic_features,MD_features_mosaic_features], order = "alphabet",
         tl.cex = 1.0,tl.srt = 45)

corrplot(combined_correaltion_residuals[weighted_mean_MD_features_mosaic_features,weighted_mean_MD_features_mosaic_features], order = "alphabet",
         tl.cex = 1.0,tl.srt = 45)

corrplot(combined_correaltion_residuals[FA_features_mosaic_features,FA_features_mosaic_features], order = "alphabet",
         tl.cex = 0.8)

corrplot(combined_correaltion_residuals[weighted_mean_FA_features_mosaic_features,weighted_mean_FA_features_mosaic_features], order = "alphabet",
         tl.cex = 0.8)

# Sorted correlation plot -------------------------------------------------

no_pure_types_for_correlation_analysis <- 
  biobank_feature_residual_analysis[!(names(biobank_feature_residual_analysis) %in% rownames(pure_types_residual_model_table))]

men_responsebilities <- lapply(no_pure_types_for_correlation_analysis, function(x){x$hypothesis_results$mixture_model$men_responsebilities})

women_responsebilities <- lapply(no_pure_types_for_correlation_analysis, function(x){x$hypothesis_results$mixture_model$women_responsebilities})

man_correlation_matrix <- men_responsebilities %>% bind_rows(.id = "feature") %>% 
  pivot_wider(id_cols = "eid", names_from = "feature", values_from = "responsebility") %>% 
  select(-eid) %>% 
  cor(use = "complete.obs")
  

woman_correlation_matrix <- women_responsebilities %>% bind_rows(.id = "feature") %>% 
  pivot_wider(id_cols = "eid", names_from = "feature", values_from = "responsebility") %>% 
  select(-eid) %>% 
  cor(use = "complete.obs")

data.frame(index = seq(0,length(man_correlation_matrix[upper.tri(man_correlation_matrix)])-1),
           men = sort(man_correlation_matrix[upper.tri(man_correlation_matrix)]),
           women = sort(woman_correlation_matrix[upper.tri(woman_correlation_matrix)])) %>% 
  pivot_longer(!index, names_to = "gender", values_to = "correlation_coefficient") %>% 
  ggplot(aes(x = index, y = correlation_coefficient, col = gender)) + 
  geom_line(size=2) +
  scale_color_manual(values=c("navyblue", "tomato2")) + 
  theme_bw() + 
  ylab("Correlation coefficient")+
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10),
        panel.border = element_blank())
