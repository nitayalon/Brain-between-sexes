library(corrplot)
library(corrr)
library(pwr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(Jmisc)
library(pbapply)
library(config)
library(DescTools)
library(effsize)
library(mixtools)
library(caret)
library(glmnet)
library(rstatix)
library(AdaptGauss)


if(Sys.info()['sysname'] == "Windows"){
  
  trace(grDevices::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
  
  
  # Enable anti-aliasing on Windows
  trace(grDevices:::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
  
  
}

# Define general plot style and style
base_size = 20
theme_set(theme_classic(base_size = base_size))

## Loading the data

load("~/Dapha_joel/Human_brain_research/new_model_with_age/BioBank_data_full_analysis_no_age_30_09_2022.RData")
# load("~/Dapha_joel/Human_brain_research/new_model_with_age/BioBank_data_full_analysis_with_age_13_04_2022.RData")
data_for_analysis <- biobank_without_age_

# Converting from EM space to p-q space
original_p_q <- sapply(data_for_analysis, function(x){x$hypothesis_results$mixture_model$m_parameters})
feature_residual_m_parameters <- convertEMProbabiltiesToPQSpace(data_for_analysis)
residuals_p_and_q_df <- sapply(feature_residual_m_parameters, function(x){c(x$p, x$q)}) %>% as.data.frame()

# Plot Cohen's D and T-test results
cohens_d_values <- 
  sapply(data_for_analysis, function(x){x$hypothesis_results$cohen_d_test$estimate}) %>% 
  data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(Cohens_D = ".")

summary(abs(cohens_d_values$Cohens_D))

cohens_d_plot <- sapply(data_for_analysis, function(x){x$hypothesis_results$cohen_d_test$estimate}) %>% 
  data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(Cohens_D = ".") %>%
  ggplot(aes(x = Cohens_D)) + 
  geom_histogram(aes(y=..density..),colour="black", fill="grey69",
                 bins=50,
                 breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) + 
  theme_classic() + 
  scale_x_continuous(breaks = round(seq(-0.5, 0.5, by = 0.1),1)) + 
  ylab("Frequency") + xlab("Cohen's D") + 
  ggtitle("Histogram of Cohen's-D values")+
  theme(text = element_text(face="bold", color="black", 
                            size=20),
        panel.border = element_blank()) 

ggsave("Cohens_d.png", cohens_d_plot)

## Student t-test analysis
t_test_p_values <- sapply(data_for_analysis, function(x){x$hypothesis_results$t_test_for_difference_between_genders$p.value}) %>% 
  data.frame() %>% 
  rename(p_value = ".")
t_test_adjusted_p_values <- p.adjust(t_test_p_values$p_value,method = "BH") 
table(t_test_adjusted_p_values < 0.05)  

student_t_plot <- sapply(data_for_analysis, function(x){x$hypothesis_results$t_test_for_difference_between_genders$statistic}) %>% 
  data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(T_statistics = ".") %>% 
  ggplot(aes(x = T_statistics)) + 
  geom_histogram(bins = 10 ,color="black", fill="grey",
                 breaks=c(-30, -20, -15, -10, -5, 0, 
                          5, 10, 15, 20, 25, 30)) + 
  theme_bw() + 
  ylab("Density") + xlab("T Statistic") + 
  ggtitle("Histogram of T-statistic values")+
  theme(text = element_text(size = 18),
        panel.border = element_blank())

ggsave("student_t_plot.png", student_t_plot)

# Searching for pure-type features
pvalue_per_feature_df_residuals <- pullPvaluePerHypothesis(data_for_analysis) %>% t() %>% as.data.frame() 

cohens_d_per_feature_residuals <- sapply(data_for_analysis, function(x){x$hypothesis_results$cohen_d_test$estimate})

t_test_p_value_per_feature_residuals <- sapply(data_for_analysis, function(x){x$hypothesis_results$t_test_for_difference_between_genders$p.value}) %>%
  as.data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(p_value = ".")
  
t_test_p_value_per_feature_residuals$adjusted_pv <- p.adjust(t_test_p_value_per_feature_residuals$p_value,method = "BH") 

combined_df_resid <- mergeDataFramesByColumnNames(pvalue_per_feature_df_residuals, 
                                                  residuals_p_and_q_df)

combined_df_resid <- rbind(combined_df_resid, cohens_d_per_feature_residuals)
rownames(combined_df_resid) <- c("p_values","p","q","Cohen_D")

combined_df_long_resid <- combined_df_resid %>% t() %>% as.data.frame()
combined_df_long_resid$bins_of_pv <- cut(combined_df_long_resid$p_values, c(1e-9,1e-8,1e-3,1e-2,1))
combined_df_long_resid$bh_pvalue <- p.adjust(combined_df_long_resid$p_values, method = "BH")

# How many pure types are there?
table(combined_df_long_resid$bh_pvalue < 0.01)
table(combined_df_long_resid$bh_pvalue < 0.05)
table(combined_df_long_resid$bh_pvalue < 0.1)

# Export and remove pure types from analysis
pure_types_residual_model_table <- 
  combined_df_long_resid[combined_df_long_resid$bh_pvalue > 0.05,] %>% 
  rownames_to_column(var = "feature") %>% 
  inner_join(t_test_p_value_per_feature_residuals)
  
single_distribution_model <- pure_types_residual_model_table$feature[3]

plotGenderHistogram(data_for_analysis[[single_distribution_model]]$feature_residuals,
                    data_for_analysis[[single_distribution_model]],
                    single_distribution_model,
                    two_mixtures=F,
                    single_gaussian=T)

write.csv(pure_types_residual_model_table, file = "pure_types_features_residual_data.csv", row.names = T)

# Removing pure type features from the last -------------------------------

biobank_feature_residual_analysis_without_pure_type <- 
  data_for_analysis[!(names(data_for_analysis) %in% pure_types_residual_model_table$feature)]

# Testing for p=q
equal_mixture_probabilities_p_value <- equalProbabilityMixturePV(biobank_feature_residual_analysis_without_pure_type)
pvalue_mixture_vs_equal_prob_pv <- equal_mixture_probabilities_p_value %>% data.frame() 
names(pvalue_mixture_vs_equal_prob_pv) <- "p_value"

residuals_p_and_q_df_equal_probs <- residuals_p_and_q_df[!(names(residuals_p_and_q_df) %in% pure_types_residual_model_table$feature)] %>% t()

names(residuals_p_and_q_df_equal_probs) <- c("p","q")

combined_df_resid_equal_probs <- base::merge(pvalue_mixture_vs_equal_prob_pv, residuals_p_and_q_df_equal_probs,by = 0, all = T)

names(combined_df_resid_equal_probs) <- c("feature","p_value", "p", "q")

cohens_d_per_feature_residuals_equal_probs <- cohens_d_per_feature_residuals[!(names(cohens_d_per_feature_residuals) %in% row.names(pure_types_residual_model_table))] %>% data.frame()

names(cohens_d_per_feature_residuals_equal_probs) <- "Cohen_D"
cohens_d_per_feature_residuals_equal_probs$feature = row.names(cohens_d_per_feature_residuals_equal_probs)

combined_df_resid_equal_probs <- inner_join(combined_df_resid_equal_probs, cohens_d_per_feature_residuals_equal_probs)
combined_df_resid_equal_probs$bh_pvalue <- p.adjust(combined_df_resid_equal_probs$p_value, method="BH")
combined_df_resid_equal_probs$p_equal_q <- combined_df_resid_equal_probs$bh_pvalue >= 0.05

combined_df_resid_equal_probs$Cohen_H = ES.h(combined_df_resid_equal_probs$p,
                                             combined_df_resid_equal_probs$q)

table(combined_df_resid_equal_probs$bh_pvalue < 0.01)
table(combined_df_resid_equal_probs$bh_pvalue < 0.05)
table(combined_df_resid_equal_probs$bh_pvalue < 0.1)


### Cohens' D analysis:
summary(combined_df_resid_equal_probs$Cohen_D)
cut(abs(combined_df_resid_equal_probs$Cohen_D), 
    breaks = c(0,0.2,0.5,0.44)) %>% 
  table()

p_vs_q_with_informaion_plot <- combined_df_resid_equal_probs %>% 
  mutate(Cohen_D_bins = cut(Cohen_D, c(-0.6,-0.5,-0.3,-0.2,-0.1,0.1,0.2,0.3,0.4,0.5))) %>% 
  ggplot(aes(p,q, colour = Cohen_D_bins)) +
  geom_point(aes(shape = p_equal_q)) +
  scale_shape_manual(values=c(3, 19), guide = "none")+
  geom_vline(xintercept = 0.5, size=1) +
  geom_hline(yintercept = 0.5, size=1) +
  #ggtitle("Values of p and q", subtitle = "shape is p=q hypothesis") + 
  geom_abline(slope = 1, intercept = 0, size=1) + 
  scale_color_manual(name = "Cohen's d",
                     values = c("(-0.6,-0.5]" = "red4",
                                "(-0.5,-0.3]" = "red3",
                                "(-0.3,-0.2]" = "red2",
                                "(-0.2,-0.1]" = "red1",
                                "(-0.1,0.1]" = "orange1",
                                "(0.1,0.2]" = "deepskyblue2",
                                "(0.2,0.3]" = "dodgerblue1",
                                "(0.3,0.4]" = "blue2",
                                "(0.4,0.5]" = "blue4"),
                     labels = c("-0.6<=d<-0.5","-0.5<=d<-0.3",
                                "-0.3<=d<-0.2","0.2<=d<-0.1","-0.1<=d<0.1","0.1<=d<0.2","0.2<=d<3","0.3<=d<0.4",
                                "0.4<=d<0.5")) +
  theme_bw() + 
  theme(text = element_text(face="bold", color="black", 
                            size=20),
        panel.border = element_blank()) +
  coord_fixed()

ggsave("p_vs_q_with_informaion_plot.png", p_vs_q_with_informaion_plot)

## Cohen's H analysis

combined_df_resid_equal_probs %>% 
  mutate(Cohen_H = abs(Cohen_H)) %>% 
  mutate(Cohen_H_bins = cut(Cohen_H, breaks=c(0,0.2,0.5,0.8,1.35, 1.4))) %>% 
  group_by(Cohen_H_bins) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)
             
combined_df_resid_equal_probs %>% 
  mutate(Cohen_H = abs(Cohen_H)) %>% 
  arrange(desc(Cohen_H)) %>% 
  head()

## Plotting the histogram of the feature with the highest Cohen's h:
feature_with_highest_cohens_h <- "Volume of grey matter in Planum Polare (right)"

combined_df_resid_equal_probs %>% 
  filter(feature == feature_with_highest_cohens_h)

largest_cohens_h_feature <- 
  plotGenderHistogram(data_for_analysis[[feature_with_highest_cohens_h]]$feature_residuals,
                    data_for_analysis[[feature_with_highest_cohens_h]],
                    feature_with_highest_cohens_h)
  
ggsave("largest_cohens_h_feature.png", largest_cohens_h_feature)

## Cohen's d - vs Cohen's h (abs)

# Remove the grid and highlight axis
cohens_h_vs_cohens_d_with_correlation <- 
  combined_df_resid_equal_probs %>% 
  ggplot(aes(abs(Cohen_D), abs(Cohen_H))) + 
  geom_point(size=2.5) + 
  ylab("Cohen's-h") + 
  xlab("Cohen's-d") + 
  theme(text = element_text(face="bold", color="black", 
                                   size=25),
        panel.border = element_blank()) 
ggsave("cohens_h_vs_cohens_d_with_correlation.png", cohens_h_vs_cohens_d_with_correlation)

combined_df_resid_equal_probs %>% 
  select(Cohen_H, Cohen_D) %>% 
  correlate()

## Compute Cohen's-h
combined_df_resid_equal_probs %>% 
  filter(!p_equal_q) %>% 
  mutate(Cohen_H_bins = cut(abs(Cohen_H), breaks=c(0,0.2,0.5,0.8,1.4))) %>% 
  group_by(Cohen_H_bins) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)


## likelihood computation
# p!= q features
combined_df_resid_equal_probs %>% 
  filter(!p_equal_q) %>% 
  filter(p < 0.5 & q > 0.5 | p > 0.5 & q < 0.5) %>% 
  mutate(men_men = p*p + (1-p)*(1-p),
         women_women = q*q + (1-q)*(1-q),
         men_women = p*q + (1-p)*(1-q)) %>% 
  select(men_men, women_women, men_women) %>% 
  mutate(men_women_to_men_men = men_women / men_men,
         men_women_to_women_women = men_women / women_women) %>% 
  summarise(avg_men_women_to_men_men = mean(men_women_to_men_men),
            min_men_women_to_men_men = min(men_women_to_men_men),
            max_men_women_to_men_men = max(men_women_to_men_men),
            avg_men_women_to_women_women = mean(men_women_to_women_women),
            min_men_women_to_women_women = min(men_women_to_women_women),
            max_men_women_to_women_women = max(men_women_to_women_women)) 

# p == q features
combined_df_resid_equal_probs %>% 
  filter(p_equal_q) %>% 
  mutate(men_men = p*p + (1-p)*(1-p),
         women_women = q*q + (1-q)*(1-q),
         men_women = p*q + (1-p)*(1-q)) %>% 
  select(men_men, women_women, men_women) %>% 
  mutate(men_women_to_men_men = men_women / men_men,
         men_women_to_women_women = men_women / women_women) %>% 
  summarise(avg_men_women_to_men_men = mean(men_women_to_men_men),
            min_men_women_to_men_men = min(men_women_to_men_men),
            max_men_women_to_men_men = max(men_women_to_men_men),
            avg_men_women_to_women_women = mean(men_women_to_women_women),
            min_men_women_to_women_women = min(men_women_to_women_women),
            max_men_women_to_women_women = max(men_women_to_women_women))

## Export Mosaic features
FDR_mosaic_features <- 
  combined_df_resid_equal_probs %>% 
  filter(!p_equal_q) %>% 
  filter(p < 0.5 & q > 0.5 | p > 0.5 & q < 0.5) %>% 
  select(feature)

q2_q4_features_for_export <- combined_df_resid_equal_probs %>% 
  filter(!p_equal_q) %>% 
  filter(p < 0.5 & q > 0.5 | p > 0.5 & q < 0.5)

write.csv(q2_q4_features_for_export, file = "q2_q4_features_data.csv", row.names = T)

## Tail distribution analysis
combined_df_resid_equal_probs %>% 
  filter(p < 0.25 & q < 0.25 | p > 0.75 & q > 0.75) %>% 
  summarise(equal = sum(p_equal_q),
            not_equal = sum(!p_equal_q))

combined_df_resid_equal_probs %>% 
  filter(!p_equal_q) %>% 
  filter(p < 0.25 & q < 0.25 | p > 0.75 & q > 0.75) %>% 
  summarise(p_bigger = sum(p - q > 0.01) / n() * 100,
            q_bigger = sum(p - q < -0.01) / n() * 100,
            equal = sum(abs(q - p) < 0.01) / n() * 100)

## find a feature with large tail diff
combined_df_resid_equal_probs %>% 
  filter(!p_equal_q) %>% 
  filter(p < 0.25 & q < 0.25 | p > 0.75 & q > 0.75) %>% 
  mutate(diff = p-q) %>% 
  arrange(desc(diff))


### Table 2
combined_df_resid_equal_probs %>% 
  filter(feature %in% FDR_mosaic_features$feature) %>% 
write.csv(., file = "Q2_Q4_features_with_p_q_cohens_d_and_cohens_h.csv", row.names = T)  


# Figure 2 ----------------------------------------------------------------

## 2A

figure_2a_feature <- "Volume of grey matter in Postcentral Gyrus (left)"
figure_2a <- plotGenderHistogram(data_for_analysis[[figure_2a_feature]]$feature_residuals,
                    data_for_analysis[[figure_2a_feature]],
                    figure_2a_feature, two_mixtures = F, single_gaussian=T)
ggsave("figure_2a.png", figure_2a)

## 2B
combined_df_resid_equal_probs %>% 
  filter(p_equal_q)

figure_2b_feature <- "Mean FA in middle cerebellar peduncle on FA skeleton"
figure_2b <- plotGenderHistogram(data_for_analysis[[figure_2b_feature]]$feature_residuals,
                    data_for_analysis[[figure_2b_feature]],
                    figure_2b_feature)
ggsave("figure_2b.png", figure_2b)

## 2c
combined_df_resid_equal_probs %>% 
  filter(!p_equal_q, p < 0.5 & q < 0.5 | p > 0.5 & q > 0.5) %>% 
  mutate(diff = p-q) %>% 
  arrange(desc(diff)) %>% head()

figure_2c_feature <- "Volume of grey matter in Lingual Gyrus (right)"
# figure_2c_feature <- "Mean MD in fornix cres+stria terminalis on FA skeleton (right)"
figure_2c <- plotGenderHistogram(data_for_analysis[[figure_2c_feature]]$feature_residuals,
                    data_for_analysis[[figure_2c_feature]],
                    figure_2c_feature)
ggsave("figure_2c.png", figure_2c)

## 2d
combined_df_resid_equal_probs %>% 
  mutate(Cohen_H = abs(Cohen_H)) %>% 
  arrange(desc(Cohen_H)) %>% 
  head(5)

combined_df_resid_equal_probs %>% 
  mutate(Cohen_H = abs(Cohen_H)) %>% 
  arrange(desc(Cohen_H)) %>% 
  head(10)

figure_2d_feature <- "Volume of grey matter in Lingual Gyrus (left)"
figure_2d <- plotGenderHistogram(data_for_analysis[[figure_2d_feature]]$feature_residuals,
                    data_for_analysis[[figure_2d_feature]],
                    figure_2d_feature)
ggsave("figure_2d.png", figure_2d)

largest_cohens_h_feature <- "Volume of grey matter in Planum Polare (right)"
largest_cohens_h_feature_plot <- plotGenderHistogram(data_for_analysis[[largest_cohens_h_feature]]$feature_residuals,
                    data_for_analysis[[largest_cohens_h_feature]],
                    largest_cohens_h_feature)
ggsave("largest_cohens_h_feature_plot.png", largest_cohens_h_feature_plot)


### Theoretical plots



# Correlation analysis ----------------------------------------------------

mosaic_features <- data_for_analysis[FDR_mosaic_features$feature]

##### Male-favoured - Figure 4a + 5A

men_data <- lapply(mosaic_features, function(x){x$hypothesis_results$mixture_model$men_responsebilities})
women_data <- lapply(mosaic_features, function(x){x$hypothesis_results$mixture_model$women_responsebilities})

men_feature_data <- men_data %>% reduce(inner_join, by = c("eid"))
names(men_feature_data) <- c("eid", FDR_mosaic_features$feature)
men_features_no_id <- men_feature_data[,-1]

women_feature_data <- women_data %>% reduce(inner_join, by = c("eid"))
names(women_feature_data) <- c("eid", FDR_mosaic_features$feature)
women_features_no_id <- women_feature_data[,-1]


# men_feature_data <- men_data[[1]]
# for(i in 2:length(men_data))
# {
#   men_feature_data <- base::merge(men_feature_data, men_data[[i]], by.x = "eid", by.y = "eid")
# }


# women_feature_data <- women_data[[1]]
# for(i in 2:length(women_data))
# {
#   women_feature_data <- base::merge(women_feature_data, women_data[[i]], by.x = "eid", by.y = "eid")
# }
# names(women_feature_data) <- c("eid", FDR_mosaic_features)

men_cor <- cor(men_features_no_id, use = "pairwise.complete.obs")
women_cor <- cor(women_features_no_id,use = "pairwise.complete.obs")
combined_correaltion <- men_cor
combined_correaltion[lower.tri(combined_correaltion)] <- women_cor[lower.tri(women_cor)]

volume_features_mosaic_features <- grep('Volume',FDR_mosaic_features$feature)
MD_features_mosaic_features <- grep('Mean MD',FDR_mosaic_features$feature)
weighted_mean_MD_features_mosaic_features <- grep('Weighted-mean MD',FDR_mosaic_features$feature)
weighted_mean_FA_features_mosaic_features <- grep('Weighted-mean FA',FDR_mosaic_features$feature)
FA_features_mosaic_features <- grep('Mean FA',FDR_mosaic_features$feature)[!grep('Mean FA',FDR_mosaic_features$feature) %in% weighted_mean_FA_features_mosaic_features]
colnames(combined_correaltion) <- rownames(combined_correaltion) <- 1:ncol(combined_correaltion)


method = "circle"

png(height=2200, width=2200,
    sprintf('q2_q4_volume_features_correlation_plot_%s.png',method), type = "cairo")
corrplot(combined_correaltion[volume_features_mosaic_features,volume_features_mosaic_features], 
         order="original", method=method,
         tl.cex = 3.5, tl.srt = 45, cl.pos="n")
dev.off()

# png(height=1800, width=1800,
#     sprintf('q2_q4_md_features_correlation_plot.png_%s.png',method), type = "cairo")
# corrplot(combined_correaltion[MD_features_mosaic_features,MD_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 4.0,tl.srt = 45, cl.pos="n")
# dev.off()

# png(height=1800, width=1800,
#     sprintf('q2_q4_fa_features_correlation_plot.png_%s.png',method), type = "cairo")
# corrplot(combined_correaltion[FA_features_mosaic_features,FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 6.0,tl.srt = 45, cl.pos="n")
# dev.off()

# png(height=1800, width=1800,'q2_q4_weighted_mean_fa_features_correlation_plot.png', type = "cairo")
# corrplot(combined_correaltion[weighted_mean_FA_features_mosaic_features,weighted_mean_FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0,tl.srt = 45)
# dev.off()

method = "number"

png(height=2200, width=2200,
    sprintf('q2_q4_volume_features_correlation_plot_%s.png',method), type = "cairo")
corrplot(combined_correaltion[volume_features_mosaic_features,volume_features_mosaic_features], 
         order="original", method=method,
         tl.cex = 2.5, tl.srt = 45, number.cex = 2.0, col=c("Black"), cl.pos="n")
dev.off()

# png(height=1800, width=1800,
#     sprintf('q2_q4_md_features_correlation_plot.png_%s.png',method), type = "cairo")
# corrplot(combined_correaltion[MD_features_mosaic_features,MD_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0, number.cex = 1.25, col=c("Black"), cl.pos="n")
# dev.off()
# 
# png(height=1800, width=1800,
#     sprintf('q2_q4_fa_features_correlation_plot.png_%s.png',method), type = "cairo")
# corrplot(combined_correaltion[FA_features_mosaic_features,FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0, number.cex = 1.25, col=c("Black"), cl.pos="n")
# dev.off()

# png(height=1800, width=1800,
#     sprintf('q2_q4_fa_features_correlation_plot.png_%s.png',method), type = "cairo")
# corrplot(combined_correaltion[weighted_mean_FA_features_mosaic_features,weighted_mean_FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0, number.cex = 1.25, col=c("Black"))
# dev.off()

##### High-mean - Figure 4b + 5b

responsebilities_updated_to_high_mean <- lapply(mosaic_features, function(x){adjustResponsebilitiesToHighMean(x)})

men_data <- lapply(responsebilities_updated_to_high_mean, function(x){x$men_responsebilities})
women_data <- lapply(responsebilities_updated_to_high_mean, function(x){x$women_responsebilities})


men_feature_data <- men_data %>% reduce(inner_join, by = c("eid"))
names(men_feature_data) <- c("eid", FDR_mosaic_features$feature)
men_features_no_id <- men_feature_data[,-1]

women_feature_data <- women_data %>% reduce(inner_join, by = c("eid"))
names(women_feature_data) <- c("eid", FDR_mosaic_features$feature)
women_features_no_id <- women_feature_data[,-1]


men_cor <- cor(men_features_no_id, use = "pairwise.complete.obs")
women_cor <- cor(women_features_no_id,use = "pairwise.complete.obs")
combined_correaltion <- men_cor
combined_correaltion[lower.tri(combined_correaltion)] <- women_cor[lower.tri(women_cor)]

volume_features_mosaic_features <- grep('Volume',FDR_mosaic_features$feature)
MD_features_mosaic_features <- grep('Mean MD',FDR_mosaic_features$feature)
weighted_mean_MD_features_mosaic_features <- grep('Weighted-mean MD',FDR_mosaic_features$feature)
weighted_mean_FA_features_mosaic_features <- grep('Weighted-mean FA',FDR_mosaic_features$feature)
FA_features_mosaic_features <- grep('Mean FA',FDR_mosaic_features$feature)[!grep('Mean FA',FDR_mosaic_features$feature) %in% weighted_mean_FA_features_mosaic_features]
colnames(combined_correaltion) <- rownames(combined_correaltion) <- 1:ncol(combined_correaltion)

method = "circle"

png(height=1800, width=1800,'q2_q4_volume_features_correlation_plot_high_mean.png', type = "cairo")
corrplot(combined_correaltion[volume_features_mosaic_features,volume_features_mosaic_features], 
         order="alphabet", method=method,
         tl.cex = 2.0,tl.srt = 45, cl.pos="n")
dev.off()

# png(height=1800, width=1800,'q2_q4_md_features_correlation_plot_high_mean.png', type = "cairo")
# corrplot(combined_correaltion[MD_features_mosaic_features,MD_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0,tl.srt = 45, cl.pos="n")
# dev.off()
# 
# png(height=1800, width=1800,'q2_q4_fa_features_correlation_plot_high_mean.png', type = "cairo")
# corrplot(combined_correaltion[FA_features_mosaic_features,FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0,tl.srt = 45, cl.pos="n")
# dev.off()

# png(height=1800, width=1800,'q2_q4_weighted_mean_fa_features_correlation_plot_high_mean.png', type = "cairo")
# corrplot(combined_correaltion[weighted_mean_FA_features_mosaic_features,weighted_mean_FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0,tl.srt = 45, cl.pos="n")
# dev.off()

method = "number"

png(height=1800, width=1800,'q2_q4_volume_features_correlation_plot_high_mean_number.png', type = "cairo")
corrplot(combined_correaltion[volume_features_mosaic_features,volume_features_mosaic_features], 
         order="alphabet", method=method,
         tl.cex = 2.0, number.cex = 1.25, col=c("Black"), cl.pos="n")
dev.off()

# png(height=1800, width=1800,'q2_q4_md_features_correlation_plot_high_mean_number.png', type = "cairo")
# corrplot(combined_correaltion[MD_features_mosaic_features,MD_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0, number.cex = 1.25, col=c("Black"), cl.pos="n")
# dev.off()
# 
# png(height=1800, width=1800,'q2_q4_fa_features_correlation_plot_high_mean_number.png', type = "cairo")
# corrplot(combined_correaltion[FA_features_mosaic_features,FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0, number.cex = 1.25, col=c("Black"), cl.pos="n")
# dev.off()

# png(height=1800, width=1800,'q2_q4_weighted_mean_fa_features_correlation_plot_high_mean_number.png', type = "cairo")
# corrplot(combined_correaltion[weighted_mean_FA_features_mosaic_features,weighted_mean_FA_features_mosaic_features], 
#          order="alphabet", method=method,
#          tl.cex = 2.0, number.cex = 1.25, col=c("Black"), cl.pos="n")
# dev.off()


# Sorted correlation plot -------------------------------------------------

full_men_data <- 
  lapply(data_for_analysis[!(names(data_for_analysis) %in% pure_types_residual_model_table$feature)], function(x){x$hypothesis_results$mixture_model$men_responsebilities}) %>% 
  reduce(inner_join, by="eid")
names(full_men_data) = c('eid', names(data_for_analysis)[!names(data_for_analysis) %in% pure_types_residual_model_table$feature])

full_women_data <- lapply(data_for_analysis[!(names(data_for_analysis) %in% pure_types_residual_model_table$feature)], function(x){x$hypothesis_results$mixture_model$women_responsebilities}) %>% 
  reduce(inner_join, by="eid")
names(full_women_data) = c('eid', names(data_for_analysis)[!names(data_for_analysis) %in% pure_types_residual_model_table$feature])

men_data_for_sorted_correlation_plot = rbind(
    full_men_data %>% 
  select(starts_with("Mean FA")) %>% 
  correlate() %>% 
  shave() %>% 
  stretch() %>% 
  drop_na(),
  full_men_data %>% 
    select(starts_with("Volume")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Mean MD")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Weighted-mean MD")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Weighted-mean FA")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na()
  )

women_data_for_sorted_correlation_plot = rbind(full_women_data %>% 
  select(starts_with("Mean FA")) %>% 
  correlate() %>% 
  shave() %>% 
  stretch() %>% 
  drop_na(),
  full_women_data %>% 
  select(starts_with("Volume")) %>% 
  correlate() %>% 
  shave() %>% 
  stretch() %>% 
  drop_na(),
  full_women_data %>% 
  select(starts_with("Mean MD")) %>% 
  correlate() %>% 
  shave() %>% 
  stretch() %>% 
  drop_na(),
  full_women_data %>% 
  select(starts_with("Weighted-mean MD")) %>% 
  correlate() %>% 
  shave() %>% 
  stretch() %>% 
  drop_na(),
  full_women_data %>% 
  select(starts_with("Weighted-mean FA")) %>% 
  correlate() %>% 
  shave() %>% 
  stretch() %>% 
  drop_na()
)

sorted_correlation_data <- 
  rbind(
  women_data_for_sorted_correlation_plot %>% 
    select(r) %>% 
    arrange(r) %>% 
    mutate(sex='Women',
           index = 1:nrow(women_data_for_sorted_correlation_plot)),
  men_data_for_sorted_correlation_plot %>% 
    select(r) %>% 
    arrange(r) %>% 
    mutate(sex='Men',
           index = 1:nrow(women_data_for_sorted_correlation_plot))
)

sorted_correlation_data %>% 
ggplot(aes(x=index, y=r, colour=sex)) + 
  geom_line(size = 1.5) + 
  ylab("Correlation Coefficient") + 
  xlab("Index") + 
  scale_colour_manual(values = c('tomato','dodgerblue')) + 
  theme(text = element_text(face="bold", color="black", 
                            size=25),
        panel.border = element_blank()) +
  theme(legend.position="none", aspect.ratio = 1)

# png(height=1800, width=1800, 'sorted_correlation_of_all_features.png', type = "cairo")  
# plot(1:nrow(women_data_for_sorted_correlation_plot),
#      men_data_for_sorted_correlation_plot$r[order(men_data_for_sorted_correlation_plot$r)], type = 'l', col = 'navyblue',
#      cex.axis=1.25, cex.lab=1.4,
#      xlab = "Index", ylab = 'Correlation coefficient', lwd = 2, yaxt = "n")
# axis(2, at = seq(-0.4, 0.8, 0.2), cex.axis=1.25)
# lines(women_data_for_sorted_correlation_plot$r[order(women_data_for_sorted_correlation_plot$r)], type = 'l', col = 'tomato', lwd = 2)
# dev.off()


# Sorted correlation plot - non normalized for volume data ----------------

sapply(biobank_without_age_without_normalization, function(x){sum(x$feature_residuals$outlier_flag)}) %>% 
  summary()

# Converting from EM space to p-q space
non_corrected_original_p_q <- sapply(biobank_without_age_without_normalization, function(x){x$hypothesis_results$mixture_model$m_parameters})
feature_residual_m_parameters <- convertEMProbabiltiesToPQSpace(biobank_without_age_without_normalization)

residuals_p_and_q_df <- sapply(feature_residual_m_parameters, function(x){c(x$p, x$q)}) %>% as.data.frame()
pvalue_per_feature_df_residuals <- pullPvaluePerHypothesis(biobank_without_age_without_normalization) %>% t() %>% as.data.frame() 

cohens_d_per_feature_residuals <- sapply(biobank_without_age_without_normalization, function(x){x$hypothesis_results$cohen_d_test$estimate})

t_test_p_value_per_feature_residuals <- sapply(biobank_without_age_without_normalization, function(x){x$hypothesis_results$t_test_for_difference_between_genders$p.value}) %>%
  as.data.frame() %>% 
  rownames_to_column(var = "feature") %>% 
  rename(p_value = ".")

t_test_p_value_per_feature_residuals$adjusted_pv <- p.adjust(t_test_p_value_per_feature_residuals$p_value,method = "BH") 

combined_df_resid <- mergeDataFramesByColumnNames(pvalue_per_feature_df_residuals, 
                                                  residuals_p_and_q_df)

combined_df_resid <- rbind(combined_df_resid, cohens_d_per_feature_residuals)
rownames(combined_df_resid) <- c("p_values","p","q","Cohen_D")

combined_df_long_resid <- combined_df_resid %>% t() %>% as.data.frame()
combined_df_long_resid$bins_of_pv <- cut(combined_df_long_resid$p_values, c(1e-9,1e-8,1e-3,1e-2,1))
combined_df_long_resid$bh_pvalue <- p.adjust(combined_df_long_resid$p_values, method = "BH")

# How many pure types are there?
table(combined_df_long_resid$bh_pvalue < 0.01)
table(combined_df_long_resid$bh_pvalue < 0.05)
table(combined_df_long_resid$bh_pvalue < 0.1)

# Export and remove pure types from analysis
pure_types_residual_model_table <- 
  combined_df_long_resid[combined_df_long_resid$bh_pvalue > 0.05,] %>% 
  rownames_to_column(var = "feature") %>% 
  inner_join(t_test_p_value_per_feature_residuals)


full_men_data <- 
  lapply(biobank_without_age_without_normalization[!(names(biobank_without_age_without_normalization) %in% pure_types_residual_model_table$feature)], function(x){x$hypothesis_results$mixture_model$men_responsebilities}) %>% 
  reduce(inner_join, by="eid")
names(full_men_data) = c('eid', names(biobank_without_age_without_normalization)[!names(biobank_without_age_without_normalization) %in% pure_types_residual_model_table$feature])

full_women_data <- lapply(biobank_without_age_without_normalization[!(names(biobank_without_age_without_normalization) %in% pure_types_residual_model_table$feature)], function(x){x$hypothesis_results$mixture_model$women_responsebilities}) %>% 
  reduce(inner_join, by="eid")
names(full_women_data) = c('eid', names(biobank_without_age_without_normalization)[!names(biobank_without_age_without_normalization) %in% pure_types_residual_model_table$feature])

men_data_for_sorted_correlation_plot = rbind(
  full_men_data %>% 
    select(starts_with("Mean FA")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Volume")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Mean MD")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Weighted-mean MD")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na(),
  full_men_data %>% 
    select(starts_with("Weighted-mean FA")) %>% 
    correlate() %>% 
    shave() %>% 
    stretch() %>% 
    drop_na()
)

women_data_for_sorted_correlation_plot = rbind(full_women_data %>% 
                                                 select(starts_with("Mean FA")) %>% 
                                                 correlate() %>% 
                                                 shave() %>% 
                                                 stretch() %>% 
                                                 drop_na(),
                                               full_women_data %>% 
                                                 select(starts_with("Volume")) %>% 
                                                 correlate() %>% 
                                                 shave() %>% 
                                                 stretch() %>% 
                                                 drop_na(),
                                               full_women_data %>% 
                                                 select(starts_with("Mean MD")) %>% 
                                                 correlate() %>% 
                                                 shave() %>% 
                                                 stretch() %>% 
                                                 drop_na(),
                                               full_women_data %>% 
                                                 select(starts_with("Weighted-mean MD")) %>% 
                                                 correlate() %>% 
                                                 shave() %>% 
                                                 stretch() %>% 
                                                 drop_na(),
                                               full_women_data %>% 
                                                 select(starts_with("Weighted-mean FA")) %>% 
                                                 correlate() %>% 
                                                 shave() %>% 
                                                 stretch() %>% 
                                                 drop_na()
)

sorted_correlation_data <- 
  rbind(
    women_data_for_sorted_correlation_plot %>% 
      select(r) %>% 
      arrange(r) %>% 
      mutate(sex='Women',
             index = 1:nrow(women_data_for_sorted_correlation_plot)),
    men_data_for_sorted_correlation_plot %>% 
      select(r) %>% 
      arrange(r) %>% 
      mutate(sex='Men',
             index = 1:nrow(women_data_for_sorted_correlation_plot))
  )

sorted_correlation_data %>% 
  ggplot(aes(x=index, y=r, colour=sex)) + 
  geom_line(size = 1.5) + 
  ylab("Correlation Coefficient") + 
  xlab("Index") + 
  scale_colour_manual(values = c('tomato','dodgerblue')) + 
  theme(text = element_text(face="bold", color="black", 
                            size=25),
        panel.border = element_blank()) +
  theme(legend.position="none", aspect.ratio = 1)

# Paper data verification -------------------------------------------------


sapply(biobank_without_age_, function(x){x$hypothesis_results$mixture_model$m_parameters$sigma_2_men / 
    x$hypothesis_results$mixture_model$m_parameters$sigma_2_women}) %>% 
  as.data.frame() %>% 
  rename(ratio = "." ) %>% 
  filter(ratio == 5)

feature_with_high_variance_ratio = "Mean MD in superior fronto-occipital fasciculus on FA skeleton (left)" 

feature_with_high_variance_ratio_right = plotGenderHistogram(
  data_for_analysis[[feature_with_high_variance_ratio]]$feature_residuals,
                    data_for_analysis[[feature_with_high_variance_ratio]],
                    feature_with_high_variance_ratio)
ggsave("feature_with_high_variance_ratio_right.png", feature_with_high_variance_ratio_right)

#### Number of outliers

sapply(biobank_without_age_, function(x){x$feature_residuals$outlier_flag %>% sum()}) %>% 
  summary()
  

# Logistic regression -----------------------------------------------------

data_for_glm_list <- lapply(biobank_without_age_, function(x){x$feature_residuals %>% 
    select(eid, sex, log_y)})

data_for_glm <- data_for_glm_list %>% reduce(left_join, by = c("eid","sex"))

set.seed(6431)
trainIndex <- createDataPartition(data_for_glm$sex, 
                                  p = .75,
                                  list = FALSE,
                                  times = 1)

Train <- data_for_glm[ trainIndex,]
Test  <- data_for_glm[-trainIndex,]

logistic_regression_model = glm(sex ~ ., data=Train %>% select(-eid), family = "binomial")
Test$model_prob <- predict(logistic_regression_model, Test, type = "response")
Test <- Test  %>% mutate(model_pred = 1*(model_prob > .5) + 0,
                         visit_binary = 1*(sex == 1) + 0)
Test <- Test %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(Test$accurate, na.rm = T)/nrow(Test)


# Kolmogorov Smirnov analysis ---------------------------------------------
Fn <- ecdf((data_for_analysis[[1]]$feature_residuals%>% 
             filter(sex == 0) %>% 
             select(residuals))$residuals)
plot(Fn)

mixture_cdf <- compute_cdf_for_ks_analysis(
  data_for_analysis[[1]]$feature_residuals %>% 
                              filter(sex == 0) %>% 
                              select(residuals),
                            data_for_analysis[[1]]$hypothesis_results$mixture_model$m_parameters)
lines(sort(mixture_cdf$range), sort(mixture_cdf$cdf))
plot(sort(mixture_cdf$range), sort(mixture_cdf$cdf))


# New KS analysis ---------------------------------------------------------

# KS distance between mixture model and empirical
ks_distribution = sapply(seq(1/289,1,1/289), function(x) {cont_ks_c_cdf(x, 289)})
ks_range = seq(0,15,length.out = 289)

create_ks_plot(ks_results, 0.75, 0.75, "outside")
# KS distance between pure-type model and empirical

# By sex, by p and q
