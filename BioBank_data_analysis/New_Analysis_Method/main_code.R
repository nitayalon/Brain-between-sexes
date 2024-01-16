#' Applying the new analysis method over brain features 

library(dplyr)
library(ggplot2)
library(Jmisc)
library(pbapply)
library(config)
library(DescTools)
library(effsize)
library(mixtools)

source("~process_biobank_data.R")
source("~/Human_brain_research/Mixture_models/source_script.R")
sourceAll("~/Human_brain_research/Mixture_models/Log_normal_mixture_model/Grid_Search/llk_computation/")
sourceAll("~/Human_brain_research/BioBank_data_analysis/New_Analysis_Method/Residuals_analysis_methods/")
sourceAll("~/Human_brain_research/Mixture_models/Log_normal_mixture_model/Grid_Search/llk_computation/")
sourceAll("~/Human_brain_research/BioBank_data_analysis/Hypothesis_testing_methods/")

total_brain_data$eid <- total_data$id
total_brain_data$age = total_data$age_when_attanded_biobank_scan
user_data <- total_data %>% select(eid, sex)
features_for_analysis_names <- names(features_for_paper)
biobank_full_analysis_list <- list()
relevant_data = features_for_paper

## Correlation analysis between log(y) = log(volume)+age+age*sex and log(y) = log(volume)+age 

linear_model_without_age_without_sex <- pblapply(features_for_analysis_names,function(x){
  applyLinearModelOverBrainFeature(x,
                                   user_data,
                                   total_brain_data,
                                   F,F)
})
names(linear_model_without_age_without_sex) <- features_for_analysis_names

linear_model_with_age_without_sex <- pblapply(features_for_analysis_names,function(x){
  applyLinearModelOverBrainFeature(x,
                                   user_data,
                                   total_brain_data,
                                   T,F)
})
names(linear_model_with_age_without_sex) <- features_for_analysis_names

linear_model_with_age_with_sex <- pblapply(features_for_analysis_names,function(x){
  applyLinearModelOverBrainFeature(x,
                                   user_data,
                                   total_brain_data,
                                   T,T)
})
names(linear_model_with_age_with_sex) <- features_for_analysis_names


correlation_between_model_1_and_model_2 <- pblapply(features_for_analysis_names,function(x){
  cor(linear_model_without_age_without_sex[[x]]$residuals,
      linear_model_with_age_without_sex[[x]]$residuals)
})

summary(unlist(correlation_between_model_1_and_model_2))
mean(unlist(correlation_between_model_1_and_model_2))
median(unlist(correlation_between_model_1_and_model_2))
hist(unlist(correlation_between_model_1_and_model_2))

correlation_between_model_1_and_model_3 <- pblapply(features_for_analysis_names,function(x){
  cor(linear_model_without_age_without_sex[[x]]$residuals,
      linear_model_with_age_with_sex[[x]]$residuals)
})

summary(unlist(correlation_between_model_1_and_model_3))
mean(unlist(correlation_between_model_1_and_model_3))
median(unlist(correlation_between_model_1_and_model_3))
hist(unlist(correlation_between_model_1_and_model_3))

## ANOVA per feature in DS1 
## ANOVA per feature in DS2
## Compute correlation - F statistic
model_1_anova = lapply(names(linear_model_without_age_without_sex),
                       function(x){anova_test(data=linear_model_without_age_without_sex[[x]],
                                              residuals ~ factor(sex))})
model_2_anova = lapply(names(linear_model_with_age_without_sex),
                       function(x){anova_test(data=linear_model_with_age_without_sex[[x]],
                                              formula = residuals ~ factor(sex))})
model_1_anova_p_value = sapply(model_1_anova, function(x){x$F})
model_2_anova_p_value = sapply(model_2_anova, function(x){x$F})
cor(model_1_anova_p_value, model_2_anova_p_value)

# ANOVA of data set 1 and ANCOVA 1 - age is cofactor
model_1_ancova = lapply(names(linear_model_without_age_without_sex),
                       function(x){anova_test(data=linear_model_without_age_without_sex[[x]],
                                              residuals ~ sex * age, covariate="age")})
model_1_ancova_p_value = sapply(model_1_ancova, function(x){x$F[x$Effect=="sex"]})
cor(model_1_anova_p_value, model_1_ancova_p_value)
## Compute correlation - F statistic

## Applying full analysis cycle over the data

biobank_with_age_residuals <- 
  pblapply(features_for_analysis_names,function(x){
  fullBrainFeatureAnalysis(x,
                           features_for_analysis_names,
                           user_data,
                           total_brain_data,
                           TRUE,
                           TRUE,
                           TRUE)
})
names(biobank_with_age_analysis) <- features_for_analysis_names

biobank_without_age_ <- pblapply(features_for_analysis_names,function(x){
  fullBrainFeatureAnalysis(x,
                           features_for_analysis_names,
                           user_data,
                           total_brain_data)
})

names(biobank_without_age_) <- features_for_analysis_names
save(biobank_without_age_, file = "BioBank_data_full_analysis_no_age_30_09_2022.RData")


biobank_without_age_without_normalization <- pblapply(features_for_analysis_names,function(x){
  fullBrainFeatureAnalysis(x,
                           features_for_analysis_names,
                           user_data,
                           total_brain_data,
                           FALSE,
                           FALSE)
})

names(biobank_without_age_without_normalization) <- features_for_analysis_names
save(biobank_without_age_without_normalization, file = "BioBank_data_full_analysis_no_age_no_normalization_06_10_2022.RData")


save(biobank_with_age_analysis, file = "BioBank_data_full_analysis_with_age_13_04_2022.RData")


## Odd feature analysis
odd_feature_analysis <- pblapply("Mean MD in cingulum hippocampus on FA skeleton (right)"
                                       ,function(x){fullBrainFeatureAnalysis(x,
                           features_for_analysis_names,
                           user_data,
                           total_brain_data)
})


plotGenderHistogram(odd_feature_analysis[[1]]$feature_residuals,
                    odd_feature_analysis[[1]],
                    "Mean MD in cingulum hippocampus on FA skeleton (right)")
load("Human_brain_research/new_model_with_age/BioBank_data_full_analysis_no_age_13_04_2022.RData")

### Adding original analysis for comparison
biobank_without_age_wrong_log_volume_analysis <- 
  pblapply(features_for_analysis_names,function(x){
    fullBrainFeatureAnalysis(x,
                             features_for_analysis_names,
                             user_data,
                             total_brain_data,
                             FALSE)
  })
names(biobank_without_age_wrong_log_volume_analysis) <- features_for_analysis_names
save(biobank_without_age_wrong_log_volume_analysis, file = "biobank_without_age_wrong_log_volume_analysis.RData")


# Debugging variance ratio ------------------------------------------------

debug_results <- fullBrainFeatureAnalysis("Mean MD in superior fronto-occipital fasciculus on FA skeleton (left)",
                         features_for_analysis_names,
                         user_data,
                         total_brain_data,
                         FALSE,
                         FALSE)
debug_results$hypothesis_results$mixture_model$m_parameters$sigma_2_men / debug_results$hypothesis_results$mixture_model$m_parameters$sigma_2_women
biobank_without_age_[["Mean MD in superior fronto-occipital fasciculus on FA skeleton (left)"]]$hypothesis_results$mixture_model$m_parameters$sigma_2_men / 
  biobank_without_age_[["Mean MD in superior fronto-occipital fasciculus on FA skeleton (left)"]]$hypothesis_results$mixture_model$m_parameters$sigma_2_women

