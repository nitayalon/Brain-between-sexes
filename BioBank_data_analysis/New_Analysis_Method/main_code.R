#' Applying the new analysis method over brain features 

library(dplyr)
library(ggplot2)
library(Jmisc)
library(pbapply)

source("~/Human_brain_research/Mixture_models/source_script.R")
sourceAll("~/Human_brain_research/Mixture_models/Log_normal_mixture_model/Grid_Search/llk_computation/")
sourceAll("~/Human_brain_research/BioBank_data_analysis/New_Analysis_Method/Residuals_analysis_methods/")
sourceAll("~/Human_brain_research/Mixture_models/Log_normal_mixture_model/Grid_Search/llk_computation/")
sourceAll("~/Human_brain_research/BioBank_data_analysis/Hypothesis_testing_methods/")

total.brain.volum.data <- bio.bank.data %>% select("X25010.2.0","X25004.2.0")
user_data <- select(bio.bank.data, eid, X31.0.0)
list.of.names <- names(full_relevant_data)[-c(1:5)]
biobank_full_analysis_list <- list()

biobank_full_analysis_list <- pblapply(list.of.names,function(x){
  fullBrainFeatureAnalysis(x,
                           list.of.names,
                           user_data)
})
names(biobank_full_analysis_list) <- list.of.names
for(feature.name in list.of.names)
{
  full.function.test <- fullBrainFeatureAnalysis(feature.name,
                                                 list.of.names,
                                                 user_data)
  biobank_full_analysis_list[[feature.name]] <- full.function.test
}  
save(biobank_full_analysis_list, file = "BioBank_data_analysis_total.RData")

analyzeFullBrainFeature(full.function.test,plot = T)


results.list[[feature.name]]$hypothesis_results$pure_type_vs_mixed_gender_em_results$null_hypothesis
