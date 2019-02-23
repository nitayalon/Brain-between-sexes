#' Applying the new analysis method over brain features 

library(dplyr)
library(ggplot2)
library(Jmisc)

source("~/Documents/Human_brain_research/DAPHNA_JOEL/Mixture_models/source_script.R")
sourceAll("~/Documents/Human_brain_research/DAPHNA_JOEL/Mixture_models/Log_normal_mixture_model/Grid_Search/llk_computation/")
sourceAll("~/Documents/Human_brain_research/DAPHNA_JOEL/BioBank_data_analysis/New_Analysis_Method/Residuals_analysis_methods/")
sourceAll("~/Documents/Human_brain_research/DAPHNA_JOEL/BioBank_data_analysis/New_Analysis_Method/Residuals_analysis_methods/")

total.brain.volum.data <- bio.bank.data %>% select("X25010.2.0","X25004.2.0")
list.of.names <- names(full_relevant_data)[-c(1:5)]
results.list <- list()

for(feature.name in list.of.names)
{
  full.function.test <- fullBrainFeatureAnalysis(feature.name,list.of.names)
  results.list[[feature.name]] <- full.function.test
}  
length(list.of.names) == length(results.list)

analyzeFullBrainFeature(full.function.test,plot = T)
