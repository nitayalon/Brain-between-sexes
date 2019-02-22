#' Applying the new analysis method over brain features 

library(dplyr)
library(ggplot2)
library(Jmisc)

source("~/Documents/Human_brain_research/DAPHNA_JOEL/Mixture_models/source_script.R")
total.brain.volum.data <- bio.bank.data %>% select("X25010.2.0","X25004.2.0")
list.of.names <- names(full_relevant_data)[-c(1:5)]
results.list <- list()

for(feature.name in list.of.names)
{
  full.function.test <- fullBrainFeatureAnalysis(feature.name,list.of.names)
  results.list[[feature.name]] <- full.function.test
}  


analyzeFullBrainFeature(full.function.test,plot = T)
