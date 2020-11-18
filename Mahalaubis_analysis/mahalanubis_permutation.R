# install.packages("GenAlgo", repos="http://R-Forge.R-project.org")
library(GenAlgo)
library(tidyverse)
library(dplyr)
library(pbapply)
# Select only volume features
data_for_mahalanubis_permutations = biobank_residuals_data[grep("Volume", names(biobank_residuals_data))]

mahalanubis_candidates = tibble(id = data_for_mahalanubis_permutations[[1]]$eid,
                                  sex = data_for_mahalanubis_permutations[[1]]$sex,
                                  value = data_for_mahalanubis_permutations[[1]]$value)
for (i in 2:length(data_for_mahalanubis_permutations)){
  right_table = tibble(id = data_for_mahalanubis_permutations[[i]]$eid,
                       sex = data_for_mahalanubis_permutations[[i]]$sex,
                       value = data_for_mahalanubis_permutations[[i]]$value)
  mahalanubis_candidates = full_join(mahalanubis_candidates, right_table,  by = c("id","sex"))
}

mahalanubis_candidates_no_na_men = drop_na(mahalanubis_candidates) %>% 
  filter(sex == 1)
mahalanubis_candidates_no_na_women = drop_na(mahalanubis_candidates) %>% 
  filter(sex == 0) %>% 
  sample_n(size = 8016)
data_for_mahalanubis_permutations = rbind(mahalanubis_candidates_no_na_men, mahalanubis_candidates_no_na_women)
data_for_mahalanubis_permutations$sex = as.factor(data_for_mahalanubis_permutations$sex)
original_mahalanubis_dist = maha(data_for_mahalanubis_permutations[,-c(1,2)], groups = data_for_mahalanubis_permutations$sex)

permute_data = function(seed, data_for_mahalanubis_permutations)
{
  set.seed(seed)
  group_a_ind = sample(1:nrow(data_for_mahalanubis_permutations),8016)
  ind_vector = factor(1:nrow(data_for_mahalanubis_permutations) %in% group_a_ind)
  return(maha(data_for_mahalanubis_permutations[,-c(1,2)], groups = ind_vector))
}

mahala_dist = sapply(1:1000, function(i){permute_data(i,data_for_mahalanubis_permutations)})
