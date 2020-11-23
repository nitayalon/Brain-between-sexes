library(dplyr)
library(tidyverse)
library(ggplot2)
library(effsize)

total_brain_volume_df = tibble(eid = bio_bank_data$`Encoded anonymised participant ID`,
                               sex = bio_bank_data$Sex,
                               value = rowSums(total.brain.volume.data))
total_brain_volume_df = total_brain_volume_df %>% 
  drop_na() %>% 
  filter(value > 0) %>% 
  mutate(log_volume = log(value)) %>% 
  mutate(value = (log_volume - mean(log_volume)) / sd(log_volume))

total_brain_volume_df %>% 
  group_by(sex) %>% 
  summarise(avg = mean(value), std = sd(value), lq = quantile(value, c(0.01)), hq = quantile(value, c(0.99)))

log_volume_em_results = applyHypothesisOverResiduals(total_brain_volume_df, F)
plotGenderHistogram(total_brain_volume_df, 
                    log_volume_em_results, 
                    two_mixtures = F,
                    single_gaussian = T)
