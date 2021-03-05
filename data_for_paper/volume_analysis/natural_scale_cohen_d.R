library(tidyverse)
library(effsize)
total_brain_volume_df = tibble(eid = bio_bank_data$`Encoded anonymised participant ID`,
                               sex = bio_bank_data$Sex,
                               value = rowSums(total.brain.volume.data))
total_brain_volume_df = total_brain_volume_df %>% 
  drop_na() 
ggplot(total_brain_volume_df, aes(x = value, fill = factor(sex))) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 5000)
cohen.d(total_brain_volume_df$value[total_brain_volume_df$sex == 0],
                        total_brain_volume_df$value[total_brain_volume_df$sex == 1])