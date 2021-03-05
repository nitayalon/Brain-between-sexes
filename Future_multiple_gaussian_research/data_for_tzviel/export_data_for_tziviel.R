library(dplyr)
library(purrr)
str(bio_bank_data)
table(bio_bank_data$Sex)
length(bio_bank_data %>% names() %>% grep('Volume',.))
bio_bank_data %>% names() %>% grep('Mean MD',.)
bio_bank_data %>% names() %>% grep('Mean FA',.)
# log feature ~ log volume
str(biobank_residuals_data)
biobank_residuals_data_df = 
  Reduce(function(x, y) merge(x, y, by = c("eid","sex"), all = TRUE), biobank_residuals_data)
names(biobank_residuals_data_df) = c('eid','sex',names(biobank_residuals_data))
write.csv(x = biobank_residuals_data_df, file = 'residual_data_bio_bank.csv')
# log feature
str(biobank_standardized_data)
biobank_standardized_data_df = 
  Reduce(function(x, y) merge(x, y, by = c("eid","sex"), all = TRUE), biobank_standardized_data)
names(biobank_standardized_data_df) = c('eid','sex',names(biobank_standardized_data))
write.csv(x = biobank_standardized_data_df, file = 'standardized_data_bio_bank.csv')
write.csv(x = bio_bank_data, file = 'bio_bank_original_data.csv')
