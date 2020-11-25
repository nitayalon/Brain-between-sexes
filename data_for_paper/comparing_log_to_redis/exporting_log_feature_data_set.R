library(dplyr)
library(ggplot2)
library(tidyverse)

### 22/11/2020 update - fixing the log(feature) data set
bio_bank_log_feature_data = 
  tibble(eid = bio_bank_data$`Encoded anonymised participant ID`, 
         sex = bio_bank_data$Sex, 
         log(bio_bank_data[,names(biobank_residuals_data)]))
# write_csv(bio_bank_log_feature_data, 'bio_bank_log_feature_data.csv')
mask_outliers = function(feature, sex, eid)
{
  no_na_data = feature[!is.na(feature)]
  sex = sex[!is.na(feature)]
  eid = eid[!is.na(feature)]
  
  sex = sex[no_na_data > -Inf]
  eid = eid[no_na_data > -Inf]
  no_na_data = no_na_data[no_na_data > -Inf]
  
  m = length(no_na_data)
  avg = mean(no_na_data[-c(1:100, (m-100):m)],na.rm = T)
  std = sd(no_na_data[-c(1:100, (m-100):m)], na.rm = T)
  below = sum(no_na_data < avg - 5 * std)
  above = sum(no_na_data > avg + 5 * std)
  within = no_na_data > avg - 5 * std & no_na_data < avg + 5 * std
  value = (no_na_data[within] - mean(no_na_data[within])) / sd(no_na_data[within])
  return(list(below = below,
              above = above,
              data_for_em = tibble(eid = eid[within],
                                  sex = sex[within],
                                  value = value)))
}

log_feature_data_for_em = lapply(bio_bank_log_feature_data[,-c(1,2)], function(x){mask_outliers(x, bio_bank_log_feature_data$sex, bio_bank_log_feature_data$eid)})
outliers_to_the_left = sapply(log_feature_data_for_em, function(x){x$below})
outliers_to_the_right = sapply(log_feature_data_for_em, function(x){x$above})
summary(c(outliers_to_the_left,outliers_to_the_right))
sort(outliers_to_the_left)
summary(outliers_to_the_right)
sort(outliers_to_the_right)
