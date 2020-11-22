library(dplyr)
library(ggplot2)
library(tidyverse)

compute_number_of_outliers = function(f, bound = 4)
{
  f_sd = sd(f$value)
  f_mean = mean(f$value)
  # counter = f$value > (f_mean + bound * f_sd) | f$value < (f_mean - bound * f_sd)
  counter = f$value < (f_mean - bound * f_sd)
  return(sum(counter))
}

standardize_feature <- function(f)
{
  f$value = (f$value - mean(f$value)) / sd(f$value)
  return(f)
}

# names(biobank_standardized_data) == names(biobank_residuals_data)
# hist(biobank_standardized_data[[1]]$value)
log_volume_outliers = sapply(biobank_standardized_data, function(x){compute_number_of_outliers(x, 4.5)})
residuals_outliers = sapply(biobank_residuals_data, function(x){compute_number_of_outliers(x, 4.5)})
standardize_log_volume = lapply(biobank_standardized_data, function(x){standardize_feature(x)})
standardize_log_volume_outliers = sapply(standardize_log_volume, function(x){compute_number_of_outliers(x)})

summary(standardize_log_volume_outliers)
summary(log_volume_outliers)
sort(log_volume_outliers)
summary(residuals_outliers)
sort(residuals_outliers)

which.max(log_volume_outliers[67])
mean(bio_bank_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`, na.rm = T)
sd(bio_bank_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`, na.rm = T)
sort(bio_bank_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`)
hist(bio_bank_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`)

qqq = bio_bank_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`[bio_bank_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)` > 0]
mean(log(qqq), na.rm = T)
sd(log(qqq), na.rm = T)
sort(log(qqq))[20]
hist(log(qqq))

compute_number_of_outliers(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`)

summary(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
sd(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
hist(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
length(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)

hist(biobank_residuals_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)
hist(biobank_standardized_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`$value)

hist((total.brain.volume.data %>% 
  mutate(total = log(`Volume of brain, grey+white matter` + `Volume of ventricular cerebrospinal fluid`)) %>% 
  select(total) %>% 
  drop_na())[,1], freq = F)

total.brain.volume.data %>% 
    mutate(total = log(`Volume of brain, grey+white matter` + `Volume of ventricular cerebrospinal fluid`)) %>% 
    select(total) %>% 
    drop_na() %>% 
  mutate(z_score = (total - mean(total)) / sd(total)) %>% 
  select(z_score) %>% 
  arrange(z_score) %>% 
  mutate(diff = z_score - lag(z_score)) %>% 
  select(diff)
# Compare log-feture to residual
inner_join(standardize_log_volume$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`,
           biobank_residuals_data$`Mean MD in posterior limb of internal capsule on FA skeleton (right)`,by = c('eid','eid')) %>% 
  select(value.x, value.y) %>% 
  plot()