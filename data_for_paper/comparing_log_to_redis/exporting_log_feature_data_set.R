### 22/11/2020 update - fixing the log(feature) data set
bio_bank_log_feature_data = 
  tibble(eid = bio_bank_data$`Encoded anonymised participant ID`, 
         sex = bio_bank_data$Sex, 
         log(bio_bank_data[,names(biobank_residuals_data)]))
scale2 <- function(x, na.rm = T) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

bio_bank_log_feature_data %>% 
  mutate_at(names(biobank_residuals_data),scale2)

below <- function(x, na.rm = T) x < (mean(x, na.rm = na.rm) - 4.5 * sd(x, na.rm))
above <- function(x, na.rm = T) x > (mean(x, na.rm = na.rm) + 4.5 * sd(x, na.rm))

low_outliers = colSums(sapply(bio_bank_log_feature_data %>% 
  select(names(biobank_residuals_data)),below), na.rm = T)
above_outliers = colSums(sapply(bio_bank_log_feature_data %>% 
  select(names(biobank_residuals_data)),above), na.rm = T)
sort(low_outliers)
sort(above_outliers)

mean(bio_bank_log_feature_data$`Mean FA in fornix on FA skeleton`, na.rm = T)
sd(bio_bank_log_feature_data$`Mean FA in fornix on FA skeleton`, na.rm = T)
sort(bio_bank_log_feature_data$`Mean FA in fornix on FA skeleton`)

mean(bio_bank_log_feature_data$`Mean MD in superior fronto-occipital fasciculus on FA skeleton (right)`, na.rm = T)
sd(bio_bank_log_feature_data$`Mean MD in superior fronto-occipital fasciculus on FA skeleton (right)`, na.rm = T)
sort(bio_bank_log_feature_data$`Mean MD in superior fronto-occipital fasciculus on FA skeleton (right)`,decreasing = T)
hist(bio_bank_log_feature_data$`Mean MD in superior fronto-occipital fasciculus on FA skeleton (right)`)